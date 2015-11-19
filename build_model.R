setwd("~/Film Performance/")  # set my working directory
graphics.off() # close charts
rm(list = ls(all = TRUE))

source('~/Film Performance/assist_functions.R')#loads additional functions

####################################
#SCRAPE WEBPAGES TO BUILD DATATABLE#
####################################

#These can take a while to run as I have to make thousands of queries against webpages
#I parallized them where possible as there are no dependencies

#get list of weekends for HSX data
date_list <- as.Date((Sys.Date()-(365*12)):Sys.Date(),origin = '1970-01-01')
weekends <- date_list[which(format(date_list,'%A')=='Friday')]

library(snow)#parallelizing package
cl <- makePSOCKcluster(4,outfile="")#initialized 4 parallel instances
movie_weekends <- parLapply(cl, weekends, grab_movie_dat) #parallelized grab movie funcition to build initial list of dataframes 
stopCluster(cl)
gc()

movie_weekends <- assemble_movie_weekends(movie_weekends)

library(data.table)
this_week <- as.data.table(grab_fantasy_dat()) #assembles this week's fantasy films and prices
opening_this_week <- as.data.table(grab_new_opening_dat()) #finds films opening this week with tickers from HSX


#assemble a table of this week's films for estimation and adds them to the movie_weekends datatable
movie_weekends <- add_current_films(movie_weekends,opening_this_week)

tickers <- unique(movie_weekends[,Symbol])#generates the ticker list needed for additional HSX data

#builds datatable of expected opening weekends from ticker list
cl <- makePSOCKcluster(c("localhost","localhost","localhost","localhost"),outfile="")
EOWs <- parLapply(cl,tickers,grab_opening_dat)
stopCluster(cl)
EOWs <- rbindlist(EOWs)
gc()

merged_movies <- merge(movie_weekends,EOWs,by='Symbol',all.x=TRUE)

#generate genere and rating datatable
cl <- makePSOCKcluster(c("localhost","localhost","localhost","localhost"),outfile="")
other_hsx_dat <- parLapply(cl,tickers,grab_hsx_dat)
stopCluster(cl)
gc()

#add genre and rating data to datatable
merged_movies <- add_rating_and_genre(other_hsx_dat,merged_movies)

####load('C:/Users/Aaron Foss/Documents/Classes/Udacity/HW4/merged_movies.Rdata')
merged_movies <- merged_movies[MPAARating%in%c('G','PG','PG13','R'),]

movie_names <- merged_movies[,list('Date' = min(Date)),by=Name]#generate unique list of movie names for generating ratings table

#build datatable of movie ratings
cl <- makePSOCKcluster(c("localhost","localhost","localhost","localhost"),outfile="")
movie_ratings <- parLapply(cl,c(1:dim(movie_names)[1]),grab_rating_dat,movie_names)
stopCluster(cl)

#add movie fan and critic ratings to datatable
movies_rated <- add_fan_critic_ratings(merged_movies)

#order movies_rated datatable by ticker and date so that some timeseries features can be built
setkey(movies_rated,Symbol,Date)

###########################
#BUILD ADDITIONAL FEATURES#
###########################

movies_rated <- build_features(movies_rated)

library(psych)#needed for winsor function which does a type of outlier handling
library(mboost)#needed for boosted models
setkey(movies_rated,Date)

##############################
#ASSEMBLE ACTUAL TRAINING SET#
##############################

#Build datatable of weeks 2-10 with features for model; tgt (LHS) variable is winsorized to remove outlier impact
#I only do weeks 2-10 as I thought opening weeks would be best predicted by the HSX market - 
#I'm not certain that is true, but have no alternate as of now
dat <- (movies_rated[!is.na(EstOW)& Week < 10 & Week > 1 ,list(Name,'tgt' = winsor(WeekGross,trim = 0.05),'Week' = as.numeric(Week), 'Week2' = as.numeric(Week)^2, 'Rating' = as.factor(MPAARating),'Month' = as.factor((substr(Date,6,7))),'Genre' = as.factor(Genre),EstOW,OWres,RelOWres,Critic,Audience,AvgRev,RevCt,RelCt,RelCritic,lagRelPerf,RelAudience,lagWeekGross,chgWeekGross,pctChgWeekGross,RelChgWeekGross,RelPctChgWeekGross,lagGRelPerf,gRelAudience,gRelCritic,gRelCt,Date)])

dt <- '2015-11-13'#specifies upcoming friday
list_of_film_dates <- dat[,list(Name,Date,Week)]

train <- which(list_of_film_dates[,Date]<dt)#build training dataset

scaled <- scale(dat[,!c('Name','Date','Rating','Genre','Month'),with=F])#scale data to enhance fit and interpretability
dat <- data.table(scaled,dat[,c('Rating','Genre','Month'),with=F])#add back 'factor' variables (will be converted to dummies when modeling)
dat2 <- na.omit(dat[train,])
dat_mat <- model.matrix(~.,dat2)[,-1]

library(rrcovHD)

pc1 <- PcaLocantore(dat_mat)

##########
#MODELING#
##########

#build model using boosted trees - this is an additive model that chooses a split in each of its 2500 iterations
m1 <- gamboost(tgt~.,baselearner = 'btree', control = boost_control(mstop = 2500,trace=TRUE),family = Gaussian(),data = as.data.frame(dat2[!pc1@flag]))
summary(m1)#shows selected factors and rough importance

library(randomForest)
m3 <- randomForest(tgt~.,data = dat2[!pc1@flag], ntree=500,verbose = TRUE)



#sanity check with simple linear model on predictions - B1 should be approx 1
#can do some error analysis which shows it is not a perfect model,
#but even if t-stats on the single factor can't be trusted, it has a high R2
#clearly it is slightly non-linear and I have a small (these are dollars) negative B0 when coerced to a linear model
#still, good enough for government work
LHS <- (round(((dat2[,tgt])*attributes(scaled)[[4]][1]+attributes(scaled)[[3]][1]),6)) #rounded scaled actuals
RHS <- (as.numeric(predict(m1,newdata = as.data.frame(dat2[c(1:dim(dat2)[1]),]))*attributes(scaled)[[4]][1]+attributes(scaled)[[3]][1])) #rescaled predictions    
RHS2 <- (as.numeric(predict(m3,newdata = as.data.frame(dat2[c(1:dim(dat2)[1])]))*attributes(scaled)[[4]][1]+attributes(scaled)[[3]][1])) #rescaled predictions    

m2 <- lm(LHS~rowMeans(cbind(RHS2)))
summary(m2)
#plot(m2)

##############
#OPTIMIZATION#
##############

#builds table of estimates for week 2-10 films
tab <- NULL
est_wk_box <- (predict(m3,newdata = as.data.frame(dat)[-train,])*attributes(scaled)[[4]][1]+attributes(scaled)[[3]][1])

tab <- data.table(list_of_film_dates[-train,!'Date',with = F],'est_wk_box' = as.numeric(est_wk_box))

#removes opening week films from movies_rated and builds estimates off of HSX price
rem <- movies_rated[!is.na(EstOW)&EstOW!=0& Week == 1 & Date == dt,]
rem[,est_wk_box:=EstOW*1000000]
#adds week one films back to week 2-10 estimates
tab <- rbind(tab,rem[,names(tab),with=F])

#finds movies in limited release going wide (only seen w/ Steve Jobs)
narrow <- names(which(table(tab[[1]])>1))

wide_this_week <- tab[Name %in% narrow,]
setkey(wide_this_week,Name,est_wk_box)
wide_this_week[,rank:=order(est_wk_box),by=Name]
wide_this_week[,max_rank:=max(rank),by=Name]
wide_this_week <- wide_this_week[rank == max_rank, list(Name,Week,est_wk_box)]
tab <- tab[!Name %in% narrow,]
tab <- rbind(tab,wide_this_week)

#repair paranormal activity for elipses in name
tab[Name %like% 'paranormal_activity_the_ghost',Name:='paranormal_activity_the_ghost_dimension_2015']

tab #show this week's films and estimated weekend boxoffice
tab[,Name:=substr(Name,1,nchar(Name)-5),by = Name]

set.seed(321)
vals <- NULL



vals <- lapply(c(1:10000),samp_res)

vals <- do.call('rbind',vals)


sols <- lapply(c(1:10000),single_solution)
smp <- do.call('rbind',sols)
smp <- data.table('nm' = smp,'V1' = 1)
smp[,len:=sum(V1),by=nm.V1]
setkey(smp,len)
unique(smp)