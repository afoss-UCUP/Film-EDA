#functions to scrape and parse webpage data, assemble datatables/feature sets and run optimizer



#grabs movie data from HSX
grab_movie_dat <- function(day){
  library(rvest)
  library(XML)
  #unload Hmisc package if loaded due to issues
  if(sum(search()=='package:Hmisc')>0){
    detach("package:Hmisc", unload=TRUE)	
  }
  
  test <- try(read_html(paste("http://www.hsx.com/security/feature.php?type=boxoffice&wdate=",as.character(day),sep=""))%>%html_table(fill=TRUE),silent = TRUE)#grabs weekend movie stats
  tab <- NULL
  #assembles and formats hsx movie dataframe columns
  if(class(test)[1]!='try-error'){
    
    tab <-  test
    tab <- test[[1]]
    names(tab) <- gsub("[[:punct:]]|Â| ","",names(tab))
    tab2 <- data.frame('Name' = rep(NA,dim(tab)[1]),'Symbol' = rep(NA,dim(tab)[1]), 'WeekGross' = rep(NA,dim(tab)[1]), 'TotalGross' = rep(NA,dim(tab)[1]), 'Week' = rep(NA,dim(tab)[1]))
    tab2[,'WeekGross'] <- as.numeric(try(gsub("[[:punct:]]|Â| | ","",tab[,'WeekGross']),silent = TRUE))
    tab2[,'TotalGross'] <- as.numeric(try(gsub("[[:punct:]]|Â| | ","",tab[,'TotalGross']),silent = TRUE))
    tab2[,'Name'] <- try(tab[,'Name'],silent = TRUE)
    tab2[,'Symbol'] <- try(tab[,'Symbol'],silent = TRUE)
    tab2[,'Week'] <- try(as.character(tab[,'Week']),silent = TRUE)
    tab2[,'Date'] <- try(day,silent = TRUE)
  }
  
  
  return(tab2)
}

#builds movie_weekends datatable
assemble_movie_weekends <- function(movie_weekends){
  library(data.table)
  
  movie_weekends <- rbindlist(movie_weekends)#coerces list of hsx dataframes into one large datatable
  movie_weekends[,Symbol:=as.character(Symbol)]
  movie_weekends[,Name:=gsub('[[:punct:]]','',Name)]#regex repair of certain names
  movie_weekends[,Name:=gsub(' ','_',Name)]#regex repair of certain names
  movie_weekends[,Name:=tolower(Name)]#coerce names to lowercase
  movie_weekends <- movie_weekends[!is.na(WeekGross),]#remove rows with N/A amounts
  movie_weekends <- movie_weekends[!Symbol%like%'N/A',]#remove rows with N/A amounts
  
  return(movie_weekends)
}

#grabs opening data
grab_new_opening_dat <- function(){
  library(rvest)
  library(XML)
  library(data.table)
  #unload Hmisc package if loaded due to issues
  if(sum(search()=='package:Hmisc')>0){
    detach("package:Hmisc", unload=TRUE)	
  }
  
  tab <- NULL
  
  
  test <- try(read_html(paste("http://www.hsx.com/security/feature.php?type=opening"))%>%html_table(fill=TRUE),silent = TRUE)
  if(class(test)[1]!='try-error'){
    tab <-  test[[1]]
    tab[,2] <- tolower(gsub('[[:punct:]]',"_",tab[,2]))
    tab[,2] <- tolower(gsub('[[:blank:]]',"_",tab[,2]))
    tab[,2] <- tolower(gsub('__',"_",tab[,2]))
    tab <- tab[,c('Name','Symbol')]	
  }
  return(tab)
}

#function to assemble this week's list of films
add_current_films <- function(movie_weekends,opening_this_week){
  
  
  
  prior <- copy(movie_weekends[Date == max(movie_weekends[,Date]),])
  prior[,WeekGross:=NA]
  prior[,TotalGross:=NA]
  prior[Week!='',Week:=as.character(as.numeric(Week)+1)]
  prior[!is.na(Week),Date:=Date+7]
  
  opening_this_week[,WeekGross:=NA]
  opening_this_week[,TotalGross:=NA]
  opening_this_week[,Week:=1]
  opening_this_week[,Date:=unique(prior[,Date])]
  movie_weekends <- rbind(movie_weekends,prior,opening_this_week)
  
  return(movie_weekends)
}


#grabs opening data
grab_opening_dat <- function(ticker){
  library(rvest)
  library(XML)
  #unload Hmisc package if loaded due to issues
  if(sum(search()=='package:Hmisc')>0){
    detach("package:Hmisc", unload=TRUE)	
  }
  
  tab <- NULL
  
  test <- try(read_html(as.character(paste("http://www.hsx.com/security/view/",as.character(ticker[1]),'.OW',sep=""))), silent = T)
  if(class(test)[1]!='try-error'){
    tab <-  try(strsplit(test %>% html_node(".value") %>% html_text()," |$")[[1]][1],silent = TRUE)
    tab <- unlist(strsplit(tab,'H'))[2]
    tab <- try(as.numeric(substr((gsub('[[:alpha:]]|$','',tab))[1],2,nchar(tab))),silent = TRUE)
    tab <- data.frame('Symbol' = ticker, 'EstOW' = tab)
  }
  return(tab)
}

#grabs other HSX data
grab_hsx_dat <- function(ticker){
  library(rvest)
  library(XML)
  #unload Hmisc package if loaded due to issues
  if(sum(search()=='package:Hmisc')>0){
    detach("package:Hmisc", unload=TRUE)	
  }
  
  tab <- NULL
  tab2 <- NULL
  tab3 <- NULL
  
  test <- try(read_html(paste("http://www.hsx.com/security/view/",as.character(ticker),sep=""))%>%html_table(fill=TRUE),silent = TRUE)
  if(class(test)[1]!='try-error'){
    tab <-  test
    tab2 <- data.frame(rbind(tab[[1]],tab[[2]]))
    tab3 <- data.frame(rep(NA,dim(tab2)[1]))
    tab3[,1] <- (gsub('[[:punct:]]| | ','',tab2[,'X2']))
    rownames(tab3) <- as.character(gsub('[[:punct:]]| | ','',tab2[,'X1']))
    tab3 <- t(tab3)
    rownames(tab3) <- NULL
    colnames(tab3) <- as.character(gsub('Â','',colnames(tab3)))
  }
  return(as.data.frame(tab3))
}

#grabs fantasy data and prices
grab_fantasy_dat <- function(){
  library(rvest)
  library(XML)
  library(data.table)
  #unload Hmisc package if loaded due to issues
  if(sum(search()=='package:Hmisc')>0){
    detach("package:Hmisc", unload=TRUE)	
  }
  
  tab <- NULL
  
  
  test <- try(read_html(paste("http://fantasymovieleague.com/researchvault?section=bux"))%>%html_table(fill=TRUE), silent = T)
  if(class(test)[1]!='try-error'){
    tab <-  test
    tab <- try(unlist(strsplit(gsub('[[:punct:]]','',as.character(tab[[1]][,2])),'FB')) ,silent = TRUE)
    tab <- data.frame(matrix(tab,ncol = 2,byrow = TRUE))
    tab[,2] <- as.numeric(as.character(tab[,2]))
    tab[,1] <- tolower(gsub('[[:blank:]]',"_",tab[,1]))
    tab[,1] <- tolower(gsub('__',"_",tab[,1]))
    names(tab) <- c('Name','FB')
  }
  return(tab)
}

#add rating and genre data to merged_movies datatable
add_rating_and_genre <- function(other_hsx_dat,merged_movies){
  
  other_hsx_dat2 <- list()
  for(i in 1:length(other_hsx_dat)){
    other_hsx_dat2[[length(other_hsx_dat2)+1]] <- other_hsx_dat[[i]][,c('Symbol','MPAARating','Genre')]
  }
  
  other_hsx_dat2 <- rbindlist(other_hsx_dat2)
  
  gc()
  
  
  merged_movies <- merge(merged_movies,other_hsx_dat2,by='Symbol',all.x=TRUE)
  
  return(merged_movies)
  
}



#grabs fan and critic data
grab_rating_dat <- function(name_vector,movie_names){
  library(rvest)
  library(XML)
  library(data.table)
  #unload Hmisc package if loaded due to issues
  if(sum(search()=='package:Hmisc')>0){
    detach("package:Hmisc", unload=TRUE)	
  }
  
  tab <- NULL
  tab1 <- NULL
  tab2 <- NULL
  tab3 <- NULL
  test <- NULL
  
  test <- try(read_html(paste("http://www.rottentomatoes.com/m/",as.character(movie_names[,Name][name_vector]),'/#contentReviews',sep="")),silent = TRUE)
  tab1 <-  try(as.numeric(test %>% html_node("#tomato_meter_link .superPageFontColor span") %>% html_text()),silent = TRUE)
  if(class(tab1)[1]=='try-error'){
    test <- try(read_html(paste("http://www.rottentomatoes.com/m/",as.character(paste(movie_names[,Name][name_vector],substr(movie_names[,Date][name_vector],1,4),sep='_')),'/#contentReviews',sep=""))[2],silent = TRUE)
  }
  
  
  if(class(test)[1]!='try-error'){
    tab1 <-  try(as.numeric(test %>% html_node("#tomato_meter_link .superPageFontColor span") %>% html_text()),silent = TRUE)
    tab2 <-  try(as.numeric(gsub('%','',test %>% html_node(".meter-value .superPageFontColor") %>% html_text())),silent = TRUE)
    tab3 <-  try(strsplit(test %>% html_node("#scoreStats") %>% html_text(),':'),silent = TRUE)
    avg_critic <- as.numeric(strsplit(tab3[[1]][2],'/')[[1]][1])
    rev_count <- as.numeric(strsplit(tab3[[1]][3],'   ')[[1]][1])
    tab <- data.table('Name' = paste(movie_names[,Name][name_vector],substr(movie_names[,Date][name_vector],1,4),sep='_'), 'Critic' = tab1, 'Audience' = tab2, 'AvgRev' = avg_critic, 'RevCt' = rev_count)
    
  }
  return(tab)
}

#adds fan and critic data to datatable
add_fan_critic_ratings <- function(merged_movies){
  movie_short_ratings <- list()
  for(i in 1:length(movie_ratings)){
    if(is.numeric(c(movie_ratings[[i]][[2]],movie_ratings[[i]][[2]]))==TRUE){
      movie_short_ratings[[length(movie_short_ratings)+1]] <- movie_ratings[[i]]	
    }
  }
  
  movie_short_ratings <- rbindlist(movie_short_ratings)
  movie_short_ratings[,Name2:= Name]
  movie_short_ratings[,Name:= NULL]
  
  setkey(movie_short_ratings,Name2)
  
  merged_movies[,rel_yr:=substr(min(Date),1,4),by = Symbol]
  merged_movies[,Name2:=paste(Name,rel_yr,sep = '_')]
  
  setkey(merged_movies,Name)
  
  movies_rated <- merge(merged_movies,movie_short_ratings,by='Name2',all.x=TRUE)
  movies_rated[,Name:=Name2]
  movies_rated[,Name2:=NULL]
  movies_rated[,rel_yr:=NULL]
  
  return(movies_rated)
}

#builds features for analysis
build_features <- function(movies_rated){
  
  movies_rated[,RelPerf:=WeekGross/sum(WeekGross,na.rm=T),by = 'Date']#movie's pct of weekly gross
  movies_rated[!is.na(Audience),RelAudience:=rank(Audience)/length(Audience),by = 'Date']#scaled audience rank
  movies_rated[!is.na(Critic),RelCritic:=rank(Critic)/length(Critic),by = 'Date']#scaled critic rank
  movies_rated[!is.na(RevCt),RelCt:=rank(RevCt)/length(RevCt),by = 'Date']#scaled rank of number of reviews
  
  movies_rated[,GenreCt:=length(WeekGross),by = list(Date,Genre)]#Count of films of type out that week
  
  
  
  library(Hmisc)
  movies_rated[,lagRelPerf:=Lag(RelPerf,1),by = 'Symbol']#lag one period so RelPerf is predictive
  movies_rated[is.na(lagRelPerf),lagRelPerf:=0,by = 'Symbol']#fix NAs
  
  movies_rated[,gRelPerf:=WeekGross/sum(WeekGross,na.rm=T),by = list(Date,Genre)]#movie's pct of weekly gross by genre
  movies_rated[,lagGRelPerf:=Lag(gRelPerf,1),by = 'Symbol']#lag movie's pct of weekly gross by genre
  
  movies_rated[!is.na(Audience),gRelAudience:=rank(Audience)/length(Audience),by = list(Date,Genre)]#scaled audience rank by genre
  movies_rated[!is.na(Critic),gRelCritic:=rank(Critic)/length(Critic),by = list(Date,Genre)]#scaled critic rank by genre
  movies_rated[!is.na(RevCt),gRelCt:=rank(RevCt)/length(RevCt),by = list(Date,Genre)]#scaled rank of number of reviews by genre
  
  
  #convert weeks from factors to numbers for ordering
  movies_rated[ ,Week:=as.character(Week)]
  movies_rated[ ,Week:=as.numeric(Week)]
  
  setkey(movies_rated,Symbol,Date)#Reorder by symbol then week
  
  movies_rated[,lnWeekGross:=log(WeekGross)]#log WeeklyGross feature
  
  movies_rated <- movies_rated[!is.na(Week) ,]#remove NA week rows
  
  movies_rated[,pctWeek:=WeekGross/max(TotalGross),by = list(Symbol)]#percent of a movie's total in a given week
  
  movies_rated[,lagWeekGross:=Lag(WeekGross,1),by = list(Symbol)]#lag WeekGross for perdictive value
  movies_rated[,chgWeekGross:=WeekGross - lagWeekGross,by = list(Symbol)]#change in week to week WeekGross
  movies_rated[,chgWeekGross:=Lag(chgWeekGross,1),by = 'Symbol']#scaled rank of chgWeekGross
  movies_rated[Week==2,chgWeekGross:=lagWeekGross]#fix week 2
  movies_rated[is.na(chgWeekGross),chgWeekGross:=0]#repair NAs
  movies_rated[!is.na(chgWeekGross),RelChgWeekGross:=rank(chgWeekGross)/length(chgWeekGross),by = 'Date']#scaled rank of chgWeekGross
  
  
  movies_rated[,pctChgWeekGross:=chgWeekGross/Lag(WeekGross,2),by = list(Symbol)]#pct change in week to week WeekGross
  movies_rated[Week==2,pctChgWeekGross:=1]#fix week 2 pct chg
  movies_rated[is.na(pctChgWeekGross),pctChgWeekGross:=0]#repair NAs
  movies_rated[is.infinite(pctChgWeekGross),pctChgWeekGross:=-1]#repair infinite
  movies_rated[!is.na(pctChgWeekGross),RelPctChgWeekGross:=rank(pctChgWeekGross)/length(pctChgWeekGross),by = 'Date']#scaled rank of chgWeekGross
  
  movies_rated[is.na(EstOW),EstOW:=0]
  movies_rated[,OWres:=(WeekGross[1] - EstOW*1000000),by = list(Symbol)]#opening weekend HSX extimate residual
  movies_rated[!is.na(OWres),RelOWres:=rank(OWres)/length(OWres),by = list(Symbol)]#scaled rank of opening estimate residual
  
  return(movies_rated)
}

#find sequential optimization solutions for full frontier
optimizeFilms <- function(dol,optimizer){
  library(Rglpk)
  
  optimizeData <- copy(optimizer)
  boxoffice <- as.matrix(optimizeData$est_wk_box/1000000)
  
  movieCost <- as.matrix(as.numeric(as.character(optimizeData$FB)))
  num.movies <- length(optimizeData$Name)
  var.types <- rep("I", num.movies)
  
  A <- t(rbind(as.numeric(movieCost),                          
               rep(1,num.movies)))                       
  
  dir <- as.matrix(c("<=","=="))
  
  b <- (as.matrix(c(dol,8)))  
  
  sol <- Rglpk_solve_LP(obj = t(boxoffice), mat = t(A), dir = dir, rhs = b,types = var.types, max = TRUE,verbose=F)
  sol2 <- sum(optimizeData$est_wk_box*sol$solution)
  return(sol2)
}

#find optimal solution
optimizeFilm <- function(dol,optimizer){
  library(Rglpk)
  
  optimizeData <- copy(optimizer)
  boxoffice <- as.matrix(optimizeData$est_wk_box/1000000)
  
  movieCost <- as.matrix(as.numeric(as.character(optimizeData$FB)))
  num.movies <- length(optimizeData$Name)
  var.types <- rep("I", num.movies)
  
  A <- t(rbind(as.numeric(movieCost),                          
               rep(1,num.movies)))                     
  
  dir <- as.matrix(c("<=","=="))
  
  b <- (as.matrix(c(dol,8)))  
  
  sol <- Rglpk_solve_LP(obj = t(boxoffice), mat = t(A), dir = dir, rhs = b,types = var.types, max = TRUE,verbose=F)
  return(sol)
}

samp_res <- function(n){
  out <- sample(as.numeric(LHS-RHS),15)
  return(out)
}

single_solution <- function(n){
  optimizer_input <- merge(tab,this_week,by = 'Name',all.y = TRUE)
  optimizer_input <- optimizer_input[!is.na(est_wk_box)]
  optimizer_input[,est_wk_box:=est_wk_box+vals[n,]]
  optimizer_input[which(optimizer_input[,Week] == 1),est_wk_box:=est_wk_box+vals[n,][which(optimizer_input[,Week] == 1)]*2.5]
  #finds best 'value' film for $3000000 bonus - see Fantasy Movie League rules;
  #a more complete solution would spread the bonus value over the films based on their probability of being the best 'value'
  optimizer_input[,val_ranked:=rank(est_wk_box/FB)]
  optimizer_input[val_ranked == max(optimizer_input[,val_ranked]),est_wk_box:=est_wk_box+3000000]
  
  
  #adds row for no film in a theater
  blank <- data.table('Name' = 'blank','Week' = NA,'est_wk_box' = -2000000, 'FB' = 0, 'val_ranked' = 0) 
  optimizer_input <- rbind(optimizer_input,blank)
  optimizer_input[Name == 'blank', val_ranked:= NA]
  optimizer_input <- unique(optimizer_input)
  
  optimizer_input# show optimizer inputs for films
  
  #this is supposed to find the efficient frontier of boxoffice at each total theater spend (1-1000),
  #but the integer optimizer does not work as it is supposed to - I think it gets stuck in local minima or
  #punts when there are non-unique ways to get to a total theater spend;
  #I think I would implement a weighted random draw (run 1000000 times or so) to come up with better answers 
  #out <- lapply(1:1000,optimizeFilms,optimizer_input) 
  
  #takes best point on frontier and finds theater solution at that point 
  #solutions <- optimizeFilm(which(unlist(out) == max(unlist(out))),optimizer_input)
  solutions <- optimizeFilm(1000,optimizer_input)
  ##########
  #SOLUTION#
  ##########
  
  #displays results
  solution <- data.table(optimizer_input,'num_th' = solutions$solution)
  setkey(solution,Name)
  films <- paste(solution[num_th>0,Name],solution[num_th>0,num_th],sep = '_')
  out <- NULL
  for(i in 1:length(films)){
    out <- paste(out,films[i],sep = ',')
  }
  return(out)
}
