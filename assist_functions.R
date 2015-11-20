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
  library(RCurl)
  #unload Hmisc package if loaded due to issues
  if(sum(search()=='package:Hmisc')>0){
    detach("package:Hmisc", unload=TRUE)	
  }
  
  tab <- NULL
  tab1 <- NULL
  tab2 <- NULL
  tab3 <- NULL
  test <- NULL
  
  tab <- data.table('Name' = NA, 'Critic' = NA, 'Audience' = NA, 'AvgRev' = NA, 'RevCt' = NA)
  tab$Name <- paste(movie_names[,Name][name_vector],substr(movie_names[,Date][name_vector],1,4),sep='_')
  
  test <- try(read_html(paste("http://www.rottentomatoes.com/m/",as.character(movie_names[,Name][name_vector]),sep="")),silent = TRUE)
  tab1 <- try(as.numeric(gsub(" |%","",html_text(xml_find_all(test, ".//*[@id = 'tomato_meter_link']")[1]))),silent = TRUE)
  if(class(tab1)[1]=='try-error'){
    test <- try(read_html(paste("http://www.rottentomatoes.com/m/",as.character(paste(movie_names[,Name][name_vector],substr(movie_names[,Date][name_vector],1,4),sep='_')),sep="")),silent = TRUE)
    tab1 <- try(as.numeric(gsub(" |%","",html_text(xml_find_all(test, ".//*[@id = 'tomato_meter_link']")[1]))),silent = TRUE)
  }
  
  if(class(tab1)[1]=='try-error'){
    test <- try(read_html(getURL(paste("http://www.rottentomatoes.com/search/?search=",gsub('the+','',gsub('_','+',as.character(paste(movie_names[,Name][name_vector],substr(movie_names[,Date][name_vector],1,4),sep='_')))),sep=""), followlocation = T)),silent = TRUE)
    tab1 <- try(as.numeric(gsub(" |%","",html_text(xml_find_all(test, ".//*[@id = 'tomato_meter_link']")[1]))),silent = TRUE)
  }
  
  
  if(class(tab1)[1]!='try-error'){
    tab2 <-  try(as.numeric(gsub(" |%","", (html_text(xml_find_all(test, ".//*[@class='meter-value']"))))),silent = TRUE)
    tab3 <-  try(strsplit(html_text(xml_find_all(test, ".//*[@id='scoreStats']")[1]),':'),silent = TRUE)
    avg_critic <- as.numeric(strsplit(tab3[[1]][2],'/')[[1]][1])
    rev_count <- as.numeric(strsplit(tab3[[1]][3],'   ')[[1]][1])
    tab$Critic = tab1
    if(class(tab2)[1]!='try-error'){
      if(length(tab2) != 0){
        tab$Audience = tab2
      }
    }
    if(class(tab3)[1]!='try-error'){
      tab$AvgRev = avg_critic
      tab$RevCt = rev_count
    }
    
    
  }
  gc()
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





##grabs BoxOffice Mojo Movie Table of Contents
grab_mojo_toc <- function(){
  library(rvest)
  library(XML)
  #unload Hmisc package if loaded due to issues
  if(sum(search()=='package:Hmisc')>0){
    detach("package:Hmisc", unload=TRUE)	
  }
  
  test <- try(html_text(read_html("http://www.boxofficemojo.com/movies/")%>%
                          html_nodes('tr+ tr b a')),silent = TRUE)
  letter_list <- unique(gsub('\n| ','',test)) 
  letter_list <- letter_list[-length(letter_list)]
    
  return(letter_list)
}

#Extracts movie pages from BoxOffice Mojo hrefs
extract_mojo_movie_pages <- function(movie_list){
  elements <- NULL
  for(tag in movie_list){
    if(!is.na(pmatch('/movies/?id',tag))){
      elements <- c(elements,tag)
    }
  }
  return(elements)
}

##grabs BoxOffice Mojo List of links on alphabetical pages 
grab_mojo_movies_links <- function(letter){
  library(rvest)
  library(XML)
  library(RCurl)
  #unload Hmisc package if loaded due to issues
  if(sum(search()=='package:Hmisc')>0){
    detach("package:Hmisc", unload=TRUE)	
  }
  
  letter <- gsub('#','NUM',letter)
  base_url <- 'http://www.boxofficemojo.com/'
  url <- "http://www.boxofficemojo.com/movies/alphabetical.htm?letter="
  dl_url <- getURL(paste(url,letter,'&p=.htm', sep = ""))
  
  sub_pages <- getHTMLLinks(dl_url, xpQuery = 
                              "//*[@class = 'alpha-nav-holder']//a/@href")
  sub_pages <- unique(sub_pages)
 
  movie_list <- getHTMLLinks(dl_url,xpQuery = "//table//tr//td//font/a/@href")
  movie_links <- build_movie_list(movie_list)
  
  for(pg in sub_pages){
    dl_url <- getURL(paste(base_url,pg,sep = ''))
    movie_list <- getHTMLLinks(dl_url, xpQuery = "//table//tr//td//a/@href")
    movie_links <- rbind(movie_links,build_movie_list(movie_list))
    
  }
  
  
  return(movie_links)
}

#takes mojo link list and builds movie & date dataframe
build_movie_list <- function(movie_list){
  library(data.table)
  df <- NULL
  df$filmid <- NULL
  df$open_date <- NULL
  id = 0
  
  for(element in movie_list){
    if(element %like% 'id=' & id == 0){
      df$filmid <- c(df$filmid, element)
      id = 1
    } else {
      if(element %like% 'id=' & id == 1){
        df$open_date <- c(df$open_date, NA)
        df$filmid <- c(df$filmid, element)
        id = 1
      } else {
        if(element %like% 'date=' & id == 1){
          dt <- unlist(strsplit(element,'date='))[2]
          dt <- substr(dt,1,10)
          df$open_date <- c(df$open_date,dt)
          id = 0
        } else {
          if(element %like% 'date=' & id == 0){
            dt <- unlist(strsplit(element,'date='))[2]
            dt <- substr(dt,1,10)
            df$open_date <- c(df$open_date,dt)
            df$filmid <- c(df$filmid,NA)
            id = 1
          }
        }
      }
    }
  }
  return(as.data.table(df))
}

##grabs BoxOffice Mojo data from individual movie title page
grab_mojo_movies_data <- function(filmid){
  library(rvest)
  library(XML)
  library(RCurl)
  library(rjson)
  #unload Hmisc package if loaded due to issues
  if(sum(search()=='package:Hmisc')>0){
    detach("package:Hmisc", unload=TRUE)	
  }
  
  base_url <- 'http://www.boxofficemojo.com/'
  dl_url <- getURL(paste(base_url,filmid,'&p=.htm', sep = ""))
  center_box <- read_html(dl_url)%>%html_nodes('center td')
  title <- read_html(dl_url)%>%html_nodes('br+ font b')%>%
    html_text()
  
  json_box <- list()
  json_box$title <- title
  for(line in center_box){
    new_line <- try(strsplit(html_text(line,trim = TRUE),': ')[[1]],
                    silent = TRUE)
    list_name <- tolower(gsub(' ','_',new_line[1]))
    value <- new_line[2]
    json_box[list_name] <- value
  }
  
  b_tags <- xpathSApply(htmlParse(dl_url),"//b",xmlValue)
  
  td_tags <- xpathSApply(htmlParse(gsub('<br>','\n',dl_url)),"//td",xmlValue) 
  
  json_box <- get_box_from_b_tags(b_tags,json_box)
  json_box <- get_talent_from_td_tags(td_tags,json_box)
  
  sub_pages <- getHTMLLinks(dl_url, xpQuery = 
                              "//ul/li/a/@href")
  
  weekly_url <- paste("/movies/?page=weekly&id=",
                  unlist(strsplit(filmid,'id='))[2],sep = '')
  
  weekend_url <- paste("/movies/?page=weekend&id=",
                  unlist(strsplit(filmid,'id='))[2],sep = '')
  
  weekly <- NULL
  weekend <- NULL
  
  if(length(which(sub_pages == weekly_url))!=0) {
    dl_weekly <- getURL(paste(base_url,weekly_url,sep = ''))
    
    weekly <- try(build_historic_performance(dl_weekly),silent = TRUE)
    
    dl_weekend <- getURL(paste(base_url,weekend_url,sep = ''))
    weekend <- try(build_historic_performance(dl_weekend),silent = TRUE) 
    
  }
  
  if(class(weekly)!='try-error'){
    json_box$weekly <- weekly
  }
  
  if(class(weekend)!='try-error'){
    json_box$weekend <- weekend
  }
  
  json_box$domestic_total_gross <- NULL
  json_box$release_date <- as.Date(json_box$release_date,'%B %d, %Y')
  json_box$runtime <- as.chron.ITime(paste(gsub('hrs.|min.',
                              ':',json_box$runtime),'00',sep = ''))
  json_box$runtime <- as.numeric(json_box$runtime)*24*60
  if(json_box$production_budget %like% 'million'){
    json_box$production_budget <- 
      as.numeric(gsub('[[:alpha:]]|[[:punct:]]| ','',
           json_box$production_budget))*1000000
  } else {
    json_box$production_budget <- 
      as.numeric(gsub('[[:alpha:]]|[[:punct:]]| ','',
           json_box$production_budget))
  }
  
  return(toJSON(json_box,method = 'C'))
}

#parses boxoffice info from mojo page
get_box_from_b_tags <- function(b_tags,json_box){
  json_box$box_office$domestic <- 
    as.numeric(gsub('\\$|,',"",b_tags[which(b_tags == 'Domestic:')+1]))
  
  json_box$box_office$total <- 
    as.numeric(gsub('\\$|,',"",b_tags[which(b_tags == 'Worldwide:')+1]))
  
  json_box$box_office$foreign <- 
    json_box$box_office$total - json_box$box_office$domestic
  
  if(length(json_box$box_office$domestic)==0){
    json_box$box_office$domestic <- NA
  }
  
  if(length(json_box$box_office$total)==0){
    json_box$box_office$total <- NA
  }
  
  if(length(json_box$box_office$foreign)==0){
    json_box$box_office$foreign <- NA
  }
  
  return(json_box)
}

#parses talent info from mojo page
get_talent_from_td_tags <- function(td_tags,json_box){
  json_box$talent$director <- 
    unlist(strsplit(td_tags[which(td_tags == 'Director:')+1],'\n'))
  
  #json_box$talent$writer <- 
  #  unlist(strsplit(td_tags[max(c(which(td_tags == 'Writer:')+1,
  #                            which(td_tags == 'Writers:')+1),na.rm=T)],'\n'))
  
  json_box$talent$actors <- 
    gsub('\\*','',unlist(strsplit(td_tags[which(td_tags == 'Actors:')+1],'\n')))
  
  if(length(json_box$talent$director)==0){
    json_box$talent$director <- NA
  }
  
  #if(length(json_box$talent$writer)==0){
  #  json_box$talent$writer <- NA
  #}
  
  if(length(json_box$talent$actors)==0){
    json_box$talent$actors <- NA
  }
  
  return(json_box)
}

#build table of historic performance from mojo data
build_historic_performance <- function(dl_file){
  library(XML)
  library(RCurl)
  library(data.table)
  library(rvest)
  
  weekend_links <- NULL
  weekend_days <- NULL
  weekend_grosses <- NULL
  weekend_theaters <- NULL
  weekend_release <- NULL
  
  weekend_links <- getHTMLLinks(dl_file, xpQuery ="//font/a/@href")
  weekend_links<- weekend_links[which(weekend_links %like% 'yr=')]
  
  weekend_days_column <- 'nobr font a'
  weekend_days <- read_html(dl_file)%>%html_nodes(weekend_days_column)%>%
    html_text()
  weekend_days <- na.omit(gsub('Â–','-',weekend_days))
  
  weekend_gross_column <- 'tr+ tr td:nth-child(3) font'
  weekend_grosses <- read_html(dl_file)%>%html_nodes(weekend_gross_column)%>%
    html_text()
  weekend_grosses <- na.omit(as.numeric(gsub('[[:punct:]]|[[:alpha:]]','',weekend_grosses[-1])))
  
  weekend_theater_column <- 'tr+ tr td:nth-child(5) font'
  weekend_theaters <- read_html(dl_file)%>%html_nodes(weekend_theater_column)%>%
    html_text()
  weekend_theaters <- na.omit(as.numeric(gsub('[[:punct:]]','',weekend_theaters)))
  
  weekend_release_column <- 'tr+ tr td:nth-child(9) font'
  weekend_release <- read_html(dl_file)%>%html_nodes(weekend_release_column)%>%
    html_text()
  weekend_release <- na.omit(as.numeric(gsub('[[:punct:]]','',weekend_release)))
  weekend_release <- c(1:length(weekend_release))
  
  out <- lapply(c(1:length(weekend_release)),
                build_week_performance,weekend_release,weekend_links,weekend_days,
                weekend_grosses,weekend_theaters)
  out <- rbindlist(out)
  
  #fixes bad handling of 3 day weekends
  if(median(out[[4]])<5){
    remove <- which(out[,days]>2)-1
    out <- out[!remove]
    out[,rel_week:=c(1:dim(out)[1])]
  }
  
  return(out)
}

#build a single week's performance
build_week_performance <- function(i,rel,lnks,dys,gro,thea){
  library(data.table)
  
  rel_week <- rel[i]
  norm_date <- unlist(strsplit(lnks[i],'='))
  year <- substr(norm_date[2],1,4)
  week <- substr(norm_date[3],1,2)
  dates <- unlist(strsplit(dys[i],'-'))
  if(!is.na(as.numeric(dates[2]))){
    end <- paste(substr(dates[1],1,3),' ',dates[2],', ',year, sep = '')
    begin <- paste(dates[1],', ',year, sep = '')
  } else {
    begin <- paste(dates[1],', ',year, sep = '')
    if(substr(dates[1],1,3) %like% 'Dec' & substr(dates[2],1,3) %like% 'Jan'){
      end <- paste(dates[2],', ',as.numeric(year)+1, sep = '')
    } else {
      end <- paste(dates[2],', ',year, sep = '')
    }
  }
  begin <- as.Date(begin,'%b %d, %Y')
  end <- as.Date(end,'%b %d, %Y')
  
  days <- as.numeric(end - begin)
  gross <- gro[i]
  theaters <- thea[i]
  
  df <- NULL
  df$rel_week <- NA
  df$week <- NA
  df$theaters <- NA
  df$days <- NA
  df$gross <- NA
  
  df <- NULL
  df$rel_week <- rel_week
  df$week <- week
  df$theaters <- theaters
  df$days <- days
  df$gross <- gross
  
  return(as.data.table(df))
  
}

