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
  if(sum(search() =='package:Hmisc')>0){
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

##grabs BoxOffice Mojo data from individual movie title page
grab_mojo_movies_data <- function(filmid){
  library(rvest)
  library(XML)
  library(RCurl)
  library(rjson)
  library(data.table)
  
  
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
  
  if(is.numeric(which(sub_pages %like% weekly_url))) {
    dl_weekly <- getURL(paste(base_url,weekly_url,sep = ''))
    
    weekly_data <- try(build_historic(dl_file = dl_weekly),silent = TRUE)
    weekly <- compose_weekly_dataframe(weekly_data)
    
    dl_weekend <- getURL(paste(base_url,weekend_url,sep = ''))
    weekend_data <- try(build_historic(dl_file = dl_weekend),silent = TRUE)
    weekend <- compose_weekly_dataframe(weekend_data) 
    
  }
  
  if(class(weekly)[1]!='try-error'){
    json_box$weekly <- weekly
  }
  
  if(class(weekend)[1]!='try-error'){
    json_box$weekend <- weekend
  }
  
  json_box$domestic_total_gross <- NULL
  json_box$release_date <- as.character(
    as.Date(json_box$release_date,'%B %d, %Y'))
  if(json_box$runtime %like% 'min.'){
    json_box$runtime <- as.chron.ITime(paste(gsub('hrs.|min.',
                                                  ':',json_box$runtime),'00',sep = ''))
    json_box$runtime <- as.numeric(json_box$runtime)*24*60
  } else {
    json_box$runtime <- NA
  }
  if(json_box$production_budget %like% 'million'){
    json_box$production_budget <- 
      as.numeric(gsub('[[:alpha:]]|[[:punct:]]| ','',
                      json_box$production_budget))*1000000
  } else {
    json_box$production_budget <- 
      as.numeric(gsub('[[:alpha:]]|[[:punct:]]| ','',
                      json_box$production_budget))
  }
  json_box <- toJSON(json_box,method = 'C')
  return(json_box)
}

#build lists of historic performance from mojo data
build_historic <- function(dl_file){
  library(XML)
  library(RCurl)
  library(data.table)
  library(rvest)
  
  
  weekend_links <- NULL
  weekend_days <- NULL
  weekend_grosses <- NULL
  weekend_theaters <- NULL
  weekend_release <- NULL
  
  weekend_links_column <- "font a"
  weekend_links <- read_html(dl_file)%>%
    html_nodes(weekend_links_column)%>%html_attrs()
  weekend_links <- unlist(weekend_links[which(weekend_links %like% 'yr=')])
  weekend_links <- iconv(weekend_links, from = "ISO-8859-1", to = "ASCII","")
  
  weekend_days_column <- 'nobr font a'
  weekend_days <- read_html(dl_file)%>%html_nodes(weekend_days_column)%>%
    html_text()
  weekend_days <- iconv(weekend_days, from = "ISO-8859-1", to = "ASCII","-")
  weekend_days <- na.omit(gsub('[[:punct:]]',':',weekend_days))
  
  
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
  
  out <- NULL
  out$weekend_links <- weekend_links 
  out$weekend_days <- weekend_days
  out$weekend_grosses <- weekend_grosses
  out$weekend_theaters <- weekend_theaters
  out$weekend_release <- weekend_release
  return(out)
}

#changes lists of historic performance from mojo data into dataframe
compose_weekly_dataframe <- function(weekly_data){
  library(data.table)
  
  weekend_release <- weekly_data$weekend_release
  weekend_links <- weekly_data$weekend_links
  weekend_days <- weekly_data$weekend_days
  weekend_grosses <- weekly_data$weekend_grosses
  weekend_theaters <- weekly_data$weekend_theaters
  
  out <- list()
  
  for(i in c(1:length(weekend_release))){
    out[[i]] <- build_week(i,weekend_release,weekend_links,
                           weekend_days,weekend_grosses,
                           weekend_theaters)
  }
  
  out <- rbindlist(out)
  
  check <- out$days
  
  #fixes bad handling of 3 day weekends
  if(2 %in% check & 3 %in% check){
    remove <- which(out[,days]>2)-1
    if(is.numeric(remove)){
      out <- out[!remove]
      out[,rel_week:=c(1:dim(out)[1])]
    }
  }
  
  return(out)
}

#build a single week's performance
build_week <- function(i,rel,lnks,dys,gro,thea){
  library(data.table)
  
  
  rel_week <- rel[i]
  norm_date <- unlist(strsplit(lnks[i],'='))
  year <- substr(norm_date[2],1,4)
  week <- substr(norm_date[3],1,2)
  dates <- stringr::str_split(dys[i],'::')[[1]]
  
  check <- try(as.numeric(dates[2]), silent = TRUE)
  if(!is.na(check)){
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
  df <- as.data.table(df)
  return(df)
  
}

#writes movie json data to file
write_list <- function(i,lists,filename){
  if(filename %in% list.files(getwd())){
    write(lists[[i]], file = filename, sep = '\n', append = TRUE)
    closeAllConnections()
    return(NULL)
  } else {
    con <- file(filename)
    open(con, open = 'w')
    writeLines(lists[[i]], con, sep = '\n')
    closeAllConnections()
    return(NULL)
  }
}








add_metacritic_to_json_row <- function(json_row){
  library(rjson)
  library(stringr)
  
  film <- fromJSON(json_row, method = 'C')
  filmid <- make_metacritic_id(film)
  
  rating <- NULL
  
  if(str_count(film$title,'\\(*[[0-9]]{4}\\)') == 1){
    rating <- try(grab_metacritic_data(filmid[2]),silent = T)
    
    if(class(rating)[1] == 'try-error'){
      rating <- try(grab_metacritic_data(filmid[3]),silent = T)
    }
  }
  
  if(class(rating)[1] == 'try-error' | is.null(rating)){
    rating <- try(grab_metacritic_data(filmid[1]),silent = T)
  }
  
  if(class(rating)[1] == 'try-error' | is.null(rating)){
    return(json_row)
  } else {
    
    film$rating$critic_avg <- rating$main_vals[1]
    film$rating$audience_avg <- rating$main_vals[2]
    film$rating$critic_scores <- rating$critic_scores
    
    json_row <- toJSON(film, method = 'C')
  }
  
  rm(film)
  
  return(json_row)
}

make_metacritic_id <- function(film){
  library(stringr)
  
  title <- film$title
  title <- strsplit(title,' \\(')[[1]]
  filmid <- title[1]
  filmid <- gsub('3,2,1...','3-2-1',filmid)
  filmid <- gsub('\\$9.99','999',filmid)
  
  if(is.numeric(str_count(filmid,':[[:alpha:]]'))){
    filmid <- gsub(':','-',filmid)
  }
  
  filmid <- gsub('-','qqqqq',filmid)
  filmid <- gsub('\\$','s',filmid)
  filmid <- gsub('\\&','and',filmid)
  filmid <- gsub('[[:punct:]]','',filmid)
  filmid <- gsub(' |qqqqq','-',filmid)
  filmid <- tolower(filmid)
  
  year <- suppressWarnings(as.numeric(substr(film$release_date,1,4)))
  
  filmid <- c(filmid,paste(filmid,'-',year,sep = ''))
  filmid <- c(filmid,paste(filmid[1],'-',year-1,sep = ''))
  
  return(filmid)
}

grab_metacritic_data <- function(filmid){
  library(rvest)
  library(XML)
  library(RCurl)
  
  baseurl <- 'http://www.metacritic.com/movie/'
  dl_url <- getURLContent(paste(baseurl,filmid,sep=''), useragent = 'Moviefan-via-R')
  closeAllConnections()
  main_vals <- read_html(dl_url)%>%html_nodes('a div')%>%html_text()
  dl_url <- getURLContent(paste(baseurl,filmid,'/critic-reviews',sep=''), useragent = 'Moviefan-via-R')
  closeAllConnections()
  critic_scores <- read_html(dl_url)%>%html_nodes('#main .indiv')%>%html_text()
  
  out <- NULL
  out$main_vals <- suppressWarnings(as.numeric(main_vals))
  out$critic_scores <- suppressWarnings(as.numeric(critic_scores))
  
  return(out)
}

#builds actor datatable from json line
actor_frame <- function(json_line){
  library(rjson)
  library(data.table)
  
  film <- fromJSON(json_line, method = 'C')
  actors <- film$talent$actors
  actors <- gsub(' \\([[:alpha:]]*\\)','',actors)
  data.table('title' = film$title, actors)
  
}


#function that attaches various measures from the actor graph to the json line
attach_actor_data <- function(json_line,actor_dat){
  library(rjson)
  library(data.table)
  
  film <- fromJSON(json_line, method = 'C')
  actors <- film$talent$actors
  film$talend$actors <- NULL
  actors <- data.table('name' = film$talent$actors)
  setkey(actors,name)
  nms <- actors[,name]
  actors[,name:=enc2utf8(name)]
  actors[,name:= gsub('[[:punct:]]| ','',name)]
  actors <- actor_dat[actors]
  actors[,name:= nms]
  film$talent$actors <- actors
  new_line <- toJSON(film, method = 'C')
  return(new_line)
}

#function that splits arules columns into appropriate relationships for igraph
splitter <- function(x){
  library(data.table)
  
  out <- strsplit(x,' --- ')[[1]]
  out <- data.table('person1' = out[1], 'person2' = out[2])
  return(out)
}