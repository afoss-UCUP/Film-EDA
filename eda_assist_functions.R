# sets knit hook options to print image captions in html
knit_hooks$set(htmlcap = function(before, options, envir) {
  if (!before) {
    paste('<p class="caption", align = "center"><font size = "2">', options$htmlcap,
          "</font></p>",
          sep = "")
  }
})

# sets knit hook options to print image captions in html
get_chart_range <- function(df,col,bg,en){
  quantile(as.numeric(df[, get(col)]), c(bg, en), na.rm = T)
}

#unlists a variable and coerces it to numeric
numlist <- function(x){
  as.numeric(unlist(x))
}

#builds a dataframe of selected film attrubutes from a json document
make_film_frame <- function(json_line){
  
  film <- fromJSON(json_line, method = 'C')
  film$gross <- film$box_office$domestic
  film$get_local_showtimes_at_imdb <- NULL
  if (is.na(as.numeric(film$gross))){
    return(NULL)
  } else {
    film$box_office <- NULL
    
    
    film$max_theaters <- max(numlist(film$weekly$theaters), na.rm = T)
    film$cum_theaters <- sum(numlist(film$weekly$theaters), na.rm = T)
    film$med_theaters <- median(numlist(film$weekly$theaters), na.rm = T)
    film$count_weeks <- length(numlist(film$weekly$rel_week))
    film$runtime <- as.numeric(film$runtime)
    film$production_budget <- as.numeric(film$production_budget)
    film$week <- film$weekly$week
    film$weekly_gross <- numlist(film$weekly$gross)
    film$weekly_theaters <- numlist(film$weekly$theaters)
    film$weekly_days <- numlist(film$weekly$days)
    film$relative_week <- numlist(film$weekly$rel_week)
    film$release_date <- as.Date(film$release_date, '%Y-%m-%d')
    film$release_mo_wk <- paste(substr(film$release_date, 6, 7),
                                ceiling(as.numeric(substr(film$release_date, 9, 10)) / 7),
                                sep = '_')
    film$weekend <- NULL
    film$weekly <- NULL
    
    if (!is.null(film$talent$actor$name)){
      
      if (!(is.na(film$talent$actor$name) | film$talent$actor$name %in% 'NA')[1]){
        
        film$count_talent <- length(numlist(film$talent$actor$degree))
        
        film$talent_max_pics <- max(numlist(film$talent$actor$pic_count))
        film$talent_median_pics <- median(numlist(film$talent$actor$pic_count))
        film$talent_cum_pics <- sum(numlist(film$talent$actor$pic_count))
        
        film$talent_max_deg <- max(numlist(film$talent$actor$degree))
        film$talent_median_deg <- median(numlist(film$talent$actor$degree))
        film$talent_cum_deg <- sum(numlist(film$talent$actor$degree))
        
        film$talent_max_bet <- max(numlist(film$talent$actor$betweenness))
        film$talent_median_bet <- median(numlist(film$talent$actor$betweenness))
        film$talent_cum_bet <- sum(numlist(film$talent$actor$betweenness))
        
      }
    } 
    
    if (!('talent_max_deg' %in% names(film))){
      
      film$count_talent <- NA
      
      film$talent_max_pics <- NA
      film$talent_median_pics <- NA
      film$talent_cum_pics <- NA
      
      film$talent_max_deg <- NA
      film$talent_median_deg <- NA
      film$talent_cum_deg <- NA
      
      film$talent_max_bet <- NA
      film$talent_median_bet <- NA
      film$talent_cum_bet <- NA
      
    }
    
    if ('rating' %in% names(film)){
      film$count_critics <- try(length(numlist(film$rating$critic_scores)), silent = T)
      film$critics_avg <- try(mean(numlist(film$rating$critic_scores), na.rm = T), silent = T)
      film$critics_med <- try(median(numlist(film$rating$critic_scores), na.rm = T), silent = T)
      if (class(film$critics_avg)[1] == 'try-error' | !is.numeric(film$critics_avg)){
        film$critics_avg <- NA
      }
      film$audience_avg <- try(as.numeric(film$rating$audience_avg), silent =T)
      if (class(film$audience_avg)[1] == 'try-error'){
        film$audience_avg <- NA
      }
      film$critics_IQR <- try(IQR(numlist(film$rating$critic_scores)),silent = T)
      if (class(film$critics_IQR)[1] == 'try-error'){
        film$critics_IQR <- NA
      }
      film$rating <- NULL
    } else {
      film$count_critics <- NA
      film$critics_avg <- NA
      film$critics_med <- NA
      film$audience_avg <- NA
      film$critics_IQR <- NA
    }
    film$talent <- NULL
    infs <- which(is.infinite(numlist(film)))
    film <- as.data.frame(film)
    film[, infs] <- NA
  }
  return(film)
}

unique_films <- function(df, column){
  library(data.table)
  df <- unique(df[, list(get(column), title)])
  setnames(df, 'V1', column)
  return(df)
}