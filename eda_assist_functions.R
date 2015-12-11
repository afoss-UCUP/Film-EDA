# sets knit hook options to print image captions in html
knit_hooks$set(htmlcap = function(before, options, envir) {
  if (!before) {
    paste('<p class="caption", align = "center"><font size = "2">', options$htmlcap,
          "</font></p>",
          sep = "")
  }
})

#univariate plotter
uni_plot <- function(df, col, rang, logx){
  
  dat <- na.omit(df[, list(V1 = get(col))])
  if(logx){
    bw <- (log10(rang[2])-log10(rang[1])) / 40
  } else {
    bw <- (rang[2]-rang[1])/40
  }
  plt <- ggplot(aes(x = V1), data = dat) +
    geom_bar(binwidth = bw, fill = 'navyblue') +
    coord_cartesian(xlim = rang) +
    xlab(col)
  
  if(logx){
    plt <- plt + scale_x_log10()
    plt <- plt + xlab(paste('log10(', col, ')', sep = ''))
  }
  
  return(plt)
}
  
# takes a data.table and column 
# then quantile range to set x/y limits when plotting
get_chart_range <- function(df,col,bg,en){
  quantile(as.numeric(df[, get(col)]), c(bg, en), na.rm = T)
}

#unlists a variable and coerces it to numeric
numlist <- function(x){
  as.numeric(unlist(x))
}

# builds a dataframe of selected film attrubutes from a json document
# highly specific to the json data structure I used
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

# builds data.table of unique films given a column in that table
unique_films <- function(df, column){
  library(data.table)
  df <- unique(df[, list(get(column), title)])
  setnames(df, 'V1', column)
  return(df)
}

# creates a correlation marker for use in plots
plot_cor <- function(col1, col2) {
  if(length(col1) != length(col2)){
    return('Error: col length mismatch')
  } else {
    c1 <- cor(cbind(col1, col2), use = 'complete')[1, 2]
    c1 <- paste('R = ',round(c1, 3), sep = '')
  }
  return (c1)
}

# finds diag attributes in ggpairs object and replaces them with variable names
# from data.table to improve plot readability
make_diag_name <- function(plt,df){
  library(stringr)
  diags <- which(plt$plots %like% 'Diag')
  for(i in 1:length(diags)){
    plt$plots[[diags[i]]] <- paste("ggally_text('",
                                   str_wrap(gsub("_"," ",names(df)[i]), width = 8)
                                   ,"', angle = 45, size = 3.5)",sep = '')
  }
  return(plt)
}

# finds cor attributes in ggpairs object and adds a text size
# to improve plot readability
change_cor_size <- function(plt,sz){
  library(stringr)
  cors <- which(plt$plots %like% '_cor')
  for(i in 1:length(cors)){
    val <- plt$plots[[cors[i]]]
    val <- substr(val,1,nchar(val)-1)
    val <- paste(val,", size = ",sz,")", sep = '')
    plt$plots[[cors[i]]] <- val
  }
  return(plt)
}

#make categorical 'tiers' from relative week 1 numeric variable
make_tier <- function(df, col, quan){
  library(data.table)
  
  vals <- df[relative_week == 1, list(title, get(col))]
  tiers <- cut(vals[, V2], breaks = quantile(vals[, V2], quan, na.rm = T))
  tiers <- factor(tiers, ordered = T)
  vals[, eval(paste(col, 'tier', sep = '_')):= tiers]
  vals[, V2:= NULL]
  
  return(vals)
}
    
#builds multivariate plots vs relative week and some tier
make_tier_week_plot <- function(df, col, tier, logx, logy){
  # find display range
  plot_range <- get_chart_range(df, as.character(col), .01, .999)
  
  #build small dataset
  dat <- df[, list(relative_week, V1 = get(col), V2 = get(tier))]
  
  # build ggplot
  plt <- ggplot(aes(x = relative_week,
                  y = V1,
                  color = V2),
              data = dat[!is.na(V1) & !is.na(V2), ]) +
    geom_point(alpha = .125, position = position_jitter(w = .15)) +
    geom_line(stat = 'summary',
              fun.y = 'median',
              size = 1) +
    geom_line(stat = 'summary',
              fun.y = quantile, prob = .25,
              linetype = 2,
              size = .5) +
    geom_line(stat = 'summary',
              fun.y = quantile, prob = .75,
              linetype = 2,
              size = .5) +
    xlab('relative_week') +
    theme(legend.position="bottom") +
    labs(color = tier) +
    scale_color_brewer(type = 'seq', palette = 'Spectral') +
    coord_cartesian(xlim = c(.5, 10.5),
                    ylim = plot_range) +
    ylab(col)
  
  if(logx){
    plt <- plt + scale_x_log10(breaks = seq(1,10,1))
    plt <- plt + xlab(paste('log10(relative_week)', sep = ''))
    plt <- plt + coord_cartesian(xlim = c(1, 10.5),
                                 ylim = plot_range)
  }
  if(logy){
    plt <- plt + scale_y_log10()
    plt <- plt + ylab(paste('log10(', col, ')', sep = ''))
  }
  
  return(plt)
}

#extract legends for complex plots
grab_legend <- function(plt){
  # get grobs
  ptab <- ggplot_gtable(ggplot_build(plt))
  # make list of grob names
  lst <- lapply(ptab$grobs, function(x) x$name)
  # find legend grob
  sec <- which(lst %like% 'guide-box')
  legend <- ptab$grobs[[sec]]
  return(legend)
}

#converts level value to numeric value for legends
conv_tier <- function(x, mils){
  x <- unlist(strsplit(x,','))
  x <- gsub(']', '', x)
  x <- gsub('\\(', '', x)
  x <- as.numeric(x)
  if(mils){
    x <- paste(x[1]/1000000,'m to ',x[2]/1000000,'m', sep = '')
  } else {
    x <- paste(x[1],' to ',x[2], sep = '')
  }
  return(x)
}


