###########################################################
# This script controls all of the website scraping utilities
# Please do not run - it takes hours to scrape everything
############################################################


setwd("~/../version-control/Film-EDA")  # set my working directory
graphics.off() # close charts
rm(list = ls(all = TRUE))

# set my working directory
setwd("~/../version-control/Film-EDA")
#load additional functions
source(paste(getwd(), '/data_assembly_utilities.R', sep = ''))

library(parallel)
library(rvest)
library(XML)
library(RCurl)
library(data.table)
library(rjson)
library(arules)
library(Matrix)
library(igraph)
library(doParallel)

########################################
#STEP 1 - GRAB Box Office Mojo page IDs#
########################################

#generate alphabet links from boxoffice mojo TOC
letter_list <- grab_mojo_toc()

#parallized scraping of all boxoffice mojo movie pages
cl <- makePSOCKcluster(7, outfile="")
setDefaultCluster(cl)
clusterExport(NULL, c('extract_mojo_movie_pages', 'build_movie_list'))
mojo_links <- parLapply(cl, letter_list, grab_mojo_movies_links)
stopCluster(cl)
closeAllConnections()
gc()

#filter results for more recent releases
mojo_links <- rbindlist(mojo_links)
mojo_links <- mojo_links[open_date > '2003-11-18' & !is.na(open_date), ]

##############################################
#STEP 2 - Scrape Box Office Mojo movie data  #
##############################################

#parallelized scraping of each boxoffice mojo individual movie page
cl <- makePSOCKcluster(7, outfile="")
registerDoParallel(cl)
movie_data_list <- foreach(i = 1:dim(mojo_links)[1], 
                           .init = NULL,
                           .errorhandling = 'remove', 
                           .verbose = TRUE, 
                           .combine = c,
                           .export = c('get_box_from_b_tags',
                                       'get_talent_from_td_tags',
                                       'build_historic',
                                       'build_week',
                                       'compose_weekly_dataframe'),
                           .packages = c('rvest'
                                         ,'XML',
                                         'RCurl',
                                         'data.table',
                                         'rjson')
) %dopar% {
  filmid <- mojo_links[i, filmid]
  instance <- grab_mojo_movies_data(filmid)
}
stopCluster(cl)
closeAllConnections()
gc()

#save Rdata file of scraped pages (just in case)
save(movie_data_list, file = 'movie_data_list.Rdata')

#repair remaining title and domestic performance fields
i <- 1
while(i < length(movie_data_list) + 1){
  check <- fromJSON(movie_data_list[[i]], method = 'C')
  if(length(check$title) > 1){
    check$title <- check$title[1]
  }
  if(sum(names(check) %like% 'domestic') > 0){
    nm <- which(names(check) %like%' domestic')
    check[[nm]] <- NULL
    print(i)
  }
  movie_data_list[[i]] <- toJSON(check, method = 'C')
  
  i <- i + 1
}

#write to boxoffice mojo json doc (intermediate step)
lapply(1:length(movie_data_list), 
       write_list, 
       movie_data_list,
       'boxoffice_mojo_data.json')

#reload json doc
file <- 'boxoffice_mojo_data.json'
con = file(file, "r")
movie_json_list <- readLines(con, -1L)
closeAllConnections()

#######################################################
#STEP 3 - Append Metacritic Critic and Audience data  #
#######################################################

#add critic and audience from metacritic
json_with_critics <- lapply(movie_json_list, add_metacritic_to_json_row)

#write to json doc (intermediate step)
lapply(1:length(json_with_critics), 
       write_list, 
       json_with_critics,
       'boxoffice_mojo_data_with_critics.json')

#######################################################
#STEP 4 - Append Metacritic Critic and Audience data  #
#######################################################

#reload json with critics doc
file <- 'boxoffice_mojo_data_with_critics.json'
con = file(file, "r")
movie_with_critics_json_list <- readLines(con, -1L)
closeAllConnections()

########################################################
#STEP 5 - Build measures of talent w/ arules and igraph#
########################################################

#build long format table of films w/ actors
actor_list <- lapply(movie_with_critics_json_list, actor_frame)
actor_table <- rbindlist(actor_list)

#counts of projects by actor
actor_projects <- actor_table[, pic_count := length(title), by= 'actors']

#project film and actor table to wide format
actors_wide <- dcast(actor_table, title ~ actors, value.var = 'title', length)

#coerce to matrix to allow for itemMatrix transformation
actors_wide <- as.matrix(actors_wide[, 2:dim(actors_wide)[2], with = F])

#coerce to itemMatrix to allow running apriori algorithm in arules
actors_wide <- as(actors_wide, 'itemMatrix')

#builds list of 'rules'; pairs of actors that worked together 
rules <- apriori(actors_wide, 
                 parameter = list(minlen = 2, 
                                  maxlen = 2, 
                                  support = 0, 
                                  confidence = .001))
rules_vec <- labels(rules, 
                    itemSep = " + ", 
                    setStart = "", 
                    setEnd = "", 
                    ruleSep = " --- ")

#parallelized build of actor pairs frame for igraph
cl <- makePSOCKcluster(7, outfile="")
setDefaultCluster(cl)
clusterExport(NULL, c('splitter'))
rules_table <- parLapply(cl, rules_vec, splitter)
stopCluster(cl)
closeAllConnections()
gc()

rules_table <- rbindlist(rules_table)

#selects non-duplicates for making undirected graph
rules_table <- rules_table[seq(1:(dim(rules_table)[1] / 2)) * 2]

#removes NA entries
rules_table <- rules_table[!(person2 == 'NA' | person1 == 'NA'),]

#build graph
actor_network <- graph.data.frame(rules_table, directed=F)

#assembles a datatable of actors and their attributes
actor_data <- data.table('name' = names(V(actor_network)), 
                         'degree' = degree(actor_network),
                         'closeness' = closeness(actor_network), 
                         'betweenness' = betweenness(actor_network),
                         'eccentricity' = eccentricity(actor_network))

#merge picture counts
setnames(actor_projects, 'actors', 'name')
setkey(actor_projects, name)
actor_data <- unique(merge(actor_data,
                           actor_projects[, !'title', with = F],
                           by = 'name', all.x = TRUE))

#repairs odd punctuation in actor names for joining and ensures UTF-8 encoding
actor_data[, name := gsub('[[:punct:]]| ', '', name)]
actor_data[, name := enc2utf8(name)]

#sorts for fast merging
setkey(actor_data, name)

#attach graph measures to individual film data
film_actor_list <- lapply(movie_with_critics_json_list, 
                          attach_actor_data, 
                          actor_data)

#write to json doc
lapply(1:length(film_actor_list), 
       write_list, 
       film_actor_list,
       'boxoffice_mojo_metacritic_merge.json')
