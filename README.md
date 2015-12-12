# Film-EDA

This project was made for submission for P4: Explore and Summarize in the Udacity Data Analyst Nanodegree.

## Film Project Contents

* film_project.Rmd : the main markdown file
* film_project.html : the current draft of the knit html output
* eda_assist_functions : functions necessary for running the analysis in film_project.Rmd
* boxoffice_mojo_metacritic_merge.json : the dataset used in film_project.Rmd
* data_documentation.txt : description of data source and data type in boxoffice_mojo_metacritic_merge.json
* sources_consulted.txt : sites used as a reference while crafting film_project.Rmd

## Film Related Web Scraping

#### Run at your peril - the number of queries and site throttling makes this take a while

* data_assembly_script.R : control script for building boxoffice_mojo_metacritic_merge.json
* data_assembly_utilities.R : functions, scrapers and parsers (highly site specific) for data_assembly_script.R

