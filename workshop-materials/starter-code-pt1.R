# IMPORT LIBRARIES
#to build our tables
library(reactablefmtr)
library(reactable)
#to add in HTML
library(htmltools)
library(htmlwidgets)
#for added data manipulation
library(tidyverse)
library(glue)
#save as png 
library(webshot2)

#install remotes::install_github("timelyportfolio/dataui")

#IMPORT HELPER FUNCTIONS - used for additional HTML/CSS styling
source("helper_functions.R")

#IMPORT DATA
artist_data<- read.csv("data/spotify-artists.csv")

#WRANGLE DATA


#STORE AESTHETICS IN VARIABLES - helps us stay consistent, easier to remember / edit


#Build Spotify Table


#Add Header & Caption with htmlwidgets + htmltools


#preview table output


#export as HTML widget


#export as png

