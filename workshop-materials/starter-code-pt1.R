# IMPORT LIBRARIES
#to install packages you can either use the UI (Packages ->Install)
#or you can install with install.packages("packagename")
#note, one library is not on CRAN - remotes::install_github("timelyportfolio/dataui")

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


#IMPORT HELPER FUNCTIONS - used for additional HTML/CSS styling
source("https://raw.githubusercontent.com/tashapiro/reactable-workshop/main/workshop-materials/helper_functions.R")

#IMPORT DATA
artist_data<- read.csv("https://raw.githubusercontent.com/tashapiro/reactable-workshop/main/workshop-materials/data/spotify-artists.csv")

#WRANGLE DATA
#arrange artists by followers in descending order
#create rank with row_number
#arrange the columns in this order using select: rank, name, genres, followers, populatiy, image_url
#get the first 500 records

#STORE AESTHETICS IN VARIABLES - helps us stay consistent, easier to remember / edit
heatmap_pal<-rev(c("#64d591","#afe88d","#D6E888","#fce782","#fdf2c0","#fdfdfd"))
bg_color <- '#0B0B0A'
border_color<-'#2E2E2E'

#Build Spotify Table
#use reactable, reactablefmtr, and htmltools to customize table


#Add Header & Caption with htmlwidgets + htmltools


#preview table output


#export as HTML widget with htmlwidget


#export as png with webshot2

