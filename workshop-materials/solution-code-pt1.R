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


#IMPORT HELPER FUNCTIONS - used for additional HTML/CSS styling
source("helper_functions.R")

#IMPORT DATA
artist_data<- read.csv("data/spotify-artists.csv")

#WRANGLE DATA
spotify_artists<-artist_data%>%
  #arrange artists by followers in descending order
  arrange(desc(followers))%>%
  #create rank with row_number
  mutate(rank = row_number())%>%
  #arrange the columns in this order using select
  select(rank, name, genres, followers, popularity, image_url)%>%
  #get the first 500 records
  head(500)


#STORE AESTHETICS IN VARIABLES - helps us stay consistent, easier to remember / edit
heatmap_pal<-rev(c("#64d591","#afe88d","#D6E888","#fce782","#fdf2c0","#fdfdfd"))
bg_color <- '#0B0B0A'
border_color<-'#2E2E2E'

#Build Spotify Table
spotify_table<-reactable(spotify_artists,
          searchable = TRUE,
          theme = reactableTheme(
            borderColor = border_color,
            backgroundColor= bg_color,
            #universal table style
            style=list(
              #fontFamily=font, 
                       backgroundColor=bg_color,
                       color="whitesmoke",
                       paddingLeft=20,
                       paddingRight=20,
                       paddingTop=20),
            #style search input
            searchInputStyle=list(
              backgroundColor=bg_color,
              borderColor=border_color
            )
          ),
          defaultColDef = colDef(vAlign="center"),
          columns = list(
            #you can hide columns in your data with show=F
            image_url = colDef(show=F),
            rank = colDef(name="Rank", align="center", width=80),
            #modify name to include a picture of the artist next to the name using custom HTML
            name = colDef(name = "Artist",
                          width=300,
                          cell = function(value, index){
                            #plug in custom function to combine image with artist name
                            image_url = spotify_artists$image_url[index]
                            image_label(image_url, value, circle=T, border_color="white", border_width=2)
                          }),
            #modify genres to create tags with custom HTML/CSS function
            genres = colDef(name="Genres", align="left", width=200,
                            #plug in custom HTML function to generate tags
                             cell = function(value){
                               genre_tags(value)
                             }),
            #apply reactablefmtr data bars to visualize followers
            followers = colDef(name="Followers",
                               align="left",
                               cell = data_bars(spotify_artists,
                                                fill_color = '#1DB954',
                                                text_position="above",
                                                bar_height=10,
                                                round_edges = TRUE,
                                                text_color = "white",
                                                background = NULL,
                                                text_size = 12,
                                                number_fmt = scales::label_number(scale_cut=scales::cut_short_scale()))),
            #add in reactablefmtr icon_sets to create heatmap effect by popualrity 
            popularity = colDef(name = "Popularity",
                                cell = icon_sets(spotify_artists, 
                                                 icons=c("star"),
                                                 colors=heatmap_pal),
                                footer = "Calculated by Spotify",
                                footerStyle = list(fontSize=11))
          )
          )%>%
  #add a default font to apply to table, browse font options - https://fonts.google.com/
  reactablefmtr::google_font(font_family="Heebo", font_weight=c(400,600))



#Add Header & Caption with htmlwidgets + htmltools
spotify_finished<-spotify_table%>%
  htmlwidgets::prependContent(
    tags$div(style = 'background-color:#0B0B0A;color:white;padding-left:30px;padding-top:5px;',
    tags$h1("Popular Artists on Spotify",style = "margin-bottom:0px;"),
    tags$span("Ranking based on follower counts on Spotify. Data as of April 2023.",
              style = 'color:#DDDDDD')
  )
  )%>%
  htmlwidgets::appendContent(
    tags$div("Source: Spotify API {spotifyR}", style = 'background-color:#0B0B0A;color:#DDDDDD;padding-left:30px;padding-bottom:20px;font-size:12px;',
)
  )

#preview table output
spotify_finished

#export as HTML widget
htmlwidgets::saveWidget(spotify_finished, "exported/spotify_table.html", selfcontained=TRUE)

#export as png
webshot(url="exported/spotify_table.html", file="exported/spotify_table.png")
