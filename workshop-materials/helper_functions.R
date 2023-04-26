library(htmltools)
library(glue)

genre_tags<-function(genres){
  
  #sub function to create a tag for a genre, colored by genre type
  genre_tag<- function(genre) {
    default_style<-"width:60px;font-size:12px;border-radius:5px;text-align:center;padding:3px 0px;"
    bg_color<-ifelse(genre=="Pop",'#D80495',ifelse(genre=="Hip-Hop", "#630585", ifelse(genre=="Rock", "#C40031", ifelse(genre=="Country","#F09732", ifelse(genre=="R&B", "#018FCC","#363636")))))
    
    tags$div(
      style = paste0(default_style,"background-color:",bg_color,";"),
      genre
    )
  }
  #create a list out of strings delimitted with commas, e.g. Pop, Rock
  genre_list <- strsplit(genres, ", ")
  #apply a genre_tag function to each item in list with lapply
  multi_tags<-lapply(genre_list[[1]], genre_tag)
  tag_list<-tagList(multi_tags)
  #add spacing between tags
  tags$div(tag_list, style="display:flex;gap:4px;")
}

image_label<-function(image_url, 
                      label,
                      circle = F,
                      height= 30,
                      border_color=NA, 
                      border_width=NA){
  
  if(circle){circle_style = 'border-radius:50%;object-fit:cover;'}
  else{circle_style=''}
  
  tagList(
    tags$img(src=image_url, 
    style=glue("display:inline-block;height:{height}px;width:{height}px;vertical-align:middle;border:{border_width}px solid {border_color};{circle_style};")),
    tags$div(label, style="display:inline-block;vertical-align:middle;padding-left:10px")
  )
}


get_flag<-function(iso_code){
  base = 'https://raw.githubusercontent.com/lipis/flag-icons/main/flags/1x1/'
  paste0(base,tolower(iso_code),".svg")
}
