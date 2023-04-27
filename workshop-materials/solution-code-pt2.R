library(tidyverse)
library(reactable)
library(reactablefmtr)
library(glue)
library(htmltools)

#import helper functions
source("https://raw.githubusercontent.com/tashapiro/reactable-workshop/main/workshop-materials/helper_functions.R")

#import data files - total cases per million and monthly cases
per_million<-read_csv("https://raw.githubusercontent.com/tashapiro/reactable-workshop/main/workshop-materials/data/cases-per-million.csv")
monthly_cases<-read_csv("https://github.com/tashapiro/reactable-workshop/blob/main/workshop-materials/data/monthly-cases.csv")

#combine data sets and aggregate monthly cases per country
covid_data<-monthly_cases%>%
group_by(iso2, location)%>%
  summarise(total_cases = sum(cases),
            monthly_cases = list(cases))%>%
  left_join(per_million, by="location")%>%
  select(location, total_cases, total_cases_per_million, monthly_cases)%>%
  filter(total_cases>0)%>%
  arrange(desc(total_cases))

#define aesthetics for reactable
left_border<-cell_style(border_color = "#f2f2f2",border_style = "solid",border_width = "1px 0px 0px 2px")
right_border<-cell_style(border_color = "#f2f2f2",border_style = "solid",border_width = "1px 0px 2px 0px")
heatmap_pal<-c("#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59" ,"#E34A33" ,"#B30000")

#create reactable
reactable(
  covid_data,
  theme = reactableTheme(
    style=list(fontSize=14, padding="0px 0px 0px 0px"),
    groupHeaderStyle = list(fontWeight="bold")
  ),
  fullWidth=FALSE,
  defaultColDef = colDef(vAlign="center", width=140),
  columnGroups = list(colGroup(name="Cases", columns=c("total_cases","total_cases_per_million", "monthly_cases"))),
  columns = list(
    location = colDef(name="Country", 
                      sticky="left",
                      width=300,
                      style='background-color:#FCFCFC;',
                      cell = function(value, index){
      iso2 = data$iso2[index]
      image_link = get_flag(iso2)
      image_label(image_link, value, circle=T)
    }),
    iso2 = colDef(show=F),
    total_cases = colDef(name="Total", 
                         format = colFormat(separators=TRUE)),
    total_cases_per_million = colDef(name="Per Million", 
                                     format = colFormat(digits=0, separators=TRUE), 
                                     style=color_scales(covid_data, colors=heatmap_pal)),
    monthly_cases = colDef(name="Monthly", width=100, align="center", cell = react_sparkline(covid_data, line_color=heatmap_pal[4]))
  )
)%>%
  reactablefmtr::google_font(font_family="IBM Plex Sans", font_weight=c(400,600))%>%
  htmlwidgets::prependContent(
    tagList(tags$h1("COVID-19 Stats", style="margin-bottom:0px;"),
            tags$span("Data from OurWorldInData.org. Information by country as of April 2023.")
    )
  )

