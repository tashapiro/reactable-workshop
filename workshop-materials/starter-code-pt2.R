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