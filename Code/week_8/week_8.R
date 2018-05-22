#John Spaw
#TidyTuesday Challenge - Week 8
#Honey Production
#Article: http://www.beeculture.com/u-s-honey-industry-report-2016/

### Suggested Questions ###
#How has honey production yield changed from 1998 to 2012?
#Over time, which states produce the most honey? 
  #Which produce the least? 
  #Which have experienced the most change in honey yield?
#Does the data show any trends in terms of the number of honey producing colonies and yield per colony before 2006
  #which was when concern over Colony Collapse Disorder spread nationwide?
#Are there any patterns that can be observed between total honey production and value of production every year? 
  #How has value of production, which in some sense could be tied to demand, changed every year?

library(readr)
library(ggplot2)
library(openintro) #abb2state
library(magrittr)  
library(dplyr)
library(Amelia)
library(gganimate)

#Final processed dataset ... could also clean this by hand using separate files... for another time...
honey_production <- read_csv("Data/week8_honey_production/honeyproduction.csv")

#Convert state abbreviations to lowercase names
honey_production$state %<>% 
  abbr2state() %>% 
  tolower()

#Add production data to a map dataframe for plotting
production_map <- map_data("state")
production_map %<>% 
  full_join(honey_production, by = c("region" = "state"))

#Create US map of honey production by state
map <- ggplot(production_map, aes(long, lat)) +
  geom_polygon(aes(fill = totalprod,
                   frame = year)) +
  scale_fill_distiller(palette = "Spectral") +
  coord_map('mercator') +
  theme_void() +
  theme(legend.position="bottom", legend.box="horizontal",
        legend.key.width=grid::unit(.1,'npc')) +
  guides(fill = guide_colourbar(title.position="top",
                                title.hjust = 0.5))

gganimate(map, interval = 5)
#Create dynamic ggplot map of production over time






