############################################################
###TO DO:
  #Add year breweries were founded and do a gganimate on a map
  #Create map: SEE NOTES
############################################################

library(Amelia)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(fiftystater)


#Data load 
  #included supplementary breweries data from data.world
  #included supplementary cites data from simple.data
beers <- readxl::read_xlsx("../tidytuesday/data/week15_beers.xlsx")
breweries <- read.csv("Supplementary_Data/Week_15/breweries.csv")
cities <- read.csv("Supplementary_Data/Week_15/uscitiesv1.4.csv")

#Initial Format data frames
cities %<>%
  select("city", "state_id", "county_fips", "county_name", "lat", "lng") %>%
  rename(state = state_id) %>%
  mutate(city_state = paste0(city,", ", state)) %>%
  select(-c("city","state"))
  
breweries %<>%
  select(-c("X")) %>%
  rename(brewery = name)

beers %<>% 
  select(-c("count", "ibu", "id")) %>%
  rename(id = brewery_id, beer_name = name) %>%
  left_join(breweries) %>%
  mutate(city_state = paste0(city,",", state)) %>%
  left_join(cities, by = "city_state") %>%
  drop_na()

#Add aggregates to brewery table
bb <- beers %>% #aggregate beer by brewery
  group_by(brewery) %>%
  summarize(brewery_mean_abv = mean(abv),
            brewery_beer_count = n())
breweries %<>% left_join(bb)

#Add aggregates to state table
sb <- breweries %>% #agg breweries at state level
  group_by(state) %>%
  summarize(state_brewery_count = n())
state <- beers %>% 
  group_by(state) %>%
  summarize(state_mean_abv = mean(abv),
            state_mean_vol = mean(ounces),
            state_beer_count = n()) %>%
  arrange(state) %>%
  left_join(sb) %>%
  mutate(state = as.factor(state))


###ISSUE --- MAPS NOT SHOWING UP
#Create map
  #Color states by number of beers
  #Add dots by location with breweries
    #Size dots by the number of beers for a given brewery
states_map <- map_data("state")
state$state <- as.factor(substr(as.character(state$state),2,3))
state$state <- tolower(state.name[match(state$state, state.abb)])

ggplot() +
  geom_map(data = state,
           map = fifty_states,
           aes(map_id = state,
               fill = state_beer_count))


