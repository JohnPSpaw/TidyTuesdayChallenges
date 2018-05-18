library(readr)
library(stringr)
library(Amelia)
library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(spaceMovie)

detach("package:plyr", unload=TRUE)

starwars <- read_csv("~/Google Drive/Data Projects/TidyTuesday/Data/week7_starwars.csv")

#Rename columns
names(starwars)[1] <- "ID"
names(starwars)[2] <- "seenAny"
names(starwars)[3] <- "fan"
names(starwars)[4:9] <- paste0("seen", seq(1:6))
names(starwars)[10:15] <- paste0("rank", seq(1:6))
names(starwars)[16:29] <- paste0("favorability",starwars[1,16:29])
names(starwars)[30] <- "shotFirst"
names(starwars)[31] <- "familiarExpanded"
names(starwars)[32] <- "fanExpanded"
names(starwars)[33] <- "fanStarTrek"
names(starwars)[34] <- "gender"
names(starwars)[35] <- "age"
names(starwars)[36] <- "income"
names(starwars)[37] <- "education"
names(starwars)[38] <- "censusRegion"

#Remove spaces in column names
names(starwars) <- str_replace_all(names(starwars), fixed(" "), "")

#Remove first row - serves as a subtitle for some headers
starwars <- as.data.frame(starwars[-c(1),])

#Function: converting movie number to abbreviated movie title
#case_when is a dplyr solution for avoiding nested ifelse statements
#Input: Character
#Output: Character
num_to_movie <- function(x) {
  y <- case_when(
    x == "1" ~ "Phantom",
    x == "2" ~ "Clones",
    x == "3" ~ "Revenge",
    x == "4" ~ "Hope",
    x == "5" ~ "Empire",
    x == "6" ~ "Return"
  )
  return(y)
}
starwars[,10:15] <- starwars[,10:15] %>% 
  num_to_movie() %>% 
  as.factor()

#Much of the missingness ofccurs in the same rows (individuals)
#missmap(starwars) #from Amelia package
starwars <- starwars %>% 
  select(age, rank1, rank6) %>%
  filter(!is.na(rank1)) %>%
  filter(!is.na(age))

#Count rankings for each movie by age group
starwars_rankcount <- starwars %>% 
  group_by(age, rank1, rank6) %>%
  summarise(n = n()) %>%
  melt( id.vars = c("n", "age"))

names(starwars_rankcount) <- c("n", "Age", "Rank", "Title")

#Change columns to factors (except n)
cols <- names(starwars_rankcount)[2:4]
starwars_rankcount[,2:4] <- starwars_rankcount[,2:4]
starwars_rankcount <- starwars_rankcount %>% mutate_at(cols, funs(factor(.)))

#Reorder levels
starwars_rankcount$Age <- factor(starwars_rankcount$Age,
                                 levels(starwars_rankcount$Age)[c(2,3,4,1)])
starwars_rankcount$Title <- factor(starwars_rankcount$Title,
                                 levels(starwars_rankcount$Title)[c(4,1,6,3,2,5)])

starwars_rankcount %>%
  filter(Rank=="rank1") %>%
  ggplot(aes(Title, n)) +
  geom_bar(stat = "identity",
           position="stack",
           aes(fill = Age)) +
  theme_fivethirtyeight() +
  scale_fill_manual(values = SW_palette("TESB")[c(3,5,7,8)]) +
  ylab("Count") +
  xlab("Movie") +
  ggtitle("Favorite Star Wars Movie By Age") +
  theme(plot.title = element_text(size = 15, face = "bold"))

starwars_rankcount %>%
  filter(Rank=="rank6") %>%
  ggplot(aes(Title, n)) +
  geom_bar(stat = "identity",
           position="stack",
           aes(fill = Age)) +
  theme_fivethirtyeight() +
  scale_fill_manual(values = SW_palette("TESB")[c(3,5,7,8)]) +
  ylab("Count") +
  xlab("Movie") +
  ggtitle("Least Favorite Star Wars Movie By Age") +
  theme(plot.title = element_text(size = 15, face = "bold"))








