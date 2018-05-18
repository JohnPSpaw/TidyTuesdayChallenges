library(readr)
library(stringr)
library(Amelia)
library(dplyr)

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

#Remove spaces
names(starwars) <- str_replace_all(names(starwars), fixed(" "), "")

#Remove first row - serves as a subtitle for some headers
starwars <- as.data.frame(starwars[-c(1),])



#Much of the missingness ofccurs in the same rows (individuals)
missmap(starwars)
starwars <- subset(starwars, !is.na(starwars$rank1))

#reformat ranking variables (integer to title)
num_to_movie <- function(x) {
  x <- ifelse(x == "1", "Phantom",
              ifelse(x == "2", "Clones",
                     ifelse(x == "3", "Revenge",
                            ifelse(x == "4", "Hope",
                                   ifelse(x == "5", "Empire", "Return")))))
  return(x)
}
starwars[,10:16] <- starwars[,10:16] %>% num_to_movie()











