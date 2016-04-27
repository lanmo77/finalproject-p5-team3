setwd("~/Google Drive/2016 Spring/Applied Data Science/Project 5/finalproject-p5-team3Tara")
library(dplyr)
library(ggplot2)
library(lubridate)

# Processing all games for a team
load("./data/teamdata3.Rdata")

game <- teamdata3
# Remove duplicates
game <- game[!duplicated(game), ]
game$WCTIMESTRING <- game$NEUTRALDESCRIPTION <- NULL
# NOTE: Comment this line out for dataframes without null first row
game <- game[-1,]
game$GAME_ID = unlist(as.factor(as.character(game$GAME_ID)))
game$EVENTNUM = unlist(as.numeric(game$EVENTNUM))
game$EVENTMSGTYPE = unlist(as.numeric(game$EVENTMSGTYPE))
game$EVENTMSGACTIONTYPE = unlist(as.numeric(game$EVENTMSGACTIONTYPE))
game$PERIOD = unlist(as.numeric(game$PERIOD))
game$HOMEDESCRIPTION = unlist(as.character(game$HOMEDESCRIPTION))
game$VISITORDESCRIPTION = unlist(as.character(game$VISITORDESCRIPTION))
game$SCORE = unlist(as.character(game$SCORE))
game$SCOREMARGIN = unlist(as.character(game$SCOREMARGIN))

## Add timestamps min:sec left AND sec into game
game$MINSECLEFT <- ms(game$PCTIMESTRING) + minutes((4 - game$PERIOD) * 12)
game$SECINTOGAME <- 2880 - period_to_seconds(game$MINSECLEFT)

## Parse SCORES
for(i in 1:nrow(game)) {
  if(game$EVENTNUM[i] == 0 | game$EVENTNUM[i] == 1)
    game$SCORE[i] <- '0 - 0'
  else if(game$SCORE[i] == 'NULL')
    game$SCORE[i] <- game$SCORE[i - 1]
  game$VSCORE[i] <- as.numeric(unlist(strsplit(game$SCORE[i], ' - '))[1])
  game$HSCORE[i] <- as.numeric(unlist(strsplit(game$SCORE[i], ' - '))[2])
}

## Fix SCOREMARGIN
for(i in 1:nrow(game)) {
  if(game$EVENTNUM[i] == 0 | game$EVENTNUM[i] == 1 | game$SCOREMARGIN[i] == 'TIE')
    game$SCOREMARGIN[i] <- '0'
  else if(game$SCOREMARGIN[i] == 'NULL')
    game$SCOREMARGIN[i] <- game$SCOREMARGIN[i - 1]
}
game$SCOREMARGIN <- as.numeric(game$SCOREMARGIN)

# Groups set 1 to 24 for 2 minute intervals
game$GROUP <- ceiling(game$SECINTOGAME / 120)
game$GROUP <- ifelse(game$GROUP == 0, 1, game$GROUP)
# Make unique identifier
game$UNIQUE <- paste(as.character(game$GAME_ID), as.character(game$GROUP), sep = '-')

## Need to filter out team for when they are visitor and when they are home
load("./data/visitinghome.Rdata")
home_games <- select(filter(visitinghome, HOME_TEAM_ID == '1610612765'), GAME_ID, AGAINST = VISITOR_TEAM_ID)
home_games$STATUS <- 'HOME'
visitor_games <- select(filter(visitinghome, VISITOR_TEAM_ID == '1610612765'), GAME_ID, AGAINST = HOME_TEAM_ID)
visitor_games$STATUS <- 'VISITOR'
home_games <- merge(game, home_games, by.x = 'GAME_ID', by.y = 'GAME_ID')
visitor_games <- merge(game, visitor_games, by.x = 'GAME_ID', by.y = 'GAME_ID')

# Add some more columns to game data; flip score margins for games where team is visitor
game$AGAINST <- ifelse(is.element(game$GAME_ID, home_games$GAME_ID), home_games$AGAINST, visitor_games$AGAINST)
game$STATUS <- ifelse(is.element(game$GAME_ID, home_games$GAME_ID), home_games$STATUS, visitor_games$STATUS)
game$SCOREMARGIN <- ifelse(game$STATUS == 'VISITOR', game$SCOREMARGIN * -1, game$SCOREMARGIN)

# Filter by free throws by player ('Jordan') with low free throw %
freethrows <- rbind(filter(home_games, EVENTMSGTYPE == 3 & VISITORDESCRIPTION == 'NULL' 
                           & (grepl('Drummond', HOMEDESCRIPTION))), 
                    filter(visitor_games, EVENTMSGTYPE == 3 & HOMEDESCRIPTION == 'NULL' 
                           & (grepl('Drummond', VISITORDESCRIPTION))))
# Count free throw events only once & get count
freethrows <- freethrows[!duplicated(freethrows$PCTIMESTRING),] %>% count(UNIQUE)
game$NUMFT <- unlist(freethrows[match(game$UNIQUE, freethrows$UNIQUE), 'n'])
game$NUMFT[is.na(game$NUMFT)] <- 0
game$HACKINT <- ifelse(game$NUMFT == 0, FALSE, TRUE)
# Get 2 min score margins
SM <- game %>% group_by(UNIQUE) %>% summarize(Difference = last(SCOREMARGIN) - first(SCOREMARGIN), 
                                              HScore = last(HSCORE) - first(HSCORE), 
                                              VScore = last(VSCORE) - first(VSCORE))
game$SM <- unlist(SM[match(game$UNIQUE, SM$UNIQUE), 'Difference'])
game$HSM <- unlist(SM[match(game$UNIQUE, SM$UNIQUE), 'HScore'])
game$VSM <- unlist(SM[match(game$UNIQUE, SM$UNIQUE), 'VScore'])

## Make aggregate two minute interval data frame 
TWOMINS <- data.frame(UNIQUE = unique(game$UNIQUE))
TWOMINS$AGAINST <- game[match(TWOMINS$UNIQUE, game$UNIQUE), 'AGAINST']
TWOMINS$GAME_ID <- game[match(TWOMINS$UNIQUE, game$UNIQUE), 'GAME_ID']
TWOMINS$STATUS <- game[match(TWOMINS$UNIQUE, game$UNIQUE), 'STATUS']
TWOMINS$GROUP <- game[match(TWOMINS$UNIQUE, game$UNIQUE), 'GROUP']
TWOMINS$NUMFT <- game[match(TWOMINS$UNIQUE, game$UNIQUE), 'NUMFT']
TWOMINS$HACKINT <- ifelse(TWOMINS$NUMFT == 0, FALSE, TRUE)
TWOMINS$SM <- game[match(TWOMINS$UNIQUE, game$UNIQUE), 'SM']
TWOMINS$HSM <- game[match(TWOMINS$UNIQUE, game$UNIQUE), 'HSM']
TWOMINS$VSM <- game[match(TWOMINS$UNIQUE, game$UNIQUE), 'VSM']
save(TWOMINS, file = "./data/PistonsTwoMinIntervals.RData")

ggplot(TWOMINS, aes(SM, fill = HACKINT, group = HACKINT)) + 
  geom_histogram(binwidth = 1, position = 'identity') +
  labs(title = 'Pistons 2014 - 2015 (Drummond)', x = 'Two Minute Score Margin', y = 'Frequency')

ggplot(TWOMINS, aes(HACKINT, SM)) + 
  geom_boxplot(aes(fill = factor(HACKINT))) + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs(title = 'Pistons 2014 - 2015 (Drummond)', x = 'Are they getting hacked?', y = 'Two Minute Score Margin')

summary(subset(TWOMINS, HACKINT == TRUE)$SM)
summary(subset(TWOMINS, HACKINT == FALSE)$SM)






'''
## Compute number of fouls ON the team in every TWO minute interval
# For all fouls: "EVENTMSGTYPE == 6"; for personal take fouls: "EVENTMSGACTIONTYPE == 28"
# Filter for where visitor fouls on home in home games and home fouls on visitor in visitor games
counts <- rbind(filter(home_games, EVENTMSGACTIONTYPE == 28 & VISITORDESCRIPTION != 'NULL') %>% count(UNIQUE), 
filter(visitor_games, EVENTMSGACTIONTYPE == 28 & HOMEDESCRIPTION != 'NULL') %>% count(UNIQUE))
game$NUMFOULS <- unlist(counts[match(game$UNIQUE, counts$UNIQUE), "n"])
game$NUMFOULS[is.na(game$NUMFOULS)] <- 0

temp <- FOULS %>% group_by(GAME_ID) %>% summarise('75'=quantile(COUNT, 0.90))
FOULS$CUTOFF <- temp[match(FOULS$GAME_ID, temp$GAME_ID), '75']

FOULS$PERCENTILE <- ecdf(FOULS$COUNT)(FOULS$COUNT)
game$HAS <- FOULS[match(game$UNIQUE, FOULS$UNIQUE), "HAS"]
game$FOULPERCENTILE <- FOULS[match(game$UNIQUE, FOULS$UNIQUE), "PERCENTILE"]
'''