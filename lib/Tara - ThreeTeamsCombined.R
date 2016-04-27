setwd("~/Google Drive/2016 Spring/Applied Data Science/Project 5/finalproject-p5-team3")
#setwd("/Users/bobminnich/Documents/Columbia/Courses/Applied_Data_Science/FinalProject/finalproject-p5-team3")

library(dplyr)
library(ggplot2)
library(lubridate)

# Processing games for all three teams
load("./data/teamdata.Rdata")
load("./data/teamdata2.Rdata")
load("./data/teamdata3.Rdata")

game1 <- teamdata # Clippers 1610612746 Jordan
game2 <- teamdata2 # Rockets 1610612745 Howard, Smith
game3 <- teamdata3 # Pistons 1610612765 Drummond




################################### CLIPPERS ###################################
# Remove duplicates
game1 <- game1[!duplicated(game1), ]
game1$WCTIMESTRING <- game1$NEUTRALDESCRIPTION <- NULL
game1$GAME_ID = unlist(as.factor(as.character(game1$GAME_ID)))
game1$EVENTNUM = unlist(as.numeric(game1$EVENTNUM))
game1$EVENTMSGTYPE = unlist(as.numeric(game1$EVENTMSGTYPE))
game1$EVENTMSGACTIONTYPE = unlist(as.numeric(game1$EVENTMSGACTIONTYPE))
game1$PERIOD = unlist(as.numeric(game1$PERIOD))
game1$HOMEDESCRIPTION = unlist(as.character(game1$HOMEDESCRIPTION))
game1$VISITORDESCRIPTION = unlist(as.character(game1$VISITORDESCRIPTION))
game1$SCORE = unlist(as.character(game1$SCORE))
game1$SCOREMARGIN = unlist(as.character(game1$SCOREMARGIN))

# Remove period 5 (overtime)
game1 <- game1[!(game1$PERIOD >= 5),]
## Add timestamps min:sec left AND sec into game
game1$MINSECLEFT <- ms(game1$PCTIMESTRING) + minutes((4 - game1$PERIOD) * 12)
game1$SECINTOGAME <- 2880 - period_to_seconds(game1$MINSECLEFT)

## Parse SCORES
for(i in 1:nrow(game1)) {
  if(game1$EVENTNUM[i] == 0 | game1$EVENTNUM[i] == 1)
    game1$SCORE[i] <- '0 - 0'
  else if(game1$SCORE[i] == 'NULL')
    game1$SCORE[i] <- game1$SCORE[i - 1]
  game1$VSCORE[i] <- as.numeric(unlist(strsplit(game1$SCORE[i], ' - '))[1])
  game1$HSCORE[i] <- as.numeric(unlist(strsplit(game1$SCORE[i], ' - '))[2])
}

## Fix SCOREMARGIN
for(i in 1:nrow(game1)) {
  if(game1$EVENTNUM[i] == 0 | game1$EVENTNUM[i] == 1 | game1$SCOREMARGIN[i] == 'TIE')
    game1$SCOREMARGIN[i] <- '0'
  else if(game1$SCOREMARGIN[i] == 'NULL')
    game1$SCOREMARGIN[i] <- game1$SCOREMARGIN[i - 1]
}
game1$SCOREMARGIN <- as.numeric(game1$SCOREMARGIN)

# Groups set 1 to 24 for 2 minute intervals
game1$GROUP <- ceiling(game1$SECINTOGAME / 120)
game1$GROUP <- ifelse(game1$GROUP == 0, 1, game1$GROUP)
# Remove last 2 minutes - cannot hack in last two minutes
game1 <- game1[!(game1$GROUP == 24),]
# Make unique identifier
game1$UNIQUE <- paste(as.character(game1$GAME_ID), as.character(game1$GROUP), sep = '-')

## Need to filter out team for when they are visitor and when they are home
load("./data/visitinghome.Rdata")
home_games <- select(filter(visitinghome, HOME_TEAM_ID == '1610612746'), GAME_ID, AGAINST = VISITOR_TEAM_ID)
home_games$STATUS <- 'HOME'
visitor_games <- select(filter(visitinghome, VISITOR_TEAM_ID == '1610612746'), GAME_ID, AGAINST = HOME_TEAM_ID)
visitor_games$STATUS <- 'VISITOR'
home_games <- merge(game1, home_games, by.x = 'GAME_ID', by.y = 'GAME_ID')
visitor_games <- merge(game1, visitor_games, by.x = 'GAME_ID', by.y = 'GAME_ID')

# Add some more columns to game data; flip score margins for games where team is visitor
game1$AGAINST <- ifelse(is.element(game1$GAME_ID, home_games$GAME_ID), home_games$AGAINST, visitor_games$AGAINST)
game1$STATUS <- ifelse(is.element(game1$GAME_ID, home_games$GAME_ID), home_games$STATUS, visitor_games$STATUS)
game1$SCOREMARGIN <- ifelse(game1$STATUS == 'VISITOR', game1$SCOREMARGIN * -1, game1$SCOREMARGIN)

# Filter by free throws by player ('Jordan') with low free throw %
freethrows <- rbind(filter(home_games, EVENTMSGTYPE == 3 & VISITORDESCRIPTION == 'NULL' 
                           & (grepl('Jordan', HOMEDESCRIPTION))), 
                    filter(visitor_games, EVENTMSGTYPE == 3 & HOMEDESCRIPTION == 'NULL' 
                           & (grepl('Jordan', VISITORDESCRIPTION))))
# Count free throw events only once & get count
freethrows <- freethrows[!duplicated(freethrows$PCTIMESTRING),] %>% count(UNIQUE)
game1$NUMFT <- unlist(freethrows[match(game1$UNIQUE, freethrows$UNIQUE), 'n'])
game1$NUMFT[is.na(game1$NUMFT)] <- 0
game1$HACKINT <- ifelse(game1$NUMFT == 0, FALSE, TRUE)
# Get 2 min score margins
SM <- game1 %>% group_by(UNIQUE) %>% summarize(Difference = last(SCOREMARGIN) - first(SCOREMARGIN), 
                                              HScore = last(HSCORE) - first(HSCORE), 
                                              VScore = last(VSCORE) - first(VSCORE))
game1$SM <- unlist(SM[match(game1$UNIQUE, SM$UNIQUE), 'Difference'])
game1$HSM <- unlist(SM[match(game1$UNIQUE, SM$UNIQUE), 'HScore'])
game1$VSM <- unlist(SM[match(game1$UNIQUE, SM$UNIQUE), 'VScore'])

## Make aggregate two minute interval data frame 
TWOMINS1 <- data.frame(UNIQUE = unique(game1$UNIQUE))
TWOMINS1$AGAINST <- game1[match(TWOMINS1$UNIQUE, game1$UNIQUE), 'AGAINST']
TWOMINS1$GAME_ID <- game1[match(TWOMINS1$UNIQUE, game1$UNIQUE), 'GAME_ID']
TWOMINS1$STATUS <- game1[match(TWOMINS1$UNIQUE, game1$UNIQUE), 'STATUS']
TWOMINS1$GROUP <- game1[match(TWOMINS1$UNIQUE, game1$UNIQUE), 'GROUP']
TWOMINS1$NUMFT <- game1[match(TWOMINS1$UNIQUE, game1$UNIQUE), 'NUMFT']
TWOMINS1$HACKINT <- ifelse(TWOMINS1$NUMFT == 0, FALSE, TRUE)
TWOMINS1$SM <- game1[match(TWOMINS1$UNIQUE, game1$UNIQUE), 'SM']
TWOMINS1$HSM <- game1[match(TWOMINS1$UNIQUE, game1$UNIQUE), 'HSM']
TWOMINS1$VSM <- game1[match(TWOMINS1$UNIQUE, game1$UNIQUE), 'VSM']
save(TWOMINS1, file = "./data/ClippersTwoMinIntervals.RData")




################################### ROCKETS ###################################
# Remove duplicates
game2 <- game2[!duplicated(game2), ]
game2$WCTIMESTRING <- game2$NEUTRALDESCRIPTION <- NULL
# NOTE: Comment this line out for dataframes without null first row
game2 <- game2[-1,]
game2$GAME_ID = unlist(as.factor(as.character(game2$GAME_ID)))
game2$EVENTNUM = unlist(as.numeric(game2$EVENTNUM))
game2$EVENTMSGTYPE = unlist(as.numeric(game2$EVENTMSGTYPE))
game2$EVENTMSGACTIONTYPE = unlist(as.numeric(game2$EVENTMSGACTIONTYPE))
game2$PERIOD = unlist(as.numeric(game2$PERIOD))
game2$HOMEDESCRIPTION = unlist(as.character(game2$HOMEDESCRIPTION))
game2$VISITORDESCRIPTION = unlist(as.character(game2$VISITORDESCRIPTION))
game2$SCORE = unlist(as.character(game2$SCORE))
game2$SCOREMARGIN = unlist(as.character(game2$SCOREMARGIN))

# Remove period 5 (overtime)
game2 <- game2[!(game2$PERIOD >= 5),]
## Add timestamps min:sec left AND sec into game
game2$MINSECLEFT <- ms(game2$PCTIMESTRING) + minutes((4 - game2$PERIOD) * 12)
game2$SECINTOGAME <- 2880 - period_to_seconds(game2$MINSECLEFT)

## Parse SCORES
for(i in 1:nrow(game2)) {
  if(game2$EVENTNUM[i] == 0 | game2$EVENTNUM[i] == 1)
    game2$SCORE[i] <- '0 - 0'
  else if(game2$SCORE[i] == 'NULL')
    game2$SCORE[i] <- game2$SCORE[i - 1]
  game2$VSCORE[i] <- as.numeric(unlist(strsplit(game2$SCORE[i], ' - '))[1])
  game2$HSCORE[i] <- as.numeric(unlist(strsplit(game2$SCORE[i], ' - '))[2])
}

## Fix SCOREMARGIN
for(i in 1:nrow(game2)) {
  if(game2$EVENTNUM[i] == 0 | game2$EVENTNUM[i] == 1 | game2$SCOREMARGIN[i] == 'TIE')
    game2$SCOREMARGIN[i] <- '0'
  else if(game2$SCOREMARGIN[i] == 'NULL')
    game2$SCOREMARGIN[i] <- game2$SCOREMARGIN[i - 1]
}
game2$SCOREMARGIN <- as.numeric(game2$SCOREMARGIN)

# Groups set 1 to 24 for 2 minute intervals
game2$GROUP <- ceiling(game2$SECINTOGAME / 120)
game2$GROUP <- ifelse(game2$GROUP == 0, 1, game2$GROUP)
# Remove last 2 minutes - cannot hack in last two minutes
game2 <- game2[!(game2$GROUP == 24),]
# Make unique identifier
game2$UNIQUE <- paste(as.character(game2$GAME_ID), as.character(game2$GROUP), sep = '-')

## Need to filter out team for when they are visitor and when they are home
load("./data/visitinghome.Rdata")
home_games <- select(filter(visitinghome, HOME_TEAM_ID == '1610612745'), GAME_ID, AGAINST = VISITOR_TEAM_ID)
home_games$STATUS <- 'HOME'
visitor_games <- select(filter(visitinghome, VISITOR_TEAM_ID == '1610612745'), GAME_ID, AGAINST = HOME_TEAM_ID)
visitor_games$STATUS <- 'VISITOR'
home_games <- merge(game2, home_games, by.x = 'GAME_ID', by.y = 'GAME_ID')
visitor_games <- merge(game2, visitor_games, by.x = 'GAME_ID', by.y = 'GAME_ID')

# Add some more columns to game data; flip score margins for games where team is visitor
game2$AGAINST <- ifelse(is.element(game2$GAME_ID, home_games$GAME_ID), home_games$AGAINST, visitor_games$AGAINST)
game2$STATUS <- ifelse(is.element(game2$GAME_ID, home_games$GAME_ID), home_games$STATUS, visitor_games$STATUS)
game2$SCOREMARGIN <- ifelse(game2$STATUS == 'VISITOR', game2$SCOREMARGIN * -1, game2$SCOREMARGIN)

# Filter by free throws by player ('Jordan') with low free throw %
freethrows <- rbind(filter(home_games, EVENTMSGTYPE == 3 & VISITORDESCRIPTION == 'NULL' 
                           & (grepl('Howard', HOMEDESCRIPTION) | grepl('Smith', HOMEDESCRIPTION))), 
                    filter(visitor_games, EVENTMSGTYPE == 3 & HOMEDESCRIPTION == 'NULL' 
                           & (grepl('Howard', VISITORDESCRIPTION) | grepl('Smith', VISITORDESCRIPTION))))
# Count free throw events only once & get count
freethrows <- freethrows[!duplicated(freethrows$PCTIMESTRING),] %>% count(UNIQUE)
game2$NUMFT <- unlist(freethrows[match(game2$UNIQUE, freethrows$UNIQUE), 'n'])
game2$NUMFT[is.na(game2$NUMFT)] <- 0
game2$HACKINT <- ifelse(game2$NUMFT == 0, FALSE, TRUE)
# Get 2 min score margins
SM <- game2 %>% group_by(UNIQUE) %>% summarize(Difference = last(SCOREMARGIN) - first(SCOREMARGIN), 
                                               HScore = last(HSCORE) - first(HSCORE), 
                                               VScore = last(VSCORE) - first(VSCORE))
game2$SM <- unlist(SM[match(game2$UNIQUE, SM$UNIQUE), 'Difference'])
game2$HSM <- unlist(SM[match(game2$UNIQUE, SM$UNIQUE), 'HScore'])
game2$VSM <- unlist(SM[match(game2$UNIQUE, SM$UNIQUE), 'VScore'])

## Make aggregate two minute interval data frame 
TWOMINS2 <- data.frame(UNIQUE = unique(game2$UNIQUE))
TWOMINS2$AGAINST <- game2[match(TWOMINS2$UNIQUE, game2$UNIQUE), 'AGAINST']
TWOMINS2$GAME_ID <- game2[match(TWOMINS2$UNIQUE, game2$UNIQUE), 'GAME_ID']
TWOMINS2$STATUS <- game2[match(TWOMINS2$UNIQUE, game2$UNIQUE), 'STATUS']
TWOMINS2$GROUP <- game2[match(TWOMINS2$UNIQUE, game2$UNIQUE), 'GROUP']
TWOMINS2$NUMFT <- game2[match(TWOMINS2$UNIQUE, game2$UNIQUE), 'NUMFT']
TWOMINS2$HACKINT <- ifelse(TWOMINS2$NUMFT == 0, FALSE, TRUE)
TWOMINS2$SM <- game2[match(TWOMINS2$UNIQUE, game2$UNIQUE), 'SM']
TWOMINS2$HSM <- game2[match(TWOMINS2$UNIQUE, game2$UNIQUE), 'HSM']
TWOMINS2$VSM <- game2[match(TWOMINS2$UNIQUE, game2$UNIQUE), 'VSM']
save(TWOMINS2, file = "./data/RocketsTwoMinIntervals.RData")




################################### PISTONS ###################################
# Remove duplicates
game3 <- game3[!duplicated(game3), ]
game3$WCTIMESTRING <- game3$NEUTRALDESCRIPTION <- NULL
# NOTE: Comment this line out for dataframes without null first row
game3 <- game3[-1,]
game3$GAME_ID = unlist(as.factor(as.character(game3$GAME_ID)))
game3$EVENTNUM = unlist(as.numeric(game3$EVENTNUM))
game3$EVENTMSGTYPE = unlist(as.numeric(game3$EVENTMSGTYPE))
game3$EVENTMSGACTIONTYPE = unlist(as.numeric(game3$EVENTMSGACTIONTYPE))
game3$PERIOD = unlist(as.numeric(game3$PERIOD))
game3$HOMEDESCRIPTION = unlist(as.character(game3$HOMEDESCRIPTION))
game3$VISITORDESCRIPTION = unlist(as.character(game3$VISITORDESCRIPTION))
game3$SCORE = unlist(as.character(game3$SCORE))
game3$SCOREMARGIN = unlist(as.character(game3$SCOREMARGIN))

# Remove period 5 (overtime)
game3 <- game3[!(game3$PERIOD >= 5),]
## Add timestamps min:sec left AND sec into game
game3$MINSECLEFT <- ms(game3$PCTIMESTRING) + minutes((4 - game3$PERIOD) * 12)
game3$SECINTOGAME <- 2880 - period_to_seconds(game3$MINSECLEFT)

## Parse SCORES
for(i in 1:nrow(game3)) {
  if(game3$EVENTNUM[i] == 0 | game3$EVENTNUM[i] == 1)
    game3$SCORE[i] <- '0 - 0'
  else if(game3$SCORE[i] == 'NULL')
    game3$SCORE[i] <- game3$SCORE[i - 1]
  game3$VSCORE[i] <- as.numeric(unlist(strsplit(game3$SCORE[i], ' - '))[1])
  game3$HSCORE[i] <- as.numeric(unlist(strsplit(game3$SCORE[i], ' - '))[2])
}

## Fix SCOREMARGIN
for(i in 1:nrow(game3)) {
  if(game3$EVENTNUM[i] == 0 | game3$EVENTNUM[i] == 1 | game3$SCOREMARGIN[i] == 'TIE')
    game3$SCOREMARGIN[i] <- '0'
  else if(game3$SCOREMARGIN[i] == 'NULL')
    game3$SCOREMARGIN[i] <- game3$SCOREMARGIN[i - 1]
}
game3$SCOREMARGIN <- as.numeric(game3$SCOREMARGIN)

# Groups set 1 to 24 for 2 minute intervals
game3$GROUP <- ceiling(game3$SECINTOGAME / 120)
game3$GROUP <- ifelse(game3$GROUP == 0, 1, game3$GROUP)
# Remove last 2 minutes - cannot hack in last two minutes
game3 <- game3[!(game3$GROUP == 24),]
# Make unique identifier
game3$UNIQUE <- paste(as.character(game3$GAME_ID), as.character(game3$GROUP), sep = '-')

## Need to filter out team for when they are visitor and when they are home
load("./data/visitinghome.Rdata")
home_games <- select(filter(visitinghome, HOME_TEAM_ID == '1610612765'), GAME_ID, AGAINST = VISITOR_TEAM_ID)
home_games$STATUS <- 'HOME'
visitor_games <- select(filter(visitinghome, VISITOR_TEAM_ID == '1610612765'), GAME_ID, AGAINST = HOME_TEAM_ID)
visitor_games$STATUS <- 'VISITOR'
home_games <- merge(game3, home_games, by.x = 'GAME_ID', by.y = 'GAME_ID')
visitor_games <- merge(game3, visitor_games, by.x = 'GAME_ID', by.y = 'GAME_ID')

# Add some more columns to game data; flip score margins for games where team is visitor
game3$AGAINST <- ifelse(is.element(game3$GAME_ID, home_games$GAME_ID), home_games$AGAINST, visitor_games$AGAINST)
game3$STATUS <- ifelse(is.element(game3$GAME_ID, home_games$GAME_ID), home_games$STATUS, visitor_games$STATUS)
game3$SCOREMARGIN <- ifelse(game3$STATUS == 'VISITOR', game3$SCOREMARGIN * -1, game3$SCOREMARGIN)

# Filter by free throws by player ('Jordan') with low free throw %
freethrows <- rbind(filter(home_games, EVENTMSGTYPE == 3 & VISITORDESCRIPTION == 'NULL' 
                           & (grepl('Drummond', HOMEDESCRIPTION))), 
                    filter(visitor_games, EVENTMSGTYPE == 3 & HOMEDESCRIPTION == 'NULL' 
                           & (grepl('Drummond', VISITORDESCRIPTION))))
# Count free throw events only once & get count
freethrows <- freethrows[!duplicated(freethrows$PCTIMESTRING),] %>% count(UNIQUE)
game3$NUMFT <- unlist(freethrows[match(game3$UNIQUE, freethrows$UNIQUE), 'n'])
game3$NUMFT[is.na(game3$NUMFT)] <- 0
game3$HACKINT <- ifelse(game3$NUMFT == 0, FALSE, TRUE)
# Get 2 min score margins
SM <- game3 %>% group_by(UNIQUE) %>% summarize(Difference = last(SCOREMARGIN) - first(SCOREMARGIN), 
                                               HScore = last(HSCORE) - first(HSCORE), 
                                               VScore = last(VSCORE) - first(VSCORE))
game3$SM <- unlist(SM[match(game3$UNIQUE, SM$UNIQUE), 'Difference'])
game3$HSM <- unlist(SM[match(game3$UNIQUE, SM$UNIQUE), 'HScore'])
game3$VSM <- unlist(SM[match(game3$UNIQUE, SM$UNIQUE), 'VScore'])

## Make aggregate two minute interval data frame 
TWOMINS3 <- data.frame(UNIQUE = unique(game3$UNIQUE))
TWOMINS3$AGAINST <- game3[match(TWOMINS3$UNIQUE, game3$UNIQUE), 'AGAINST']
TWOMINS3$GAME_ID <- game3[match(TWOMINS3$UNIQUE, game3$UNIQUE), 'GAME_ID']
TWOMINS3$STATUS <- game3[match(TWOMINS3$UNIQUE, game3$UNIQUE), 'STATUS']
TWOMINS3$GROUP <- game3[match(TWOMINS3$UNIQUE, game3$UNIQUE), 'GROUP']
TWOMINS3$NUMFT <- game3[match(TWOMINS3$UNIQUE, game3$UNIQUE), 'NUMFT']
TWOMINS3$HACKINT <- ifelse(TWOMINS3$NUMFT == 0, FALSE, TRUE)
TWOMINS3$SM <- game3[match(TWOMINS3$UNIQUE, game3$UNIQUE), 'SM']
TWOMINS3$HSM <- game3[match(TWOMINS3$UNIQUE, game3$UNIQUE), 'HSM']
TWOMINS3$VSM <- game3[match(TWOMINS3$UNIQUE, game3$UNIQUE), 'VSM']
save(TWOMINS3, file = "./data/PistonsTwoMinIntervals.RData")




################################### GRAPHS ###################################
ggplot(TWOMINS1, aes(SM, fill = HACKINT, group = HACKINT)) + 
  geom_histogram(binwidth = 1, position = 'identity') +
  labs(title = 'Clippers 2014 - 2015 (Jordan)', x = 'Two Minute Score Margin', y = 'Frequency')

ggplot(TWOMINS1, aes(HACKINT, SM)) + 
  geom_boxplot(aes(fill = factor(HACKINT))) + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs(title = 'Clippers 2014 - 2015 (Jordan)', x = 'Are they getting hacked?', y = 'Two Minute Score Margin')

summary(subset(TWOMINS1, HACKINT == TRUE)$SM)
summary(subset(TWOMINS1, HACKINT == FALSE)$SM)

###################################

ggplot(TWOMINS2, aes(SM, fill = HACKINT, group = HACKINT)) + 
  geom_histogram(binwidth = 1, position = 'identity') +
  labs(title = 'Rockets 2014 - 2015 (Howard and Smith)', x = 'Two Minute Score Margin', y = 'Frequency')

ggplot(TWOMINS2, aes(HACKINT, SM)) + 
  geom_boxplot(aes(fill = factor(HACKINT))) + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs(title = 'Rockets 2014 - 2015 (Howard and Smith)', x = 'Are they getting hacked?', y = 'Two Minute Score Margin')

summary(subset(TWOMINS2, HACKINT == TRUE)$SM)
summary(subset(TWOMINS2, HACKINT == FALSE)$SM)

###################################

ggplot(TWOMINS3, aes(SM, fill = HACKINT, group = HACKINT)) + 
  geom_histogram(binwidth = 1, position = 'identity') +
  labs(title = 'Pistons 2014 - 2015 (Drummond)', x = 'Two Minute Score Margin', y = 'Frequency')

ggplot(TWOMINS3, aes(HACKINT, SM)) + 
  geom_boxplot(aes(fill = factor(HACKINT))) + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs(title = 'Pistons 2014 - 2015 (Drummond)', x = 'Are they getting hacked?', y = 'Two Minute Score Margin')

summary(subset(TWOMINS3, HACKINT == TRUE)$SM)
summary(subset(TWOMINS3, HACKINT == FALSE)$SM)
