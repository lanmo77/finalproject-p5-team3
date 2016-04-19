setwd("C:/Users/mangmangyuzhou/Desktop/2016 Spring/data science/project 5")
load("game2011.Rdata")
load("teamdata.Rdata")
load("teamdata2.Rdata")
library(dplyr)

teamdata$test <- lapply(strsplit(as.character(teamdata$HOMEDESCRIPTION), "\\("), "[", 2)
teamdata$HOMEDESCRIPTION <- lapply(strsplit(as.character(teamdata$HOMEDESCRIPTION), "\\("), "[", 1)

teamdata$test2 <- lapply(strsplit(as.character(teamdata$VISITORDESCRIPTION), "\\("), "[", 2)
teamdata$VISITORDESCRIPTION <- lapply(strsplit(as.character(teamdata$VISITORDESCRIPTION), "\\("), "[", 1)

##good until now, trying to clean the format

JordanFT1=filter(teamdata, teamdata$"HOMEDESCRIPTION" == "Jordan Free Throw 1 of 2")
# Is not work
JordanFT1=filter(teamdata, teamdata$"HOMEDESCRIPTION" == "NULL")
# It works
