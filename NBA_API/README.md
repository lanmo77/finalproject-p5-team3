# Instruction
This R file is written for downloading data from NBA stats API. Before using the function, you need to install the following packages in R:
install.package(rjson)
install.package(RCurl)

library(rjson)
library(RCurl)


*sample* 
sample.list=('0021401226', '0021401203', '0021401195', '0021401179', '0021401164')


## Play by Play
### Function:   
playbyplay(gameID.list)
### Result: 
Return the play by play data within each game. 
### Example: 
playbyplay_data=playbyplay(gameID.list=sample.list)

*note: gameID should be character and do not miss out the '00' in the beginning*

## Player Stats
### Function:
playerstats(season, seasontype)
### Result:
return to the overall stats for each player in the specified season and season type
### Example:
PlayerStats_data=playerstats(season='2014-15',seasontype='Regular+Season')
  
*note: season should follow the format given in the example, season type can be chosen from 'Regular+Season' or 'Playoffs'*

## Team Stats
### Function:
teamstats(season, seasontype)
### Result:
return to the overall stats for each team in the given season and season type
### Example:
TeamStats_data=teamstats(season='2014-15',seasontype='Regular+Season')
  
*note: season should follow the format given in the example, season type can be chosen from 'Regular+Season' or 'Playoffs'*

## Data for Each Team
### Function:
teamdata(teamID, seasontype, season)
### Result:
Return to the summary stats for each game the given team played in the specified season and season type
### Example:
TeamData_data=teamdata(teamID='1610612765',seasontype = 'Regular+Season',season='2014-15')
  
*note: season should follow the format given in the example, season type can be chosen from 'Regular+Season' or 'Playoffs', TeamID should be character*

## Game Stats
### Function:
gamestats(gameID.list)
### Result:
Return to the stats of given game for both home team and visiting team
### Example:
GameStats_data=gamestats(gameID.list=sample.list)
*note: gameID should be character and do not miss out the '00' in the beginning*


## Visiting Home
### Function:
visitinghome(gameID.list)
### Result:
Return to the visiting team ID and home team ID of each given game
### Example:
visitinghome_data=visitinghome(gameID.list=sample.list)
  
*note: gameID should be character and do not miss out the '00' in the beginning*


## Shot Data for each player
### Function:
shotdata(playerID, season, seasontype)
### Result:
Return to the detailed information about each shot the given player made including location information
### Example:
shotdata_data=shotdata(playerID=201939, season='2014-15',seasontype='Regular+Season')
  
*note: season should follow the format given in the example, season type can be chosen from 'Regular+Season' or 'Playoffs', PlayerID should be character*



