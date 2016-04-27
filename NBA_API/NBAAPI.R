
so
gameID.list[1:5]
0021401226 0021401203 0021401195 0021401179 0021401164

#################################################################
#################################################################
# play by play 

  # variable needed: the list of game ID

playbyplay=function(gameID.list){

  teamdata=matrix(ncol = 12)
  for (i in 1:length(gameID.list)){
    gameID=gameID.list[i]
    
    
    playURL=paste('http://stats.nba.com/stats/playbyplay?StartPeriod=0&EndPeriod=10&GameID=',gameID,sep='')
    
    
    playData=fromJSON(file = playURL, method="C")
    
    
    playDataf=matrix(unlist(playData$resultSets[[1]]$rowSet,recursive = F), ncol=12, byrow = TRUE)
    
    teamdata=rbind(teamdata,playDataf)
    
    print(i)
  }
  name.player=playData$resultSets[[1]]$headers
  
  teamdata=data.frame(teamdata)
  names(teamdata)=name.player
  teamdata=teamdata[-1,]
  return(teamdata)
}


#################################################################
#################################################################
# player stats
    # variable needed: season format: '2014-15'
    #                  seasontype: 'Regular+Season' or 'Playoffs' 

playerstats=function(season='2014-15',seasontype='Regular+Season'){
  
  
  playerstatsURL=paste('http://stats.nba.com/stats/leaguedashplayerstats?GameScope=&PlayerExperience=&PlayerPosition=&StarterBench=&MeasureType=Base&PerMode=Totals&PlusMinus=N&PaceAdjust=N&Rank=Y&Season=',season,'&SeasonType=',seasontype,'&Outcome=&Location=&Month=0&SeasonSegment=&DateFrom=&DateTo&OpponentTeamID=0&VsConference=&VsDivision=&GameSegment=&Period=0&LastNGames=0',sep='')
  
  
  playerstatsData=fromJSON(file=playerstatsURL,method='C')
  
  name=playerstatsData$resultSets[[1]]$headers
  
  playerstatsData=data.frame(matrix(unlist(playerstatsData$resultSets[[1]]$rowSet), ncol=35, byrow = TRUE))
  
  names(playerstatsData)=name
  
  return(playerstatsData)
}

#################################################################
#################################################################
# team stats
# result include:
#"TEAM_ID"    "TEAM_NAME"  "GP"         "W"          "L"          "W_PCT"      "MIN"        "FGM"       
#"FGA"        "FG_PCT"     "FG3M"       "FG3A"       "FG3_PCT"    "FTM"        "FTA"        "FT_PCT"    
#"OREB"       "DREB"       "REB"        "AST"        "TOV"        "STL"        "BLK"        "BLKA"      
#"PF"         "PFD"        "PTS"        "PLUS_MINUS" "CFID"       "CFPARAMS"  
    # variable needed: season format: '2014-15'
    #                  seasontype: 'Regular+Season' or 'Playoffs' 

teamstats=function(season='2014-15',seasontype='Regular+Season'){
  teamstatsURL=paste('http://stats.nba.com/stats/leaguedashteamstats?MeasureType=Base&PerMode=Totals&PlusMinus=N&Rank=Y&Season=',season,'&SeasonType=',seasontype,'&Location=&Outcome=&SeasonSegment&DateFrom=&DateTo=&Period=0&GameSegment=&VsDivision=&LastNGames=100&Month=0&VsConference=&PaceAdjust=N&OpponentTeamID=0&',sep='')
  
  teamstatsData=fromJSON(file=teamstatsURL,method='C')
  
  
  name=teamstatsData$resultSets[[1]]$headers
  
  teamstatsData=data.frame(matrix(unlist(teamstatsData$resultSets[[1]]$rowSet), ncol=30, byrow = TRUE))
  names(teamstatsData)=name
  
  return(teamstatsData)
}


#################################################################
#################################################################
# stats for each team
teamdata=function(teamID,seasontype,season){
  teamURL=paste('http://stats.nba.com/stats/teamgamelog?TeamID=',teamID,'&SeasonType=',seasontype,'&Season=',season,sep='')
  
  
  teamData <- fromJSON(file = teamURL, method="C")
  
  #teamDataf=matrix(teamData$resultSets[[1]]$rowSet,ncol=24)
  name=teamData$resultSets[[1]]$headers
  
  teamDataf=data.frame(matrix(unlist(teamData$resultSets[[1]]$rowSet), ncol=24, byrow = TRUE))
  names(teamDataf)=name
  
  return(teamDataf)
}


#################################################################
#################################################################
# game stats

gamestats=function(gameID.list){
  data.score=matrix(ncol=51)
  for (i in 1:length(gameID.list)){
    
    
    gameID=gameID.list[i]
    scoreURL=paste("http://stats.nba.com/stats/boxscore/?GameID=",gameID,"&StartPeriod=0&EndPeriod=10&StartRange=0&EndRange=0&RangeType=0",sep="")
    
    scoreData <- fromJSON(file = scoreURL, method="C")
    value1=matrix()
    value2=matrix()
    value1=matrix(scoreData$resultSets[[6]]$rowSet[[1]],nrow=1)
    value2=matrix(scoreData$resultSets[[6]]$rowSet[[2]],nrow=1)
    time=scoreData$resultSets[[1]]$rowSet[[1]][[1]]
    value=cbind(time,value1,value2)
    data.score=rbind(data.score,value)
    print(i)
    
  }
  
  data.score=as.data.frame(data.score)
  name1=scoreData$resultSets[[6]]$headers
  name2='date'
  name=c(name2,name1,name1)
  names(data.score)=name
  
  data.score=data.score[-1,]
  

  
  return(data.score)
}


#################################################################
#################################################################
# Visiting Home

visitinghome=function(gameID.list){
  visitinghome=matrix(ncol=7)
  for (i in 1:length(gameID.list)){
    gameID=gameID.list[i]
    scoreURL=paste("http://stats.nba.com/stats/boxscore/?GameID=",gameID,"&StartPeriod=0&EndPeriod=10&StartRange=0&EndRange=0&RangeType=0",sep="")
    
    scoreData <- fromJSON(file = scoreURL, method="C")
    value=matrix(scoreData$resultSets[[3]]$rowSet[[1]],nrow=1)
    visitinghome=rbind(visitinghome,value)
    print(i)
  }
  
  visitinghome=as.data.frame(visitinghome)[-1,]
  name1=scoreData$resultSets[[3]]$headers
  
  names(visitinghome)=name1
  return(visitinghome)
}


#################################################################
#################################################################
# shot chart data for each player
# variable needed: season format: '2014-15'
#                  seasontype: 'Regular+Season' or 'Playoffs' 
#                  playerID: format: '201939'

shotdata=function(playerID,season,seasontype){
  
  shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",
                   playerID,'&PlusMinus=N&Position=&Rank=N&RookieYear=&Season',season,'&SeasonSegment=&SeasonType=',seasontype,'&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0', sep = "")
  
  # import from JSON
  shotData <- fromJSON(file = shotURL, method="C")
  
  # unlist shot data, save into a data frame
  shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=21, byrow = TRUE))
  
  # shot data headers
  colnames(shotDataf) <- shotData$resultSets[[1]][[2]]
  
  # covert x and y coordinates into numeric
  shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
  shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
  shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))
  
  return(shotDataf)
}

