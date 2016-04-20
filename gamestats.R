

########This is for game summary###########

# only need to replace the gameID
# the final result is a 1*50 matrix. the first 25 columns are for the visiting team

gameID='0041500111'  #Replace this one to see another game 

boxscore=matrix(ncol=50)


scoreURL=paste("http://stats.nba.com/stats/boxscore/?GameID=",gameID,"&StartPeriod=0&EndPeriod=10&StartRange=0&EndRange=0&RangeType=0",sep="")
  
scoreData <- fromJSON(file = scoreURL, method="C")

value1=matrix()
value2=matrix()
value1=matrix(scoreData$resultSets[[6]]$rowSet[[1]],nrow=1)
value2=matrix(scoreData$resultSets[[6]]$rowSet[[2]],nrow=1)

boxscore=as.data.frame(cbind(value1,value2))

name1=scoreData$resultSets[[6]]$headers
name=c(name1,name1)  

names(boxscore)=name