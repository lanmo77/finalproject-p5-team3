Tracking the pace of using Hack team and the team be Hacked
```{r, echo=FALSE, results="hide", warning=FALSE, message=FALSE}
setwd("C:/Users/mangmangyuzhou/Desktop/2016 Spring/data science/project 5")
library(dplyr)
library(ggplot2)
library(lubridate)
# load("teamdata.Rdata")
# load("teamdata2.Rdata")
# load("teamdata3.Rdata")
# teamdata = process(teamdata)
# teamdata2 = process(teamdata2)
# teamdata3 = process(teamdata3)
# 
# teamdata$MINSECLEFT <- ms(teamdata$PCTIMESTRING) + minutes((4 - teamdata$PERIOD) * 12)
# teamdata$SECINTOGAME <- 2880 - period_to_seconds(teamdata$MINSECLEFT)
# teamdata$min2r <- ceiling(teamdata$SECINTOGAME / 120)
# 
# teamdata2$MINSECLEFT <- ms(teamdata2$PCTIMESTRING) + minutes((4 - teamdata2$PERIOD) * 12)
# teamdata2$SECINTOGAME <- 2880 - period_to_seconds(teamdata2$MINSECLEFT)
# teamdata2$min2r <- ceiling(teamdata2$SECINTOGAME / 120)
# 
# teamdata3$MINSECLEFT <- ms(teamdata3$PCTIMESTRING) + minutes((4 - teamdata3$PERIOD) * 12)
# teamdata3$SECINTOGAME <- 2880 - period_to_seconds(teamdata3$MINSECLEFT)
# teamdata3$min2r <- ceiling(teamdata3$SECINTOGAME / 120)
# 
# saveRDS(teamdata, "teamdata.RDS")
# saveRDS(teamdata2, "teamdata2.RDS")
# saveRDS(teamdata3, "teamdata3.RDS")
teamdata=readRDS("teamdata.RDS")
teamdata2=readRDS("teamdata2.RDS")
teamdata3=readRDS("teamdata3.RDS")
load("ClippersTwoMinIntervals.Rdata")
CHACKRAW=TWOMINS
load("RocketsTwoMinIntervals.Rdata")
HHACKRAW=TWOMINS
load("PistonsTwoMinIntervals.Rdata")
PHACKRAW=TWOMINS
CHACK=filter(CHACKRAW, NUMFT>0)
HHACK=filter(HHACKRAW, NUMFT>0)
PHACK=filter(PHACKRAW, NUMFT>0)
#Cdf_gameh

# Cdf_gameh$HACK = FALSE
# 
# CHACK$GAME_ID = as.character(CHACK$GAME_ID)
# Cdf_gameh$GAME_ID = as.character(Cdf_gameh$GAME_ID)
# 
# test_df = left_join(Cdf_gameh,CHACK,by = c("GAME_ID"= "GAME_ID","min2r" = "GROUP"))

# for(i in 1:nrow(CHACKRAW)){
#   for(j in 1:nrow(Cdf_gameh))
#     if(as.character(CHACKRAW$GAME_ID[i]) == as.character(Cdf_gameh$GAME_ID[j]) & CHACKRAW$GROUP[i] == Cdf_gameh$min2r[j]){
#       Cdf_gameh$HACK[j] = CHACKRAW$HACKINT[i]
#     }
# }

maximin = function(hscore){
  return (max(hscore) - min(hscore))
}
load("visitinghome.Rdata")
C_V=filter(visitinghome, visitinghome$"VISITOR_TEAM_ID" == "1610612746")
C_H=filter(visitinghome, visitinghome$"HOME_TEAM_ID" == "1610612746")
teamdata_v = filter(teamdata, teamdata$"GAME_ID" %in% C_V$GAME_ID)
teamdata_h = filter(teamdata, teamdata$"GAME_ID" %in% C_H$GAME_ID)

#Clippers  hack game 2 min interval
CHACK_v = filter(CHACK, CHACK$"GAME_ID" %in% C_V$GAME_ID)
CHACK_h = filter(CHACK, CHACK$"GAME_ID" %in% C_H$GAME_ID)


HOU_V=filter(visitinghome, visitinghome$"VISITOR_TEAM_ID" == "1610612745")
HOU_H=filter(visitinghome, visitinghome$"HOME_TEAM_ID" == "1610612745")
HOUteamdata_v = filter(teamdata2, teamdata2$"GAME_ID" %in% HOU_V$GAME_ID)
HOUteamdata_h = filter(teamdata2, teamdata2$"GAME_ID" %in% HOU_H$GAME_ID)

#Rockets hack game 2 min interval
HHACK_v = filter(HHACK, HHACK$"GAME_ID" %in% HOU_V$GAME_ID)
HHACK_h = filter(HHACK, HHACK$"GAME_ID" %in% HOU_H$GAME_ID)



DET_V=filter(visitinghome, visitinghome$"VISITOR_TEAM_ID" == "1610612765")
DET_H=filter(visitinghome, visitinghome$"HOME_TEAM_ID" == "1610612765")
DETteamdata_v = filter(teamdata3, teamdata3$"GAME_ID" %in% DET_V$GAME_ID)
DETteamdata_h = filter(teamdata3, teamdata3$"GAME_ID" %in% DET_H$GAME_ID)

#Pistons hack game 2 min interval
PHACK_v = filter(PHACK, PHACK$"GAME_ID" %in% DET_V$GAME_ID)
PHACK_h = filter(PHACK, PHACK$"GAME_ID" %in% DET_H$GAME_ID)


#HOU:1610612745
#DET:1610612765

#Clippers
Cdf_gameh = filter(teamdata_h, hscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_h =maximin(vscore) )
Cdf_gamev = filter(teamdata_v, vscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_v =maximin(hscore) )
Cdf_gameh = filter(Cdf_gameh, min2r < 25)
Cdf_gamev = filter(Cdf_gamev, min2r < 25)

Odf_gamev = filter(teamdata_h, hscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_v =maximin(hscore) )
Odf_gameh = filter(teamdata_v, vscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_h =maximin(vscore) )
Odf_gamev = filter(Odf_gamev, min2r < 25)
Odf_gameh = filter(Odf_gameh, min2r < 25)

#Houston
Hdf_gameh = filter(HOUteamdata_h, hscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_h =maximin(vscore) )
Hdf_gamev = filter(HOUteamdata_v, vscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_v =maximin(hscore) )
Hdf_gameh = filter(Hdf_gameh, min2r < 25)
Hdf_gamev = filter(Hdf_gamev, min2r < 25)

HOdf_gamev = filter(HOUteamdata_h, hscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_v =maximin(hscore))
HOdf_gameh = filter(HOUteamdata_v, vscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_h =maximin(vscore))
HOdf_gamev = filter(HOdf_gamev, min2r < 25)
HOdf_gameh = filter(HOdf_gameh, min2r < 25)


#DET
Ddf_gameh = filter(DETteamdata_h, hscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_h =maximin(vscore) )
Ddf_gamev = filter(DETteamdata_v, vscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_v =maximin(hscore) )
Ddf_gameh = filter(Ddf_gameh, min2r < 25)
Ddf_gamev = filter(Ddf_gamev, min2r < 25)

DOdf_gamev = filter(DETteamdata_h, hscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_v =maximin(hscore))
DOdf_gameh = filter(DETteamdata_v, vscore!="NULL")%>%group_by(GAME_ID,min2r)%>% summarise(diff_h =maximin(vscore))
DOdf_gamev = filter(DOdf_gamev, min2r < 25)
DOdf_gameh = filter(DOdf_gameh, min2r < 25)



#measure the Pace in Hack-Shaq period by comparing socring per min with Avg

# #Clippers score per min
# regPPM_C=(sum(Cdf_gameh$diff_h)+sum(Cdf_gamev$diff_v))/((length(Cdf_gameh$diff_h)*2)+(length(Cdf_gamev$diff_v)*2));regPPM_C
# 
# #Clippers Opponent Score per min
# regPPM_CO=(sum(Odf_gameh$diff_h)+sum(Odf_gamev$diff_v))/((length(Odf_gameh$diff_h)*2)+(length(Odf_gamev$diff_v)*2));regPPM_CO
# 
# 
# #Rockets score per min
# regPPM_H=(sum(Hdf_gameh$diff_h)+sum(Hdf_gamev$diff_v))/((length(Hdf_gameh$diff_h)*2)+(length(Hdf_gamev$diff_v)*2));regPPM_H
# 
# #Rockets Opponent Score per min
# regPPM_HO=(sum(HOdf_gameh$diff_h)+sum(HOdf_gamev$diff_v))/((length(HOdf_gameh$diff_h)*2)+(length(HOdf_gamev$diff_v)*2));regPPM_HO
# 
# 
# #Pistons score per min
# regPPM_D=(sum(Ddf_gameh$diff_h)+sum(Ddf_gamev$diff_v))/((length(Ddf_gameh$diff_h)*2)+(length(Ddf_gamev$diff_v)*2));regPPM_D
# 
# #Pistons Opponent Score per min
# regPPM_DO=(sum(DOdf_gameh$diff_h)+sum(DOdf_gamev$diff_v))/((length(DOdf_gameh$diff_h)*2)+(length(DOdf_gamev$diff_v)*2));regPPM_DO

#####################################################################     WHEN HACKING

#Clippers score per min
HPPM_C=(sum(CHACK_h$HSM)+sum(CHACK_v$VSM))/((length(CHACK_h$HSM)*2)+(length(CHACK_v$VSM)*2));HPPM_C

#Clippers Opponent Score per min
HPPM_CO=(sum(CHACK_h$VSM)+sum(CHACK_v$HSM))/((length(CHACK_h$VSM)*2)+(length(CHACK_v$HSM)*2));HPPM_CO


#Rockets score per min
HPPM_H=(sum(HHACK_h$HSM)+sum(HHACK_v$VSM))/((length(HHACK_h$HSM)*2)+(length(HHACK_v$VSM)*2));HPPM_H

#Rockets Opponent Score per min
HPPM_HO=(sum(HHACK_h$VSM)+sum(HHACK_v$HSM))/((length(HHACK_h$VSM)*2)+(length(HHACK_v$HSM)*2));HPPM_HO


#Pistons score per min
HPPM_D=(sum(PHACK_h$HSM)+sum(PHACK_v$VSM))/((length(PHACK_h$HSM)*2)+(length(PHACK_v$VSM)*2));HPPM_D

#Pistons Opponent Score per min
HPPM_DO=(sum(PHACK_h$VSM)+sum(PHACK_v$HSM))/((length(PHACK_h$VSM)*2)+(length(PHACK_v$HSM)*2));HPPM_DO


################################            Regular time
CHACKRAW_v = filter(CHACKRAW, CHACKRAW$"GAME_ID" %in% C_V$GAME_ID)
CHACKRAW_h = filter(CHACKRAW, CHACKRAW$"GAME_ID" %in% C_H$GAME_ID)
HHACKRAW_v = filter(HHACKRAW, HHACKRAW$"GAME_ID" %in% HOU_V$GAME_ID)
HHACKRAW_h = filter(HHACKRAW, HHACKRAW$"GAME_ID" %in% HOU_H$GAME_ID)
PHACKRAW_v = filter(PHACKRAW, PHACKRAW$"GAME_ID" %in% DET_V$GAME_ID)
PHACKRAW_h = filter(PHACKRAW, PHACKRAW$"GAME_ID" %in% DET_H$GAME_ID)



#Clippers score per min
REGPPM_C=(sum(CHACKRAW_h$HSM)+sum(CHACKRAW_v$VSM))/((length(CHACKRAW_h$HSM)*2)+(length(CHACKRAW_v$VSM)*2));REGPPM_C

#Clippers Opponent Score per min
REGPPM_CO=(sum(CHACKRAW_h$VSM)+sum(CHACKRAW_v$HSM))/((length(CHACKRAW_h$VSM)*2)+(length(CHACKRAW_v$HSM)*2));REGPPM_CO


#Rockets score per min
REGPPM_H=(sum(HHACKRAW_h$HSM)+sum(HHACKRAW_v$VSM))/((length(HHACKRAW_h$HSM)*2)+(length(HHACKRAW_v$VSM)*2));REGPPM_H

#Rockets Opponent Score per min
REGPPM_HO=(sum(HHACKRAW_h$VSM)+sum(HHACKRAW_v$HSM))/((length(HHACKRAW_h$VSM)*2)+(length(HHACKRAW_v$HSM)*2));REGPPM_HO


#Pistons score per min
REGPPM_D=(sum(PHACKRAW_h$HSM)+sum(PHACKRAW_v$VSM))/((length(PHACKRAW_h$HSM)*2)+(length(PHACKRAW_v$VSM)*2));REGPPM_D

#Pistons Opponent Score per min
REGPPM_DO=(sum(PHACKRAW_h$VSM)+sum(PHACKRAW_v$HSM))/((length(PHACKRAW_h$VSM)*2)+(length(PHACKRAW_v$HSM)*2));REGPPM_DO




##fill=hack

```
The Pace of game in regular time and Hack-A-Shaq time
```{r}

Pace1=round(c(REGPPM_C, REGPPM_CO, REGPPM_H, REGPPM_HO, REGPPM_D, REGPPM_DO), digits= 3)

names1=c("LAC", "LAC_O", "HOU", "HOU_O", "DET", "DET_O")
REGPACE=bind_cols(as.data.frame(Pace1),as.data.frame(names1))

#Pace in regular time
ggplot(REGPACE, aes(x = factor(names1),y = Pace1)) + geom_bar(aes(fill = names1),stat = "identity") + geom_text(aes(label = Pace1, y = Pace1), size = 5)+xlab("Team's name")+ylab("Pace measured by score per min")


Pace_Hack=round(c(REGPPM_C, HPPM_C,REGPPM_CO, HPPM_CO, REGPPM_H, HPPM_H, REGPPM_HO, HPPM_HO, REGPPM_D, HPPM_D, REGPPM_DO, HPPM_DO), digits= 3)
names2=c("LAC","LAC_H", "LAC_O","LAC_O_H", "HOU","HOU_H", "HOU_O","HOU_O_H", "DET","DET_H", "DET_O", "DET_O_H")

HACK_PACE=bind_cols(as.data.frame(Pace_Hack),as.data.frame(names2))

#Pace in Hack time
ggplot(HACK_PACE, aes(x = factor(names2),y = Pace_Hack)) + geom_bar(aes(fill = names2),stat = "identity") + geom_text(aes(label = Pace_Hack, y = Pace_Hack), size = 5)+xlab("Team's name")+ylab("Pace measured by score per min")




```

If substitute the poor shooting player out, will that hurts?
```{r, , echo=FALSE}

#Jordan
trial2 <- matrix(c(117.6, 101.2, 105.8, 104.8), ncol=2)
rownames(trial2) <- c('Jordan on court', 'Jordan off court')
colnames(trial2) <- c('Clippers score per 100 round', 'Opponent score per 100 round')
trial.table2 <- as.table(trial2);trial.table2
#Howard
trial3 <- matrix(c(108.7, 106.4, 100.2, 104.9), ncol=2)
rownames(trial3) <- c('Howard on court', 'Howard off court')
colnames(trial3) <- c('Rockets score per 100 round', 'Opponent score per 100 round')
trial.table3 <- as.table(trial3);trial.table3
#Smith
trial4 <- matrix(c(109.2, 105.7, 101.4, 104.7), ncol=2)
rownames(trial4) <- c('Smith on court', 'Smith off court')
colnames(trial4) <- c('Rockets score per 100 round', 'Opponent score per 100 round')
trial.table4 <- as.table(trial4);trial.table4
#Drummond
trial5 <- matrix(c(106.5, 103.2, 108.2, 104.3), ncol=2)
rownames(trial5) <- c('Drummond on', 'Drummond off')
colnames(trial5) <- c('Pistons score per 100 round', 'Opponent score per 100 round')
trial.table5 <- as.table(trial5);trial.table5



```






```{r, echo=FALSE, results="hide", warning=FALSE, message=FALSE}
#Tim Duncan play againest Clippers

duncan_miss1=grepl("miss duncan 1",teamdata$VISITORDESCRIPTION)
duncan_miss2=grepl("miss duncan 2",teamdata$VISITORDESCRIPTION)
duncan_miss3=grepl("miss duncan 3",teamdata$VISITORDESCRIPTION)
duncan_miss4=grepl("miss duncan 4",teamdata$VISITORDESCRIPTION)
duncan_miss5=grepl("miss duncan 5",teamdata$VISITORDESCRIPTION)
duncan_miss6=grepl("miss duncan 6",teamdata$VISITORDESCRIPTION)
duncan_miss7=grepl("miss duncan 7",teamdata$VISITORDESCRIPTION)
duncan_miss8=grepl("miss duncan 8",teamdata$VISITORDESCRIPTION)
duncan_miss9=grepl("miss duncan 9",teamdata$VISITORDESCRIPTION)

duncan_miss11=grepl("miss duncan 1",teamdata$HOMEDESCRIPTION)
duncan_miss21=grepl("miss duncan 2",teamdata$HOMEDESCRIPTION)
duncan_miss31=grepl("miss duncan 3",teamdata$HOMEDESCRIPTION)
duncan_miss41=grepl("miss duncan 4",teamdata$HOMEDESCRIPTION)
duncan_miss51=grepl("miss duncan 5",teamdata$HOMEDESCRIPTION)
duncan_miss61=grepl("miss duncan 6",teamdata$HOMEDESCRIPTION)
duncan_miss71=grepl("miss duncan 7",teamdata$HOMEDESCRIPTION)
duncan_miss81=grepl("miss duncan 8",teamdata$HOMEDESCRIPTION)
duncan_miss91=grepl("miss duncan 9",teamdata$HOMEDESCRIPTION)


Nduncan_miss=sum(duncan_miss1)+sum(duncan_miss2)+sum(duncan_miss3)+sum(duncan_miss4)+sum(duncan_miss5)+sum(duncan_miss6)+sum(duncan_miss7)+sum(duncan_miss8)+sum(duncan_miss9)+sum(duncan_miss11)+sum(duncan_miss21)+sum(duncan_miss31)+sum(duncan_miss41)+sum(duncan_miss51)+sum(duncan_miss61)+sum(duncan_miss71)+sum(duncan_miss81)+sum(duncan_miss91);Nduncan_miss


duncan_made1=grepl("duncan 1",teamdata$VISITORDESCRIPTION)
duncan_made2=grepl("duncan 2",teamdata$VISITORDESCRIPTION)
duncan_made3=grepl("duncan 3",teamdata$VISITORDESCRIPTION)
duncan_made4=grepl("duncan 4",teamdata$VISITORDESCRIPTION)
duncan_made5=grepl("duncan 5",teamdata$VISITORDESCRIPTION)
duncan_made6=grepl("duncan 6",teamdata$VISITORDESCRIPTION)
duncan_made7=grepl("duncan 7",teamdata$VISITORDESCRIPTION)
duncan_made8=grepl("duncan 8",teamdata$VISITORDESCRIPTION)
duncan_made9=grepl("duncan 9",teamdata$VISITORDESCRIPTION)

duncan_made11=grepl("duncan 1",teamdata$HOMEDESCRIPTION)
duncan_made21=grepl("duncan 2",teamdata$HOMEDESCRIPTION)
duncan_made31=grepl("duncan 3",teamdata$HOMEDESCRIPTION)
duncan_made41=grepl("duncan 4",teamdata$HOMEDESCRIPTION)
duncan_made51=grepl("duncan 5",teamdata$HOMEDESCRIPTION)
duncan_made61=grepl("duncan 6",teamdata$HOMEDESCRIPTION)
duncan_made71=grepl("duncan 7",teamdata$HOMEDESCRIPTION)
duncan_made81=grepl("duncan 8",teamdata$HOMEDESCRIPTION)
duncan_made91=grepl("duncan 9",teamdata$HOMEDESCRIPTION)


Nduncan_made=sum(duncan_made1)+sum(duncan_made2)+sum(duncan_made3)+sum(duncan_made4)+sum(duncan_made5)+sum(duncan_made6)+sum(duncan_made7)+sum(duncan_made8)+sum(duncan_made9)+sum(duncan_made11)+sum(duncan_made21)+sum(duncan_made31)+sum(duncan_made41)+sum(duncan_made51)+sum(duncan_made61)+sum(duncan_made71)+sum(duncan_made81)+sum(duncan_made91);Nduncan_made

duncan_per=Nduncan_made/(Nduncan_made+Nduncan_miss);duncan_per


#Tim Duncan play against Rockets

duncan_miss1=grepl("miss duncan 1",teamdata2$VISITORDESCRIPTION)
duncan_miss2=grepl("miss duncan 2",teamdata2$VISITORDESCRIPTION)
duncan_miss3=grepl("miss duncan 3",teamdata2$VISITORDESCRIPTION)
duncan_miss4=grepl("miss duncan 4",teamdata2$VISITORDESCRIPTION)
duncan_miss5=grepl("miss duncan 5",teamdata2$VISITORDESCRIPTION)
duncan_miss6=grepl("miss duncan 6",teamdata2$VISITORDESCRIPTION)
duncan_miss7=grepl("miss duncan 7",teamdata2$VISITORDESCRIPTION)
duncan_miss8=grepl("miss duncan 8",teamdata2$VISITORDESCRIPTION)
duncan_miss9=grepl("miss duncan 9",teamdata2$VISITORDESCRIPTION)

duncan_miss11=grepl("miss duncan 1",teamdata2$HOMEDESCRIPTION)
duncan_miss21=grepl("miss duncan 2",teamdata2$HOMEDESCRIPTION)
duncan_miss31=grepl("miss duncan 3",teamdata2$HOMEDESCRIPTION)
duncan_miss41=grepl("miss duncan 4",teamdata2$HOMEDESCRIPTION)
duncan_miss51=grepl("miss duncan 5",teamdata2$HOMEDESCRIPTION)
duncan_miss61=grepl("miss duncan 6",teamdata2$HOMEDESCRIPTION)
duncan_miss71=grepl("miss duncan 7",teamdata2$HOMEDESCRIPTION)
duncan_miss81=grepl("miss duncan 8",teamdata2$HOMEDESCRIPTION)
duncan_miss91=grepl("miss duncan 9",teamdata2$HOMEDESCRIPTION)


Nduncan_miss=sum(duncan_miss1)+sum(duncan_miss2)+sum(duncan_miss3)+sum(duncan_miss4)+sum(duncan_miss5)+sum(duncan_miss6)+sum(duncan_miss7)+sum(duncan_miss8)+sum(duncan_miss9)+sum(duncan_miss11)+sum(duncan_miss21)+sum(duncan_miss31)+sum(duncan_miss41)+sum(duncan_miss51)+sum(duncan_miss61)+sum(duncan_miss71)+sum(duncan_miss81)+sum(duncan_miss91);Nduncan_miss


duncan_made1=grepl("duncan 1",teamdata2$VISITORDESCRIPTION)
duncan_made2=grepl("duncan 2",teamdata2$VISITORDESCRIPTION)
duncan_made3=grepl("duncan 3",teamdata2$VISITORDESCRIPTION)
duncan_made4=grepl("duncan 4",teamdata2$VISITORDESCRIPTION)
duncan_made5=grepl("duncan 5",teamdata2$VISITORDESCRIPTION)
duncan_made6=grepl("duncan 6",teamdata2$VISITORDESCRIPTION)
duncan_made7=grepl("duncan 7",teamdata2$VISITORDESCRIPTION)
duncan_made8=grepl("duncan 8",teamdata2$VISITORDESCRIPTION)
duncan_made9=grepl("duncan 9",teamdata2$VISITORDESCRIPTION)

duncan_made11=grepl("duncan 1",teamdata2$HOMEDESCRIPTION)
duncan_made21=grepl("duncan 2",teamdata2$HOMEDESCRIPTION)
duncan_made31=grepl("duncan 3",teamdata2$HOMEDESCRIPTION)
duncan_made41=grepl("duncan 4",teamdata2$HOMEDESCRIPTION)
duncan_made51=grepl("duncan 5",teamdata2$HOMEDESCRIPTION)
duncan_made61=grepl("duncan 6",teamdata2$HOMEDESCRIPTION)
duncan_made71=grepl("duncan 7",teamdata2$HOMEDESCRIPTION)
duncan_made81=grepl("duncan 8",teamdata2$HOMEDESCRIPTION)
duncan_made91=grepl("duncan 9",teamdata2$HOMEDESCRIPTION)


Nduncan_made=sum(duncan_made1)+sum(duncan_made2)+sum(duncan_made3)+sum(duncan_made4)+sum(duncan_made5)+sum(duncan_made6)+sum(duncan_made7)+sum(duncan_made8)+sum(duncan_made9)+sum(duncan_made11)+sum(duncan_made21)+sum(duncan_made31)+sum(duncan_made41)+sum(duncan_made51)+sum(duncan_made61)+sum(duncan_made71)+sum(duncan_made81)+sum(duncan_made91);Nduncan_made

H_duncan_per=Nduncan_made/(Nduncan_made+Nduncan_miss);H_duncan_per



#Tim Duncan play against DET
duncan_miss1=grepl("miss duncan 1",teamdata3$VISITORDESCRIPTION)
duncan_miss2=grepl("miss duncan 2",teamdata3$VISITORDESCRIPTION)
duncan_miss3=grepl("miss duncan 3",teamdata3$VISITORDESCRIPTION)
duncan_miss4=grepl("miss duncan 4",teamdata3$VISITORDESCRIPTION)
duncan_miss5=grepl("miss duncan 5",teamdata3$VISITORDESCRIPTION)
duncan_miss6=grepl("miss duncan 6",teamdata3$VISITORDESCRIPTION)
duncan_miss7=grepl("miss duncan 7",teamdata3$VISITORDESCRIPTION)
duncan_miss8=grepl("miss duncan 8",teamdata3$VISITORDESCRIPTION)
duncan_miss9=grepl("miss duncan 9",teamdata3$VISITORDESCRIPTION)

duncan_miss11=grepl("miss duncan 1",teamdata3$HOMEDESCRIPTION)
duncan_miss21=grepl("miss duncan 2",teamdata3$HOMEDESCRIPTION)
duncan_miss31=grepl("miss duncan 3",teamdata3$HOMEDESCRIPTION)
duncan_miss41=grepl("miss duncan 4",teamdata3$HOMEDESCRIPTION)
duncan_miss51=grepl("miss duncan 5",teamdata3$HOMEDESCRIPTION)
duncan_miss61=grepl("miss duncan 6",teamdata3$HOMEDESCRIPTION)
duncan_miss71=grepl("miss duncan 7",teamdata3$HOMEDESCRIPTION)
duncan_miss81=grepl("miss duncan 8",teamdata3$HOMEDESCRIPTION)
duncan_miss91=grepl("miss duncan 9",teamdata3$HOMEDESCRIPTION)


Nduncan_miss=sum(duncan_miss1)+sum(duncan_miss2)+sum(duncan_miss3)+sum(duncan_miss4)+sum(duncan_miss5)+sum(duncan_miss6)+sum(duncan_miss7)+sum(duncan_miss8)+sum(duncan_miss9)+sum(duncan_miss11)+sum(duncan_miss21)+sum(duncan_miss31)+sum(duncan_miss41)+sum(duncan_miss51)+sum(duncan_miss61)+sum(duncan_miss71)+sum(duncan_miss81)+sum(duncan_miss91);Nduncan_miss


duncan_made1=grepl("duncan 1",teamdata3$VISITORDESCRIPTION)
duncan_made2=grepl("duncan 2",teamdata3$VISITORDESCRIPTION)
duncan_made3=grepl("duncan 3",teamdata3$VISITORDESCRIPTION)
duncan_made4=grepl("duncan 4",teamdata3$VISITORDESCRIPTION)
duncan_made5=grepl("duncan 5",teamdata3$VISITORDESCRIPTION)
duncan_made6=grepl("duncan 6",teamdata3$VISITORDESCRIPTION)
duncan_made7=grepl("duncan 7",teamdata3$VISITORDESCRIPTION)
duncan_made8=grepl("duncan 8",teamdata3$VISITORDESCRIPTION)
duncan_made9=grepl("duncan 9",teamdata3$VISITORDESCRIPTION)

duncan_made11=grepl("duncan 1",teamdata3$HOMEDESCRIPTION)
duncan_made21=grepl("duncan 2",teamdata3$HOMEDESCRIPTION)
duncan_made31=grepl("duncan 3",teamdata3$HOMEDESCRIPTION)
duncan_made41=grepl("duncan 4",teamdata3$HOMEDESCRIPTION)
duncan_made51=grepl("duncan 5",teamdata3$HOMEDESCRIPTION)
duncan_made61=grepl("duncan 6",teamdata3$HOMEDESCRIPTION)
duncan_made71=grepl("duncan 7",teamdata3$HOMEDESCRIPTION)
duncan_made81=grepl("duncan 8",teamdata3$HOMEDESCRIPTION)
duncan_made91=grepl("duncan 9",teamdata3$HOMEDESCRIPTION)


Nduncan_made=sum(duncan_made1)+sum(duncan_made2)+sum(duncan_made3)+sum(duncan_made4)+sum(duncan_made5)+sum(duncan_made6)+sum(duncan_made7)+sum(duncan_made8)+sum(duncan_made9)+sum(duncan_made11)+sum(duncan_made21)+sum(duncan_made31)+sum(duncan_made41)+sum(duncan_made51)+sum(duncan_made61)+sum(duncan_made71)+sum(duncan_made81)+sum(duncan_made91);Nduncan_made

D_duncan_per=Nduncan_made/(Nduncan_made+Nduncan_miss);D_duncan_per

```

Key players performance in Hack periord
```{r, echo=FALSE}
duncan_reg=0.513

duncan=round(c(duncan_reg,duncan_per, H_duncan_per, D_duncan_per), digits= 3)

lib3=c("Duncan Average", "Against Clippers","Against Rockets", "Against Pistons")
tim_per=bind_cols(as.data.frame(duncan),as.data.frame(lib3))

#Tim Duncan shooting percentage
ggplot(tim_per, aes(x = factor(lib3),y = duncan)) + geom_bar(aes(fill = lib3),stat = "identity") + geom_text(aes(label = duncan, y = duncan), size = 5)+xlab("Tim Duncan")+ylab("Shooting percentage")

```




Wins contribution
```{r, echo=FALSE, results="hide", warning=FALSE, message=FALSE}
#Clippers home result
C_H_W = filter(teamdata_h, PERIOD > 3, min==0, sec==0)
C_H_W$margin=C_H_W$vscore-C_H_W$hscore
C_H_W = filter(C_H_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
C_H_W$Win=1
C_H_W$Win[C_H_W$margin<0]=0
sum(C_H_W$Win)


#Clippers visit result
C_V_W = filter(teamdata_v, PERIOD > 3, min==0, sec==0)
C_V_W$margin=C_V_W$vscore-C_V_W$hscore
C_V_W = filter(C_V_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
C_V_W$Win=1
C_V_W$Win[C_V_W$margin>0]=0
sum(C_V_W$Win)

#Win percent (needs Hack data)
Cwin_per=(sum(C_H_W$Win)+sum(C_V_W$Win))/(length(C_H_W$Win)+length(C_V_W$Win));Cwin_per


#Rockets home result
R_H_W = filter(HOUteamdata_h, PERIOD > 3, min==0, sec==0)
R_H_W$margin=R_H_W$vscore-R_H_W$hscore
R_H_W = filter(R_H_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
R_H_W$Win=1
R_H_W$Win[R_H_W$margin<0]=0
sum(R_H_W$Win)

#Rockets visit result
R_V_W = filter(HOUteamdata_v, PERIOD > 3, min==0, sec==0)
R_V_W$margin=R_V_W$vscore-R_V_W$hscore
R_V_W = filter(R_V_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
R_V_W$Win=1
R_V_W$Win[R_V_W$margin>0]=0
sum(R_V_W$Win)

#Win percent (needs Hack data)
Rwin_per=(sum(R_H_W$Win)+sum(R_V_W$Win))/(length(R_H_W$Win)+length(R_V_W$Win));Rwin_per

#Pistions home result
D_H_W = filter(DETteamdata_h, PERIOD > 3, min==0, sec==0)
D_H_W$margin=D_H_W$vscore-D_H_W$hscore
D_H_W = filter(D_H_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
D_H_W$Win=1
D_H_W$Win[D_H_W$margin<0]=0
sum(D_H_W$Win)

#Pistions visit result
D_V_W = filter(DETteamdata_v, PERIOD > 3, min==0, sec==0)
D_V_W$margin=D_V_W$vscore-D_V_W$hscore
D_V_W = filter(D_V_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
D_V_W$Win=1
D_V_W$Win[D_V_W$margin>0]=0
sum(D_V_W$Win)

#Win percent (needs Hack data)
Dwin_per=(sum(D_H_W$Win)+sum(D_V_W$Win))/(length(D_H_W$Win)+length(D_V_W$Win));Dwin_per



#############################################Hack#################3
#Clippers home result
C_H_W = filter(teamdata_h, PERIOD > 3, min==0, sec==0)
# abc=as.data.frame(unique(CHACK_h$"GAME_ID"))
# abc$ab=abc$`unique(CHACK_h$GAME_ID)`

C_H_W$margin=C_H_W$vscore-C_H_W$hscore
C_H_W = filter(C_H_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
C_H_W = filter(C_H_W, GAME_ID %in% as.factor(unique(CHACK_h$"GAME_ID")))
C_H_W$Win=1
C_H_W$Win[C_H_W$margin<0]=0
sum(C_H_W$Win)



#Clippers visit result
C_V_W = filter(teamdata_v, PERIOD > 3, min==0, sec==0)
C_V_W$margin=C_V_W$vscore-C_V_W$hscore
C_V_W = filter(C_V_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
C_V_W = filter(C_V_W, GAME_ID %in% as.factor(unique(CHACK_v$"GAME_ID")))
C_V_W$Win=1
C_V_W$Win[C_V_W$margin>0]=0
sum(C_V_W$Win)

#Win percent (needs Hack data)
CHwin_per=(sum(C_H_W$Win)+sum(C_V_W$Win))/(length(C_H_W$Win)+length(C_V_W$Win));Cwin_per


#Rockets home result
R_H_W = filter(HOUteamdata_h, PERIOD > 3, min==0, sec==0)
R_H_W$margin=R_H_W$vscore-R_H_W$hscore
R_H_W = filter(R_H_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
R_H_W = filter(R_H_W, GAME_ID %in% as.factor(unique(HHACK_h$"GAME_ID")))
R_H_W$Win=1
R_H_W$Win[R_H_W$margin<0]=0
sum(R_H_W$Win)

#Rockets visit result
R_V_W = filter(HOUteamdata_v, PERIOD > 3, min==0, sec==0)
R_V_W$margin=R_V_W$vscore-R_V_W$hscore
R_V_W = filter(R_V_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
R_V_W = filter(R_V_W, GAME_ID %in% as.factor(unique(HHACK_v$"GAME_ID")))
R_V_W$Win=1
R_V_W$Win[R_V_W$margin>0]=0
sum(R_V_W$Win)

#Win percent (needs Hack data)
RHwin_per=(sum(R_H_W$Win)+sum(R_V_W$Win))/(length(R_H_W$Win)+length(R_V_W$Win));Rwin_per

#Pistions home result
D_H_W = filter(DETteamdata_h, PERIOD > 3, min==0, sec==0)
D_H_W$margin=D_H_W$vscore-D_H_W$hscore
D_H_W = filter(D_H_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
D_H_W = filter(D_H_W, GAME_ID %in% as.factor(unique(PHACK_h$"GAME_ID")))
D_H_W$Win=1
D_H_W$Win[D_H_W$margin<0]=0
sum(D_H_W$Win)

#Pistions visit result
D_V_W = filter(DETteamdata_v, PERIOD > 3, min==0, sec==0)
D_V_W$margin=D_V_W$vscore-D_V_W$hscore
D_V_W = filter(D_V_W, margin>0 | margin<0, HOMEDESCRIPTION=="null", VISITORDESCRIPTION=="null")
D_V_W = filter(D_V_W, GAME_ID %in% as.factor(unique(PHACK_v$"GAME_ID")))
D_V_W$Win=1
D_V_W$Win[D_V_W$margin>0]=0
sum(D_V_W$Win)

#Win percent (needs Hack data)
DHwin_per=(sum(D_H_W$Win)+sum(D_V_W$Win))/(length(D_H_W$Win)+length(D_V_W$Win));Dwin_per




```
Will Hack bring wins?
```{r}
Win_total=round(c(Cwin_per, Rwin_per, Dwin_per), digits= 3)

lib1=c("LAC", "HOU","DET")
wins=bind_cols(as.data.frame(Win_total),as.data.frame(lib1))

#Wining percent in the whole season
ggplot(wins, aes(x = factor(lib1),y = Win_total)) + geom_bar(aes(fill = lib1),stat = "identity") + geom_text(aes(label = Win_total, y = Win_total), size = 5)+xlab("Team")+ylab("Regular Winning percentage")

#Games be hacked
GamesHack=round(c(Cwin_per, CHwin_per, Rwin_per, RHwin_per, Dwin_per, DHwin_per), digits= 3)

lib2=c("LAC_win_per", "LAC_Hack_win_per", "HOU_win_per","HOU_Hack_win_per", "DET_win_per", "DET_Hack_win_per")
hwins=bind_cols(as.data.frame(GamesHack),as.data.frame(lib2))

#Wining percent in the hacked game
ggplot(hwins, aes(x = factor(lib2),y = GamesHack)) + geom_bar(aes(fill = lib2),stat = "identity") + geom_text(aes(label = GamesHack, y = GamesHack), size = 5)+xlab("Team")+ylab("Winning percentage compare with hack")



```


Last 2 minutes lead
```{r, echo=FALSE, results="hide", warning=FALSE, message=FALSE}
# Define last 2 min(2:18-1:45)(no need Hack)
#home
C_H2.1 = filter(teamdata_h, PERIOD == 4, min==2, sec<20)
C_H2.2 = filter(teamdata_h, PERIOD == 4, min==1, sec>40)
C_H2=bind_rows(C_H2.1, C_H2.2)
length(unique(C_H2$GAME_ID))

#home margin in last 2 min
Cdf_gameh_2min = data.frame()
for(i in unique(C_H2$"GAME_ID")){
  df = filter(C_H2, C_H2$"GAME_ID" == i)
  median_margin = median(df$SCOREMARGIN)
  c = cbind(median_margin,i)
  Cdf_gameh_2min = rbind(Cdf_gameh_2min,c)
}


#vistor
C_V2.1 = filter(teamdata_v, PERIOD == 4, min==2, sec<20)
C_V2.2 = filter(teamdata_v, PERIOD == 4, min==1, sec>40)
C_V2=bind_rows(C_V2.1, C_V2.2)
length(unique(C_V2$GAME_ID))

#vistor margin in last 2 min
Cdf_gamev_2min = data.frame()
for(i in unique(C_V2$"GAME_ID")){
  df = filter(C_V2, C_V2$"GAME_ID" == i)
  median_margin = median(df$SCOREMARGIN)*(-1)
  D = cbind(median_margin,i)
  Cdf_gamev_2min = rbind(Cdf_gamev_2min,D)
}


last_2min=bind_rows(Cdf_gamev_2min, Cdf_gameh_2min)

#If wants to win, one should lead Clippers how much points by last 2 mins
lose_game_V=filter(C_V_W, Win==0)
lose_game_H=filter(C_H_W, Win==0)
lose_game=bind_rows(lose_game_H, lose_game_V)
C_lg2 = filter(last_2min, i %in% lose_game$"GAME_ID")


C_lead_2min=1+floor(abs(median(as.numeric(C_lg2$median_margin))));C_lead_2min


#Rockets home
R_H2.1 = filter(HOUteamdata_h, PERIOD == 4, min==2, sec<20)
R_H2.2 = filter(HOUteamdata_h, PERIOD == 4, min==1, sec>40)
R_H2=bind_rows(R_H2.1, R_H2.2)
length(unique(R_H2$GAME_ID))

# Rockets home margin in last 2 min
Rdf_gameh_2min = data.frame()
for(i in unique(R_H2$"GAME_ID")){
  df = filter(R_H2, R_H2$"GAME_ID" == i)
  median_margin = median(df$SCOREMARGIN)
  c = cbind(median_margin,i)
  Rdf_gameh_2min = rbind(Rdf_gameh_2min,c)
}


#Rockets vistor
R_V2.1 = filter(HOUteamdata_v, PERIOD == 4, min==2, sec<20)
R_V2.2 = filter(HOUteamdata_v, PERIOD == 4, min==1, sec>40)
R_V2=bind_rows(R_V2.1, R_V2.2)
length(unique(R_V2$GAME_ID))

#Rockets vistor margin in last 2 min
Rdf_gamev_2min = data.frame()
for(i in unique(R_V2$"GAME_ID")){
  df = filter(R_V2, R_V2$"GAME_ID" == i)
  median_margin = median(df$SCOREMARGIN)*(-1)
  D = cbind(median_margin,i)
  Rdf_gamev_2min = rbind(Rdf_gamev_2min,D)
}


R_last_2min=bind_rows(Rdf_gamev_2min, Rdf_gameh_2min)

#If wants to win, one should lead Rockets how much points by last 2 mins
R_lose_game_V=filter(R_V_W, Win==0)
R_lose_game_H=filter(R_H_W, Win==0)
R_lose_game=bind_rows(R_lose_game_H, R_lose_game_V)
R_lg2 = filter(R_last_2min, i %in% R_lose_game$"GAME_ID")


R_lead_2min=1+floor(abs(median(as.numeric(R_lg2$median_margin))));R_lead_2min


#Pistions home
D_H2.1 = filter(DETteamdata_h, PERIOD == 4, min==2, sec<20)
D_H2.2 = filter(DETteamdata_h, PERIOD == 4, min==1, sec>40)
D_H2=bind_rows(D_H2.1, D_H2.2)
length(unique(D_H2$GAME_ID))
# Pistons home margin in last 2 min
Ddf_gameh_2min = data.frame()
for(i in unique(D_H2$"GAME_ID")){
  df = filter(D_H2, D_H2$"GAME_ID" == i)
  median_margin = median(df$SCOREMARGIN)
  c = cbind(median_margin,i)
  Ddf_gameh_2min = rbind(Ddf_gameh_2min,c)
}


#Pistions vistor
D_V2.1 = filter(DETteamdata_v, PERIOD == 4, min==2, sec<20)
D_V2.2 = filter(DETteamdata_v, PERIOD == 4, min==1, sec>40)
D_V2=bind_rows(D_V2.1, D_V2.2)
length(unique(D_V2$GAME_ID))

#pistions vistor margin in last 2 min
Ddf_gamev_2min = data.frame()
for(i in unique(D_V2$"GAME_ID")){
  df = filter(D_V2, D_V2$"GAME_ID" == i)
  median_margin = median(df$SCOREMARGIN)*(-1)
  D = cbind(median_margin,i)
  Ddf_gamev_2min = rbind(Ddf_gamev_2min,D)
}


D_last_2min=bind_rows(Ddf_gamev_2min, Ddf_gameh_2min)

#If wants to win, one should lead Rockets how much points by last 2 mins
D_lose_game_V=filter(D_V_W, Win==0)
D_lose_game_H=filter(D_H_W, Win==0)
D_lose_game=bind_rows(D_lose_game_H, D_lose_game_V)
D_lg2 = filter(D_last_2min, i %in% D_lose_game$"GAME_ID")


D_lead_2min=1+floor(abs(median(as.numeric(D_lg2$median_margin))));D_lead_2min



#Last 2 min score margin

#Clippers
Cdf_gameh2 = filter(Cdf_gameh, min2r == 24)
Cdf_gamev2 = filter(Cdf_gamev, min2r == 24)
Odf_gamev2 = filter(Odf_gamev, min2r == 24)
Odf_gameh2 = filter(Odf_gameh, min2r == 24)
Hdf_gameh2 = filter(Hdf_gameh, min2r == 24)
Hdf_gamev2 = filter(Hdf_gamev, min2r == 24) 
HOdf_gamev2 = filter(HOdf_gamev, min2r == 24)
HOdf_gameh2 = filter(HOdf_gameh, min2r == 24)
Ddf_gameh2 = filter(Ddf_gameh, min2r == 24)
Ddf_gamev2 = filter(Ddf_gamev, min2r == 24)
DOdf_gamev2 = filter(DOdf_gamev, min2r == 24)
DOdf_gameh2 = filter(DOdf_gameh, min2r == 24)




#At least lead clippers in last 2 mins
C_Margin_2=1+floor((sum(Cdf_gameh2$diff_h)+sum(Cdf_gamev$diff_v))/(length(Cdf_gameh2$diff_h)+length(Cdf_gamev$diff_v))-(sum(Odf_gameh2$diff_h)+sum(Odf_gamev$diff_v))/(length(Odf_gameh2$diff_h)+length(Odf_gamev$diff_v)));C_Margin_2

#At least lead Rockets in last 2 mins
R_Margin_2=1+floor((sum(Hdf_gameh2$diff_h)+sum(Hdf_gamev$diff_v))/(length(Hdf_gameh2$diff_h)+length(Hdf_gamev$diff_v))-(sum(HOdf_gameh2$diff_h)+sum(HOdf_gamev$diff_v))/(length(HOdf_gameh2$diff_h)+length(HOdf_gamev$diff_v)));R_Margin_2


#At least lead Pistons in last 2 mins
D_Margin_2=1+floor((sum(Ddf_gameh2$diff_h)+sum(Ddf_gamev$diff_v))/(length(Ddf_gameh2$diff_h)+length(Ddf_gamev$diff_v))-(sum(DOdf_gameh2$diff_h)+sum(DOdf_gamev$diff_v))/(length(DOdf_gameh2$diff_h)+length(DOdf_gamev$diff_v)));D_Margin_2



#95% win 
#Clippers
Cdf_gamev2$diff_h=Cdf_gamev2$diff_v
Odf_gamev2$diff_h=Odf_gamev2$diff_v
cdf_game2=bind_rows(Cdf_gameh2,Cdf_gamev2)
Odf_game2 = bind_rows(Odf_gameh2,Odf_gamev2)

Cdf22=merge(cdf_game2, Odf_game2, by.x = "GAME_ID", by.y = "GAME_ID")
#get the margin of each game score in last 2 min
Cdf22$margin=Cdf22$diff_h.x-Cdf22$diff_h.y

Clead=1+floor(qnorm(0.95, mean=mean(Cdf22$margin), sd=sqrt(var(Cdf22$margin))));Clead

#Rockets
Hdf_gamev2$diff_h=Hdf_gamev2$diff_v
HOdf_gamev2$diff_h=HOdf_gamev2$diff_v
Hdf_game2=bind_rows(Hdf_gameh2, Hdf_gamev2)
HOdf_game2=bind_rows(HOdf_gameh2, HOdf_gamev2)

Hdf22=merge(Hdf_game2, HOdf_game2, by.x = "GAME_ID", by.y = "GAME_ID")
#get the margin of each game score in last 2 min
Hdf22$margin=Hdf22$diff_h.x-Hdf22$diff_h.y

Hlead=1+floor(qnorm(0.95, mean=mean(Hdf22$margin), sd=sqrt(var(Hdf22$margin))));Hlead

#Pistions
Ddf_gamev2$diff_h=Ddf_gamev2$diff_v
DOdf_gamev2$diff_h=DOdf_gamev2$diff_v
Ddf_game2=bind_rows(Ddf_gameh2, Ddf_gamev2)
DOdf_game2=bind_rows(DOdf_gameh2, DOdf_gamev2)

Ddf22=merge(Ddf_game2, DOdf_game2, by.x = "GAME_ID", by.y = "GAME_ID")
#get the margin of each game score in last 2 min
Ddf22$margin=Ddf22$diff_h.x-Ddf22$diff_h.y

Dlead=1+floor(qnorm(0.95, mean=mean(Ddf22$margin), sd=sqrt(var(Ddf22$margin))));Dlead





```
Game statistics of last 2 minutes(Can not use Hack-A-Shaq)
```{r}
#Winning strategy in last 2 mintues
#Three ways to compare. First, filter the losing game and get the mean of losing game margin. Second, get the last 2 min interval of the score margin for each team to compare the difference. Third, based on the score margin of each team in last two minutes, get the 95% distribution of leading points at the begining of last two minutes.
trial <- matrix(c(C_lead_2min,R_lead_2min,D_lead_2min,C_Margin_2, R_Margin_2, D_Margin_2, Clead, Hlead, Dlead), ncol=3)
rownames(trial) <- c('Clippers', 'Rockets', "Pistons")
colnames(trial) <- c('Mean Game Lose Margin', 'Mean differential', "95% to win")
trial.table <- as.table(trial);trial.table


```
