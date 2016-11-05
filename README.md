---
title: "Applied Data Science - Project 5 Group 3"
author: "Bob Minnich, Tara Shui, Yuhan Sun, and Yuan Zhao"
output:
  html_document:
    css: styles.css
---

# Introduction
## What is a "Hack-a-Shaq"?

<div style="text-align: center;" align = "center">
<iframe width="640" height="480" align = "center" src="https://www.youtube.com/embed/-S_soY5Jg_Q" frameborder="0" allowfullscreen></iframe>
</div>

<br><br>
<br><br>
<br><br>
<br><br>

## Why Hack-a-Shaq....
### or a-Jordan, or a-Drummond, or a-Howard?

<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="figs/Three.png" float = "center" ALT="image" width="800">
  </span>
</div>

<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>

## What do these players have in common?

<div style="text-align: center;" align = "center">
<iframe width="640" height="360" src="https://www.youtube.com/embed/YwYXAzsTsdw" frameborder="0" allowfullscreen></iframe>
</div>

<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>

## How much do they make? 19.690 MILLION USD

<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="figs/income.png" float = "center" ALT="image" width="800">
  </span>
</div>

<br><br>
<br><br>
<br><br>
<br><br>

```{r,echo=FALSE,message=FALSE,warning=FALSE}
setwd("~/Google Drive/2016 Spring/Applied Data Science/Project 5/finalproject-p5-team3")
#setwd("/Users/sunxiaohan/Desktop/github/finalproject-p5-team3")
#setwd("/Users/bobminnich/Documents/Columbia/Courses/Applied_Data_Science/FinalProject/finalproject-p5-team3")
library(dplyr)
library(grid)
library(gridExtra)

# COLORS:
# Pistons - "#009E73" opponent: "#66FFCC"
# Rockets - "#CC0033" opponent: "#CC9999"
# Clippers - "#0072B2" opponent: "#99CCFF"

load("data/ClippersTwoMinIntervals.RData")
load("data/PistonsTwoMinIntervals.RData")
load("data/RocketsTwoMinIntervals.RData")

all_df = rbind(TWOMINS1,TWOMINS2,TWOMINS3)

```

## In the News...

<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="figs/News.jpg" float = "center" ALT="image" width="800">
  </span>
</div>

<br><br>
<br><br>
<br><br>
<br><br>

# So... let's find these "hacks"!

### Can you tell where the "hacks" are happening?

<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="figs/spurs-clippers.png" float = "center" ALT="image" width="800">
  </span>
</div>

<br><br>
<br><br>
<br><br>
<br><br>

## Goal 1: Targeting "bad" free throwers

<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="figs/Jordan FT.png" float = "center" ALT="image" width="800">
  </span>
</div>

### ... But how do we define bad?

<br><br>
<br><br>
<br><br>
<br><br>

<center> <h3> Free Throw Attempts (FTA) vs Free Throw Percentages (FT%) </h3> </center>

```{r,results='asis',tidy=FALSE, echo=FALSE, fig.align='center', message=FALSE, warning=FALSE}
setwd("/Users/sunxiaohan/Desktop/github/finalproject-p5-team3")


library(ggplot2)
library(plotly)

load('Data/playerstatsData.Rdata')
note=playerstatsData[c(107,130,20,259),]  #Deandre Jordan and DH

a=list()
for (i in 1:4){
  m=note[i,]
  a[[i]]=list(
    x=m$FT_PCT,
    y=m$FTA,
    text=m$PLAYER_NAME,
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
  )
}

plot_ly(playerstatsData,x = FT_PCT, y =FTA ,
        text=paste('Name:',playerstatsData$PLAYER_NAME),
        mode = "markers", color = playerstatsData$FT_PCT) %>% layout(annotations = a)

```

<br><br>
<br><br>
<br><br>
<br><br>

## Goal 2: Determining "hack" games

### We decided to look at three teams we hear about in the news:

<div style="text-align: center;">

  <span style="float:left;width: 33%;">
  <IMG SRC="figs/LA_logo.png" float = "right" ALT="image">
  </span>
  
  <span style="float:left;width: 33%;">
  <IMG SRC="figs/DET_logo.png" float = "right" ALT="image">
  </span>
  
  <span style="float:left;width: 33%;">
  <IMG SRC="figs/hou_logo.png" float = "right" ALT="image">
  </span>

</div>

<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>

### And the performance of their infamously bad free throwers last season:

<div style="text-align: center;">

  <span style="float:left;width: 50%;">
  <IMG SRC="figs/Clippers_Dist.png" float = "right" ALT="image">
  </span>
  
  <span style="float:right;width: 50%;">
  <IMG SRC="figs/Pistons_Dist.png" float = "right" ALT="image">
  </span>
  
  <IMG SRC="figs/Rockets_Dist.png" float = "center" ALT="image">

</div>

<br><br>
<br><br>
<br><br>
<br><br>

## Goal 3: Determining "hack" periods
### For each game, we broke everything down into two minute intervals

<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="figs/2mins.png" float = "center" ALT="image" width="800">
  </span>
</div>

#### ... 493 two-minute intervals were identified as "hacking"
#### ... and 5165 two-minute intervals that were identified as not "hacking"

<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>

# So, the Big Question: Does Hack-A-Shaq work?

<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>

## Two-Minute Score Margins (Aggregate)

```{r,results='asis',tidy=FALSE, echo=FALSE, fig.align='center'}

t_plot = all_df
t_plot$HACKINT[t_plot$HACKINT == TRUE] = "Hack"
t_plot$HACKINT[t_plot$HACKINT == FALSE] = "No Hack"

a = ggplot(all_df, aes(SM, fill = HACKINT)) + 
  geom_histogram(binwidth = 1, position = 'identity') +
  theme(legend.position="none")+
  labs( x = 'Two Minute Score Margin', y = 'Frequency') +
  scale_fill_manual(values = c("#CC6666", "#9999CC"))

hack = subset(all_df, HACKINT == TRUE)$SM
no_hack  = subset(all_df, HACKINT == FALSE)$SM
no_hack_mean = round(mean(no_hack),2)
hack_mean = round(mean(hack),2)
#print(length(hack))
#print(length(no_hack))

b = ggplot(t_plot, aes(HACKINT, SM)) + 
  geom_boxplot(aes(fill = factor(HACKINT))) + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs( y = 'Two Minute Score Margin')+
  annotate("text",x = 1.45,y = hack_mean-0.5,label = paste("mean:",hack_mean))+
  annotate("text",x = 2.45,y = no_hack_mean-0.5,label = paste("mean:",no_hack_mean))+ 
  theme(axis.title.y = element_blank()) +
  scale_fill_manual(values = c("#9999CC", "#CC6666"))

#summary(subset(TWOMINS1, HACKINT == TRUE)$SM)
#summary(subset(TWOMINS1, HACKINT == FALSE)$SM)
  MainTitle = "2015 Regular Season Games - Jordan, Drummond, Howard, Smith"
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(0.5, 5), "null"))))
  grid.text(MainTitle, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(a, vp = viewport(layout.pos.row = 2, layout.pos.col = 1),newpage=FALSE)
  print(b, vp = viewport(layout.pos.row = 2, layout.pos.col = 2),newpage=FALSE)
  popViewport(1)
  
# ttest_all = t.test(hack,no_hack)
# print(ttest_all)
```

t-test results

* t: 3.5334
* p-value: 0.0004412
* Degrees of Freedom: 612.07
* 95% Confidence Intervals: {0.2365976, 0.8286634}

```{r, echo=FALSE, results="hide", warning=FALSE, message=FALSE}
setwd("~/Google Drive/2016 Spring/Applied Data Science/Project 5/finalproject-p5-team3")
#setwd("C:/Users/mangmangyuzhou/Desktop/2016 Spring/data science/project 5")
library(dplyr)
library(ggplot2)
library(lubridate)

teamdata=readRDS("./data/clippers.RDS")
teamdata2=readRDS("./data/rockets.RDS")
teamdata3=readRDS("./data/pistons.RDS")
load("./data/ClippersTwoMinIntervals.Rdata")
CHACKRAW=TWOMINS1
load("./data/RocketsTwoMinIntervals.Rdata")
HHACKRAW=TWOMINS2
load("./data/PistonsTwoMinIntervals.Rdata")
PHACKRAW=TWOMINS3
CHACK=filter(CHACKRAW, NUMFT>0)
HHACK=filter(HHACKRAW, NUMFT>0)
PHACK=filter(PHACKRAW, NUMFT>0)

maximin = function(hscore){
  return (max(hscore) - min(hscore))
}
load("./data/visitinghome.Rdata")
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
```
<br><br>
<br><br>

## Average Pace of the Game per Minute 
```{r, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center'}

Pace1=round(c(REGPPM_C, REGPPM_CO, REGPPM_H, REGPPM_HO, REGPPM_D, REGPPM_DO), digits= 3)

names1=c("LAC", "LAC_O", "HOU", "HOU_O", "DET", "DET_O")
REGPACE=bind_cols(as.data.frame(Pace1),as.data.frame(names1))

#Pace in regular time
ggplot(REGPACE, aes(x = factor(names1),y = Pace1, fill = names1)) + geom_bar(stat = "identity") + geom_text(aes(label = Pace1, y = Pace1), size = 5)+xlab("Team's name")+ylab("Pace measured by score per min")+scale_fill_manual(values =  c("#009E73","#66FFCC","#CC0033","#CC9999","#0072B2","#99CCFF"))
```

<br><br>
<br><br>

## Average Pace During "Hack" vs non-"Hack" Periods
```{r, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center'}
Pace_Hack=round(c(REGPPM_C, HPPM_C,REGPPM_CO, HPPM_CO, REGPPM_H, HPPM_H, REGPPM_HO, HPPM_HO, REGPPM_D, HPPM_D, REGPPM_DO, HPPM_DO), digits= 3)
names2=c("LAC","LAC_H", "LAC_O","LAC_O_H", "HOU","HOU_H", "HOU_O","HOU_O_H", "DET","DET_H", "DET_O", "DET_O_H")

HACK_PACE=bind_cols(as.data.frame(Pace_Hack),as.data.frame(names2))

#Pace in Hack time
ggplot(HACK_PACE, aes(x = factor(names2),y = Pace_Hack)) + geom_bar(aes(fill = names2),stat = "identity") + geom_text(aes(label = Pace_Hack, y = Pace_Hack), size = 3)+xlab("Team's name")+ylab("Pace measured by score per min")+scale_fill_manual(values =  c("#009E73","#009E73","#66FFCC","#66FFCC","#CC0033","#CC0033","#CC9999","#CC9999","#0072B2","#0072B2","#99CCFF","#99CCFF"))

```

<br><br>
<br><br>
<br><br>
<br><br>

<center> <h1> LOS ANGELES CLIPPERS </h1> </center>

<br><br>

## Two-Minute Score Margins

* 169 two-minute intervals were identified as "hacking""
* 1717 two-minute intervals were identified as not "hacking""

```{r,results='asis',tidy=FALSE, echo=FALSE, fig.align='center',message=FALSE,warning=FALSE}

t_plot = TWOMINS1
t_plot$HACKINT[t_plot$HACKINT == TRUE] = "Hack"
t_plot$HACKINT[t_plot$HACKINT == FALSE] = "No Hack"

a = ggplot(TWOMINS1, aes(SM, fill = HACKINT)) + 
  geom_histogram(binwidth = 1, position = 'identity') +
  theme(legend.position="none")+
  labs(x = 'Two Minute Score Margin', y = 'Frequency')+
  scale_fill_manual(values = c("#CC6666", "#9999CC"))

hack = subset(TWOMINS1, HACKINT == TRUE)$SM
no_hack  = subset(TWOMINS1, HACKINT == FALSE)$SM
no_hack_mean = round(mean(no_hack),2)
hack_mean = round(mean(hack),2)
#print(length(hack))
#print(length(no_hack))

b = ggplot(t_plot, aes(HACKINT, SM)) + 
  geom_boxplot(aes(fill = factor(HACKINT))) + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs( y = 'Two Minute Score Margin')+
  annotate("text",x = 1.45,y = hack_mean-0.5,label = paste("mean:",hack_mean))+
  annotate("text",x = 2.45,y = no_hack_mean-0.5,label = paste("mean:",no_hack_mean))+ 
  theme(axis.title.y = element_blank())+
  scale_fill_manual(values = c("#9999CC", "#CC6666"))

#summary(subset(TWOMINS1, HACKINT == TRUE)$SM)
#summary(subset(TWOMINS1, HACKINT == FALSE)$SM)
  MainTitle = "LA Clippers 2014 - 2015 (Jordan)"
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(0.5, 5), "null"))))
  grid.text(MainTitle, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(a, vp = viewport(layout.pos.row = 2, layout.pos.col = 1),newpage=FALSE)
  print(b, vp = viewport(layout.pos.row = 2, layout.pos.col = 2),newpage=FALSE)
  popViewport(1)

```

t-test results

* t: 1.8756  
* p-value: 0.06211  
* Degrees of Freedom: 209.19  
* 95% Confidence Intervals:  -0.0241699  0.9705684  

<br><br>
<br><br>

## Team Network Visualization

```{r,results='hide',tidy=FALSE, echo=FALSE, fig.align='center',message=FALSE,warning=FALSE}
########################### NETWORK
  
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(igraph)
setwd("/Users/sunxiaohan/Desktop/github/finalproject-p5-team3")


load('Data/teamDataf.Rdata')

# Load the data
gamedata=readRDS('data/Game_Summary_HAS.RDS')
gamedata=gamedata[,c('HTEAM_ID','HTEAM_ABBREVIATION','VTEAM_ID','VTEAM_ABBREVIATION','Hack','HAS_Count')]

hackdata=filter(gamedata,Hack==1)

LAC=filter(hackdata,HTEAM_ABBREVIATION=='LAC'|VTEAM_ABBREVIATION=='LAC')
LAC['OPP']=LAC$HTEAM_ABBREVIATION
ind=(LAC$OPP=='LAC')
LAC=as.matrix(LAC)
LAC[ind,'OPP']=LAC[ind,'VTEAM_ABBREVIATION']
LAC=as.data.frame(LAC)


LAC.new=LAC[,c('HAS_Count','OPP')]
LAC.new$HAS_Count=as.numeric(LAC.new$HAS_Count)

LAC.new=aggregate(LAC.new[,1]~LAC.new[,2],data=LAC.new,FUN=mean)



nodes3=as.data.frame(matrix(nrow=nrow(LAC.new)+1))
nodes3['id']=c('LAC',as.character(LAC.new[,1]))
nodes3=nodes3[,-1]

links3=as.data.frame(matrix(nrow=nrow(LAC.new)))
links3['from']='LAC'
links3['to']=LAC.new$`LAC.new[, 2]`
links3=links3[,-1]
links3['weight']=LAC.new$`LAC.new[, 1]`


net3 <- graph.data.frame(links3, nodes3, directed=T)
V(net3)$size=c(10,links3$weight*8)



V(net3)$color='light blue'


# Set edge width based on weight:
E(net3)$width <- E(net3)$weight/6

#change arrow size and edge color:
E(net3)$arrow.size <- .2
E(net3)$edge.color <- "gray80"
E(net3)$width <- 1+E(net3)$weight/12
E(net3)$length=links3$weight*10

# plot(net3,vertex.label.family="Arial Black",main='HAS for LAC') 

##################t test
# ttest_la = t.test(hack,no_hack)
# print(ttest_la)
```
  
<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="figs/network_LAC.png" float = "center" ALT="image" width="800">
  </span>
</div>

<br><br>
<br><br>
<br><br>
<br><br>

<center> <h1> DETROIT PISTONS </h1> </center>

<br><br>

## Two-Minute Score Margins

* 154 two-minute intervals were identified as "hacking""
* 1732 two-minute intervals were identified as not "hacking""

```{r,results='asis',tidy=FALSE, echo=FALSE, fig.align='center'}

t_plot = TWOMINS2
t_plot$HACKINT[t_plot$HACKINT == TRUE] = "Hack"
t_plot$HACKINT[t_plot$HACKINT == FALSE] = "No Hack"

a = ggplot(TWOMINS2, aes(SM, fill = HACKINT)) + 
  geom_histogram(binwidth = 1, position = 'identity') +
  theme(legend.position="none")+
  labs(x = 'Two Minute Score Margin', y = 'Frequency')+
  scale_fill_manual(values = c("#CC6666", "#9999CC"))

hack = subset(TWOMINS2, HACKINT == TRUE)$SM
no_hack  = subset(TWOMINS2, HACKINT == FALSE)$SM
no_hack_mean = round(mean(no_hack),2)
hack_mean = round(mean(hack),2)
#print(length(hack))
#print(length(no_hack))

b = ggplot(t_plot, aes(HACKINT, SM)) + 
  geom_boxplot(aes(fill = factor(HACKINT))) + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs( y = 'Two Minute Score Margin')+
  annotate("text",x = 1.45,y = hack_mean-0.5,label = paste("mean:",hack_mean))+
  annotate("text",x = 2.45,y = no_hack_mean-0.5,label = paste("mean:",no_hack_mean))+ 
  theme(axis.title.y = element_blank())+
  scale_fill_manual(values = c("#9999CC", "#CC6666"))

#summary(subset(TWOMINS1, HACKINT == TRUE)$SM)
#summary(subset(TWOMINS1, HACKINT == FALSE)$SM)
  MainTitle = "Detroit Pisons 2014 - 2015 (Drummond)"
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(0.5, 5), "null"))))
  grid.text(MainTitle, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(a, vp = viewport(layout.pos.row = 2, layout.pos.col = 1),newpage=FALSE)
  print(b, vp = viewport(layout.pos.row = 2, layout.pos.col = 2),newpage=FALSE)
  popViewport(1)

```

t-test results:

* t: 2.4296  
* p-value: 0.01591  
* degrees of freedom: 222.17  
* 95% Confidence Intervals:  0.1125442 1.0791765  

<br><br>
<br><br>

## Team Network Visualization

```{r,results='hide',tidy=FALSE, echo=FALSE, fig.align='center',message=FALSE,warning=FALSE}
###########network plot
DET=filter(hackdata,HTEAM_ABBREVIATION=='DET'|VTEAM_ABBREVIATION=='DET')
DET['OPP']=DET$HTEAM_ABBREVIATION
ind=(DET$OPP=='DET')
DET=as.matrix(DET)
DET[ind,'OPP']=DET[ind,'VTEAM_ABBREVIATION']
DET=as.data.frame(DET)


DET.new=DET[,c('HAS_Count','OPP')]
DET.new$HAS_Count=as.numeric(DET.new$HAS_Count)

DET.new=aggregate(DET.new[,1]~DET.new[,2],data=DET.new,FUN=mean)



nodes1=as.data.frame(matrix(nrow=nrow(DET.new)+1))
nodes1=nodes1[,-1]
nodes1['id']=c('DET',as.character(DET.new[,1]))
nodes1['group']='LL'
nodes1['weight']=c(100,DET.new$`DET.new[, 1]`*50)

links1=as.data.frame(matrix(nrow=nrow(DET.new)))
links1['from']='DET'
links1['to']=DET.new$`DET.new[, 2]`
links1=links1[,-1]
links1['weight']=DET.new$`DET.new[, 1]`





net1 <- graph.data.frame(links1, nodes1, directed=T)
V(net1)$size=c(10,links1$weight*10)



V(net1)$color='light blue'


# Set edge width based on weight:
E(net1)$width <- E(net1)$weight/6

#change arrow size and edge color:
E(net1)$arrow.size <- .2
E(net1)$edge.color <- "gray80"
E(net1)$width <- 1+E(net1)$weight/12
E(net1)$length=links1$weight*10

# plot(net1,vertex.label.family="Arial Black",main='HAS for DET') 

##########################t test
  
# ttest_det = t.test(hack,no_hack)
# print(ttest_det)


```

<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="figs/network_DET.png" float = "center" ALT="image" width="800">
  </span>
</div>

<br><br>
<br><br>
<br><br>
<br><br>

<center> <h1> HOUSTON ROCKETS </h1> </center>

<br><br>

## Two-Minute Score Margins

* 170 two-minute intervals were identified as "hacking""
* 1716 two-minute intervals were identified as not "hacking""

```{r,results='asis',tidy=FALSE, echo=FALSE, fig.align='center'}

t_plot = TWOMINS3
t_plot$HACKINT[t_plot$HACKINT == TRUE] = "Hack"
t_plot$HACKINT[t_plot$HACKINT == FALSE] = "No Hack"

a = ggplot(TWOMINS3, aes(SM, fill = HACKINT)) + 
  geom_histogram(binwidth = 1, position = 'identity') +
  theme(legend.position="none")+
  labs(x = 'Two Minute Score Margin', y = 'Frequency')+
  scale_fill_manual(values = c("#CC6666", "#9999CC"))

hack = subset(TWOMINS3, HACKINT == TRUE)$SM
no_hack  = subset(TWOMINS3, HACKINT == FALSE)$SM
no_hack_mean = round(mean(no_hack),2)
hack_mean = round(mean(hack),2)
print(length(hack))
print(length(no_hack))

b = ggplot(t_plot, aes(HACKINT, SM)) + 
  geom_boxplot(aes(fill = factor(HACKINT))) + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs( y = 'Two Minute Score Margin')+
  annotate("text",x = 1.45,y = hack_mean-0.5,label = paste("mean:",hack_mean))+
  annotate("text",x = 2.45,y = no_hack_mean-0.5,label = paste("mean:",no_hack_mean))+ 
  theme(axis.title.y = element_blank())+
  scale_fill_manual(values = c("#9999CC", "#CC6666"))

#summary(subset(TWOMINS1, HACKINT == TRUE)$SM)
#summary(subset(TWOMINS1, HACKINT == FALSE)$SM)
  MainTitle = "Houston Rockets 2014 - 2015 (Howard and Rice)"
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(0.5, 5), "null"))))
  grid.text(MainTitle, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(a, vp = viewport(layout.pos.row = 2, layout.pos.col = 1),newpage=FALSE)
  print(b, vp = viewport(layout.pos.row = 2, layout.pos.col = 2),newpage=FALSE)
  popViewport(1)

```

t-test results:
* t: 1.7703  
* p-value: 0.07836  
* Degrees of Freedom: 181.21  
* 95% Confidence Intervals: -0.05876789  1.08453459  

<br><br>
<br><br>

## Team Network Visualization

```{r,results='hide',tidy=FALSE, echo=FALSE, fig.align='center',message=FALSE,warning=FALSE}
################network plot
HOU=filter(hackdata,HTEAM_ABBREVIATION=='HOU'|VTEAM_ABBREVIATION=='HOU')
HOU['OPP']=HOU$HTEAM_ABBREVIATION
ind=(HOU$OPP=='HOU')
HOU=as.matrix(HOU)
HOU[ind,'OPP']=HOU[ind,'VTEAM_ABBREVIATION']
HOU=as.data.frame(HOU)


HOU.new=HOU[,c('HAS_Count','OPP')]
HOU.new$HAS_Count=as.numeric(HOU.new$HAS_Count)

HOU.new=aggregate(HOU.new[,1]~HOU.new[,2],data=HOU.new,FUN=mean)



nodes2=as.data.frame(matrix(nrow=nrow(HOU.new)+1))
nodes2['id']=c('HOU',as.character(HOU.new[,1]))
nodes2=nodes2[,-1]

links2=as.data.frame(matrix(nrow=nrow(HOU.new)))
links2['from']='HOU'
links2['to']=HOU.new$`HOU.new[, 2]`
links2=links2[,-1]
links2['weight']=HOU.new$`HOU.new[, 1]`


net2 <- graph.data.frame(links2, nodes2, directed=T)
V(net2)$size=c(10,links2$weight*8)



V(net2)$color='light blue'


# Set edge width based on weight:
E(net2)$width <- E(net2)$weight/6

#change arrow size and edge color:
E(net2)$arrow.size <- .2
E(net2)$edge.color <- "gray80"
E(net2)$width <- 1+E(net2)$weight/12
E(net2)$length=links2$weight*10

# plot(net2,vertex.label.family="Arial Black",main='HOU')


#######################t test
  
# ttest_hou = t.test(hack,no_hack)
# print(ttest_hou)



```

<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="figs/network_HOU.png" float = "center" ALT="image" width="800">
  </span>
</div>

<br><br>
<br><br>
<br><br>
<br><br>

# An optimist coach might say, "Let's sub the player out!"
## ... Is subbing out those bad free throwers worth it?

```{r, , echo=FALSE}

#Jordan
trial2 <- matrix(c(117.6, 101.2, 105.8, 104.8), ncol=2)
rownames(trial2) <- c('Jordan on court', 'Jordan off court')
colnames(trial2) <- c('Clippers score per 100 rounds', 'Opponent score per 100 rounds')
trial.table2 <- as.table(trial2);trial.table2
#Howard
trial3 <- matrix(c(108.7, 106.4, 100.2, 104.9), ncol=2)
rownames(trial3) <- c('Howard on court', 'Howard off court')
colnames(trial3) <- c('Rockets score per 100 rounds', 'Opponent score per 100 rounds')
trial.table3 <- as.table(trial3);trial.table3
#Smith
trial4 <- matrix(c(109.2, 105.7, 101.4, 104.7), ncol=2)
rownames(trial4) <- c('Smith on court', 'Smith off court')
colnames(trial4) <- c('Rockets score per 100 rounds', 'Opponent score per 100 rounds')
trial.table4 <- as.table(trial4);trial.table4
#Drummond
trial5 <- matrix(c(106.5, 103.2, 108.2, 104.3), ncol=2)
rownames(trial5) <- c('Drummond on', 'Drummond off')
colnames(trial5) <- c('Pistons score per 100 rounds', 'Opponent score per 100 rounds')
trial.table5 <- as.table(trial5);trial.table5

```

<br><br>
<br><br>
<br><br>
<br><br>

# Does "hacking" benefit the using team's key players?

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

## ... San Antonio Spurs' Tim Duncan sure thinks so!
<center> <h3> (During Hack Periods) </h3> </center>

```{r, echo=FALSE, fig.align='center'}
duncan_reg=0.513

duncan=round(c(duncan_reg,duncan_per, H_duncan_per, D_duncan_per), digits= 3)

lib3=c("Duncan Average", "Against Clippers","Against Rockets", "Against Pistons")
tim_per=bind_cols(as.data.frame(duncan),as.data.frame(lib3))

#Tim Duncan shooting percentage
ggplot(tim_per, aes(x = factor(lib3),y = duncan,fill=lib3)) + geom_bar(stat = "identity") + geom_text(aes(label = duncan, y = duncan), size = 5)+xlab("Tim Duncan")+ylab("Shooting percentage")+
  scale_fill_manual(values =  c("#0072B2","#009E73","#CC0033",'grey'))
 

```

<br><br>
<br><br>
<br><br>
<br><br>

# So finally... Does "hacking" bring in wins?

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

Win_total=round(c(Cwin_per, Rwin_per, Dwin_per), digits= 3)

lib1=c("LAC", "HOU","DET")
wins=bind_cols(as.data.frame(Win_total),as.data.frame(lib1))

#Wining percent in the whole season
# ggplot(wins, aes(x = factor(lib1),y = Win_total,fill = lib1)) + geom_bar(stat = "identity") + geom_text(aes(label = Win_total, y = Win_total), size = 5)+xlab("Team")+ylab("Regular Winning percentage")+scale_fill_manual(values =  c("#009E73","#CC0033","#0072B2"))
```

<center> <h2> Hacked Games vs Regular Games </h2> </center>
```{r, echo=FALSE, results="hide", warning=FALSE, message=FALSE}

#Games be hacked

GamesHack=round(c(Cwin_per, CHwin_per, Rwin_per, RHwin_per, Dwin_per, DHwin_per), digits= 3)

lib2=c("LAC_win_per", "LAC_Hack_win_per", "HOU_win_per","HOU_Hack_win_per", "DET_win_per", "DET_Hack_win_per")
hwins=bind_cols(as.data.frame(GamesHack),as.data.frame(lib2))
```

```{r, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center'}
#Winning percent in the hacked game
ggplot(hwins, aes(x = factor(lib2),y = GamesHack, fill=lib2)) + geom_bar(stat = "identity") + geom_text(aes(label = GamesHack, y = GamesHack), size = 5)+xlab("Team")+ylab("Winning percentage compare with hack")+scale_fill_manual(values =  c("#009E73","#009E73","#CC0033","#CC0033", "#0072B2","#0072B2"))

```

<br><br>
<br><br>

## Can hacking *ever* guarantee a win?
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

```{r,echo=FALSE}
#Winning strategy in last 2 mintues
#Three ways to compare. First, filter the losing game and get the mean of losing game margin. Second, get the last 2 min interval of the score margin for each team to compare the difference. Third, based on the score margin of each team in last two minutes, get the 95% distribution of leading points at the begining of last two minutes.
trial <- matrix(c(C_lead_2min,R_lead_2min,D_lead_2min,C_Margin_2, R_Margin_2, D_Margin_2, Clead, Hlead, Dlead), ncol=3)
rownames(trial) <- c('Clippers', 'Rockets', "Pistons")
colnames(trial) <- c('Mean Game Lose Margin', 'Mean differential', "95% to win")
trial.table <- as.table(trial);trial.table

```

<br><br>
<br><br>
<br><br>
<br><br>
<br><br>
<br><br>

# Conclusion

## Do we recommend Hack-a-Shaq?
Honestly, no. 

* The greatest piece of evidence comes from the win percentages, but all of our tests have shown that there is no significant difference in the results of "hacking" periods and non-"hacking" periods. Wins are wins, and good players are good players.
* One theory common throughout the sports community is that the centers who get "hacked", such as Jordan and Howard, are just too good at what they're good at. How much they contribute to the game while they're on the court actually overshadows the missed free throws - we now have proof of this from examining the points per 100 rounds. 
* Morale: Intentionally fouling players that are bad at free throws has gotten a lot of bad press from both players, coaches, news casters, and basketball fans in general. A lot of people argue that it's not in the spirit of the game to exploit such a weakness. Shaq himself has been known to say, "C'mon. Let's play ball, not strategy." For basketball (and NBA) the business, why not hack-a-shaq? But for basketball the game, hack-a-shaq ruins the fun.
* Business: Maybe Hack-a-Shaq can win a game every once in a while, but there are greater hidden costs that most people don't realize. The more fouls and free throws in a game, the longer a game drags on. TV networks and advertisers end up having to change their programmed scheduling, and this costs big $$. 

### You don't want to be this guy.

<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="figs/popovich.jpg" float = "center" ALT="image" width="800">
  </span>
</div>

<br><br>
<br><br>

## And finally... NBA API
We have provided for everyone an easy, accessible, and convenient way to scrape the NBA API for future projects. Find our scraping code on our project page: <https://github.com/TZstatsADS/finalproject-p5-team3/tree/master/NBA_API> Enjoy!

<span style="float:middle;width: 30%;">
  <IMG SRC="figs/API.png" float = "right" ALT="image">
  </span>
    
