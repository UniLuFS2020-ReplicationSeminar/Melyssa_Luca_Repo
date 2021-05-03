##########################################

# Melyssa - replication text
# May 3th, 2021

##########################################
# HOW POLITICAL PARTIES SHAPE PUBLIC OPINION IN THE REAL WORLD
# AUTHORS: Martin Bisgaard & Rune Slothuus
# VERSION: SEPTEMBER 2020

#Install packages

install.packages("rio")
install.packages("foreign")
install.packages("car")
install.packages("plyr")
install.packages("plm")
install.packages("lme4")
install.packages("lmtest")
install.packages("stargazer")
install.packages("xtable")
install.packages("psych")
install.packages("grDevices")
install.packages("plyr")
install.packages("reshape2")
install.packages("pBrackets")
install.packages("MatchIt")
install.packages("sandwich")

require(rio)
require(foreign)
require(car)
require(plyr)
require(plm)
require(lme4)
require(lmtest)
require(stargazer)
require(xtable)
require(psych)
require(grDevices)
require(plyr)
require(reshape2)
require(pBrackets)
require(MatchIt)
require(sandwich)

#Set up WD

rm(list=ls())
getwd()
setwd("C:/Users/Melyssa Pina/Desktop/Melyssa_Luca_Repo")

#Import data set
paneldata_long <- read_dta("Problem Set 4/01 Data/paneldata_long.dta")
paneldata_wide <- read_dta("Problem Set 4/01 Data/paneldata_wide.dta")
partypositions <- read_sav("Problem Set 4/01 Data/partypositions.sav")

#Analysis of party positions
mediadata <- partypositions
dim(mediadata)
length(unique(mediadata$ID))
sum(table(unlist(mediadata[,22:28])))
retiredata <- mediadata[mediadata$issue==2,]
unempdata <- mediadata[mediadata$issue==1,]
dim(retiredata)
names(retiredata)
dim(unempdata)
names(unempdata)
Vpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posVny,"party"="V")
DPPpos<- data.frame("tid"=unempdata$tid,"pos"=unempdata$posDFny,"party"="DF")
treatment<-rbind(Vpos,DPPpos)
SFpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posSny,"party"="S")
Spos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posSFny,"party"="SF")
ELpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posELny,"party"="EL")
centerLeft<-rbind(SFpos,Spos)
RVpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posRVny,"party"="RV")
Kpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posKny,"party"="K")
centerRight<-rbind(RVpos,Kpos)
treatment$date <- as.Date(as.character(treatment$tid),"%Y%m%d")
centerLeft$date <- as.Date(as.character(centerLeft$tid),"%Y%m%d") 
centerRight$date <- as.Date(as.character(centerRight$tid),"%Y%m%d") 
treatment$pos[treatment$pos==0]<-NA
centerLeft$pos[centerLeft$pos==0]<-NA
centerRight$pos[centerRight$pos==0]<-NA
treatment	<-	na.omit(treatment) 
centerLeft	<-	na.omit(centerLeft)
centerRight	<-	na.omit(centerRight)
dim(treatment)
dim(centerRight)

#Unemployment benefit period
pdf("benefitsCue.pdf",width=6,height=4)
cols1=rgb(.1,.1,1,alpha=.175)
cols2=rgb(1,.1,.1,alpha=.175)
cols3=rgb(0,.1,0,alpha=.1)
par(omd=c(.1,1,0,1))
plot(treatment$date,jitter(treatment$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5),xlim=c(as.Date("2010-01-01"),as.Date("2011-03-01")),xaxt='n')
axis(2,at=1:5,las=2,labels=c("Oppose","Partly\noppose","Ambiguous","Partly\nsupport","Support"),cex.axis=.8)
m<-smooth.spline(treatment$pos~treatment$date,spar=1)
lines(m,lwd=1.5)
m<-smooth.spline(centerLeft$pos~centerLeft$date,spar=1)
lines(m,lwd=1.5,col="grey60")
m<-smooth.spline(centerRight$pos~centerRight$date,spar=1)
lines(m,lwd=1.5,lty=2,col="grey60")
text(x=as.Date("2010-06-01"),y=3.4,label="Liberals\nDPP",cex=.8)
text(x=as.Date("2010-10-01"),y=2.4,label="Social Democrats\nSPP",cex=.8,col="grey60")
text(x=as.Date("2010-03-15"),y=5,label="Conservatives\nSocial Liberals",cex=.8,col="grey60")
rng<-par("usr")
rect(15015,rng[3],rng[2],rng[4],col="grey90")
mtext(4,line=-1.35,text="Reduce Unemp. Benefits")
dens<-c(centerRight$date,centerLeft$date,treatment$date)
rug(dens)
dev.off()

#Party Postions on other issue: Abolishing early retirement
treatment <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posVny,"party"="V")
SFpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posSny,"party"="S")
Spos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posSFny,"party"="SF")
DPPpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posDFny,"party"="DF")
centerLeft<-rbind(SFpos,Spos,DPPpos)
RVpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posRVny,"party"="RV")
Kpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posKny,"party"="K")
centerRight<-rbind(RVpos,Kpos)
centerLeft$date <- as.Date(as.character(centerLeft$tid),"%Y%m%d") 
centerRight$date <- as.Date(as.character(centerRight$tid),"%Y%m%d") 
treatment$date<-as.Date(as.character(treatment$tid),"%Y%m%d")
treatment<-na.omit(treatment)
centerLeft	<-	na.omit(centerLeft) 
centerRight	<-	na.omit(centerRight) 
centerLeft$pos[centerLeft$pos==0]<-NA
centerRight$pos[centerRight$pos==0]<-NA
treatment$pos[treatment$pos==0]<-NA
cols3=rgb(0,.1,0,alpha=.1)

pdf("retireCue.pdf",width=6,height=4)
cols1=rgb(.1,.1,1,alpha=.175)
cols2=rgb(1,.1,.1,alpha=.175)
par(omd=c(.1,1,0,1))
plot(treatment$date,jitter(treatment$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5),xlim=c(as.Date("2010-01-01"),as.Date("2011-03-01")),xaxt='n')
axis(2,at=1:5,las=2,labels=c("Oppose","Partly\noppose","Ambiguous","Partly\nsupport","Support"),cex.axis=.8)
axis.Date(1,at=seq(as.Date("2010-01-01"),as.Date("2011-01-01"), by="month"), format="%b",las=1,cex.axis=.8)
axis.Date(1,at=c(as.Date("2010-01-01"),as.Date("2011-01-01")), format="%Y",las=1,line=1,tick="FALSE",cex.axis=.8)
m<-smooth.spline(treatment$pos~treatment$date,spar=1)
lines(m,lwd=1.5)
m<-smooth.spline(centerLeft$pos~centerLeft$date,spar=1)
lines(m,lwd=1.5,col="grey60")
m<-smooth.spline(centerRight$pos~centerRight$date,spar=1)
lines(m,lwd=1.5,col="grey60",lty=2)
rng<-par("usr")
rect(15015,rng[3],rng[2],rng[4],col="grey90")
mtext(4,line=-1.35,text="Abolish Early Retirement")
text(x=as.Date("2010-11-01"),y=3.5,label="Liberals",cex=.8)
text(x=as.Date("2010-07-01"),y=1,label="Social Democrats\nSPP and DPP",cex=.8,col="grey60")
text(x=as.Date("2010-5-01"),y=5,label="Conservatives\nSocial Liberals",cex=.8,col="grey60")
dens<-c(centerRight$date,centerLeft$date,treatment$date)
rug(dens)
dev.off()



