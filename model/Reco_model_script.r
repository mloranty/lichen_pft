################################################
### Code to organize data and send to model#####
### for Reco fluxes                        #####
### this version will calculate flux values#####
### in model code rather than assume they  #####
### are a fixed value with no variation    #####
################################################
#packages
library(plyr)
library(rjags)
library(lubridate)


setwd("c:\\Users\\hkropp\\Google Drive\\lichen")
#read in raw data
in.files <- list.files(path=paste0(getwd(),"/raw"), full.names=T)
#get plot name
#get rid of ".txt"
pID<-gsub(".txt","",in.files)


#read in final flux times determined by Mike Loranty
datFT<-read.csv("flux.rates.final.2017-06-02.csv")

#read in met data
datM<-read.csv("met.flux.csv")

#read in flux data and subset by the appropriate time increment
dat<-list()
datC<-list()
datH<-list()
dimC<-numeric(0)
dimH<-numeric(0)
pnum<-numeric(0)
readout<-character(0)
pTime<-character(0)
pdate<-character(0)
for(i in 1:length(in.files)){
  dat[[i]] <- read.table(file=in.files[i],header=T,skip=1)
  #organize into CO2 data frame
  datC[[i]]<-dat[[i]][datFT$CO2.start[i]:datFT$CO2.finish[i],1:3]
  datC[[i]]$siteid<-rep(i,dim(datC[[i]])[1])
  dimC[i]<-dim(datC[[i]])[1]
  #add water data frame too
  datH[[i]]<-dat[[i]][datFT$H2O.start[i]:datFT$H2O.finish[i],1:4]
  datH[[i]]$siteid<-rep(i,dim(datH[[i]])[1])
  dimH[i]<-dim(datC[[i]])[1]
  #get the plot numbers
  pnum[i]<-as.numeric(gsub("[A-z]","",strsplit(pID[i], "plot")[[1]][2]))
  #get the start time for the met data
  readout[i]<-read.table(file=in.files[i],nrows=1,stringsAsFactors=FALSE)[1]
  pTime[i]<-strsplit(readout[[i]][1], "at ")[[1]][2]
  #get plot day of year
  pdate[i]<-strsplit(readout[[i]][1], " at ")[[1]][1]
 }
 
#turn fluxes back into a data table
Cflux<-ldply(datC,data.frame)
Hflux<-ldply(datH,data.frame)



#create a plot info table
PlotI<-data.frame(siteid=seq(1,length(in.files)))
#now get matching time and plot id
PlotI$plot<-pnum	
PlotI$time<-pTime	
#convert date
dateplot<-as.Date(pdate,"%Y-%m-%d")	
PlotI$doy<-yday(dateplot)	
PlotI$dimC<-dimC
PlotI$dimH<-dimH

#now join to fluxes
CfluxA<-join(Cflux,PlotI, by="siteid", type="left")
HfluxA<-join(Hflux,PlotI, by="siteid", type="left")

#convert dates for datM
dateM<-as.Date(datM$date, "%m/%d/%y")
datM$doy<-yday(dateM)

#now join to see how many plots match

plotMatch<-join(PlotI, datM, by=c("time","doy","plot"), type="right")
