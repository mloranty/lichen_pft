################################
# read and plot soil temp
# data from PFT plot HOBOs
#
# MML 29 Oct 2013
# updated 28 June 2014 - MML
# updated 28 May 2017 - MML ahh!
################################
require('plyr')
require('xlsx')
require('lubridate')
rm(list=ls())

setwd("/Users/mloranty/Documents/GitHub/lichen_pft/hobo_pft/")
# list all of the collated csv files
f <- list.files(pattern=".csv")
# read first file 
temp <- read.csv(f[1],skip=2,header=F,stringsAsFactors=F)[,1:3]
# set var names
names(temp) <- c("rec","date",f[1])
# get rid of seconds in timestamp
temp$date <- substr(temp$date,1,14)

# read in rest of files and append to first data frame
for(i in 2:length(f))
{
  t <- read.csv(f[i],skip=2,header=F)[,2:3]
  names(t) <- c("date",f[i])
  t$date <- substr(t$date,1,14)
  temp <- join(temp,t,by="date",type="full")
}
rm(t)

## get rid of data logged before sensor installation on 7/24/2012
temp <- temp[521:17392,]

#format date and time#
temp$date <- strptime(temp$date,format="%m/%d/%y %H:%M")

#read data on sensor placement and veg condition#
veg <- read.csv("/Users/mloranty/Documents/GitHub/lichen_pft/hobo_sensor_info.csv",header=T)

#put temp back in chronological order#
# ord <- match(sort(temp$rec),temp$rec)
# temp <- temp[ord,]
# temp$mean <- rowMeans(temp[,3:21],na.rm=T)
# write.csv(temp,file="temp_daily.csv")

#aggregate to daily & format daily date#
temp.daily <- aggregate(temp[,3:20],by=list(substr(temp$date,1,10)),
                        FUN=mean,na.rm=T)
temp.daily$date <- strptime(temp.daily$Group.1,format="%Y-%m-%d")

#aggregate to monthly#
temp.monthly <- aggregate(temp.daily[,2:19],by=list(substr(temp.daily$Group.1,1,7)),
                                   FUN=mean,na.rm=T)
temp.monthly$date <- seq(as.POSIXlt("2012-07-15"),as.POSIXlt("2014-06-15"),"month")


###sort and aggregate by vegetation type ###
lichen <- veg[which(veg$Veg == "L"),]
shrub <- veg[which(veg$Veg == "SM"),]

# calculate hourly temp mean and sd for lichen/shrub
l.rec <- na.omit(match(lichen$Serial.No.,as.numeric(substr(names(temp),1,7))))
s.rec <- na.omit(match(shrub$Serial.No.,as.numeric(substr(names(temp),1,7))))
temp$lichen.mean <- apply(temp[,l.rec],1,FUN=mean,na.rm=T)
temp$lichen.sd <- apply(temp[,l.rec],1,FUN=sd,na.rm=T)
temp$shrub.mean <- apply(temp[,s.rec],1,FUN=mean,na.rm=T)
temp$shrub.sd <- apply(temp[,s.rec],1,FUN=sd,na.rm=T)
#get date/time components for plot
temp$hour <- hour(temp$date)
temp$day <- ymd(substr(temp$date,1,10))
temp$dif <- temp$lichen.mean-temp$shrub.mean

# calculate daily temp mean and sd for lichen/shrub
l.rec <- na.omit(match(lichen$Serial.No.,as.numeric(substr(names(temp.daily),1,7))))
s.rec <- na.omit(match(shrub$Serial.No.,as.numeric(substr(names(temp.daily),1,7))))
temp.daily$lichen.mean <- apply(temp.daily[,l.rec],1,FUN=mean,na.rm=T)
temp.daily$lichen.sd <- apply(temp.daily[,l.rec],1,FUN=sd,na.rm=T)
temp.daily$shrub.mean <- apply(temp.daily[,s.rec],1,FUN=mean,na.rm=T)
temp.daily$shrub.sd <- apply(temp.daily[,s.rec],1,FUN=sd,na.rm=T)

# calculate monthly temp mean and sd for lichen/shrub
l.rec <- na.omit(match(lichen$Serial.No.,as.numeric(substr(names(temp.monthly),1,7))))
s.rec <- na.omit(match(shrub$Serial.No.,as.numeric(substr(names(temp.monthly),1,7))))
temp.monthly$lichen.mean <- apply(temp.monthly[,l.rec],1,FUN=mean,na.rm=T)
temp.monthly$lichen.sd <- apply(temp.monthly[,l.rec],1,FUN=sd,na.rm=T)
temp.monthly$shrub.mean <- apply(temp.monthly[,s.rec],1,FUN=mean,na.rm=T)
temp.monthly$shrub.sd <- apply(temp.monthly[,s.rec],1,FUN=sd,na.rm=T)

# plot data for the paper

tiff(file="/Users/mloranty/Documents/GitHub/lichen_pft/paper_figures/figure4.tiff",
     width=10,height=5,units="in",res=300,compression="lzw",bg="white")
par(cex.lab=1.25,cex.axis=1.25,mar=c(0,6,3,2),mfcol=c(2,1))

plot(temp.daily$date,temp.daily$lichen.mean,type="l",lwd=1.5,
     ylab="",yaxt="n",
     xlab="",xaxt="n",col="red",ylim=c(-14,8))
axis.POSIXct(1,at=seq(temp.monthly$date[1],temp.monthly$date[24],by="month"),format="%b",labels=F)
axis(side = 2,labels=T,tick=T,las=2,cex.axis=1.5)
mtext(expression(paste(T[soil]," (",C*degree,")")),side=2,cex=1.5,line=3)
lines(temp.daily$date,temp.daily$shrub.mean,type="l",lwd=2,col="black")
lines(temp.daily$date,temp.daily$shrub.mean-temp.daily$shrub.sd,lwd=1,col="black",lty='dotted')
lines(temp.daily$date,temp.daily$shrub.mean+temp.daily$shrub.sd,lwd=1,col="black",lty='dotted')
lines(temp.daily$date,temp.daily$lichen.mean+temp.daily$lichen.sd,lwd=1,col="red",lty='dotted')
lines(temp.daily$date,temp.daily$lichen.mean-temp.daily$lichen.sd,lwd=1,col="red",lty='dotted')
# polygon(c(temp.daily$date,rev(temp.daily$date)),
#         c(temp.daily$shrub.mean+temp.daily$shrub.sd,rev(temp.daily$shrub.mean-temp.daily$shrub.sd)),
#         col="red")
# polygon(c(temp.daily$date,rev(temp.daily$date)),
#         c(temp.daily$lichen.mean+temp.daily$lichen.sd,rev(temp.daily$lichen.mean-temp.daily$lichen.sd)),
#         density = -1,col="lavender",border=NA)

legend("bottomleft",c("Lichen","Shrub"),col=c("red","black"),lwd=2,bty="n",cex=1.25)
legend("topright","a",bty="n",cex=1.25)

## plot the difference between lichen and shrub
par(cex.lab=1.25,cex.axis=1.25,mar=c(3,6,0,2),mfcol=c(2,1),new=T)
plot(temp.daily$date,temp.daily$lichen.mean-temp.daily$shrub.mean,type="l",lwd=2,
     ylab="",yaxt="n",
     xlab="",xaxt="n",ylim=c(-0.5,4.5))
axis.POSIXct(1,at=seq(temp.monthly$date[1],temp.monthly$date[24],by="month"),format="%b",labels=TRUE)
axis(side = 2,labels=T,tick=T,las=2,cex.axis=1.5)
mtext(expression(paste(T[dif]," (",C*degree,")")),side=2,cex=1.5,line=3)
legend("topright","b",bty="n",cex=1.25)
dev.off()

