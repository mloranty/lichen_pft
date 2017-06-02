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

setwd("/Users/mloranty/Google Drive/Documents/Research/NSFBorealFireCherskii_2014-17/Loranty_data/EXB_pft_hobo_Tsoil_ML/all/")

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
veg <- read.csv("/Users/mloranty/Google Drive/Documents/Research/NSFBorealFireCherskii_2014-17/Loranty_data/EXB_pft_hobo_Tsoil_ML/hobo_sensor_info.csv",header=T)

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

pdf(file="/Users/mloranty/Google Drive/Documents/Research/manuscripts/PFT_Flux_lichen/figures/pft_Tsoil_hobo_annual.pdf",10,5)
par(cex.lab=1.25,cex.axis=1.25,mar=c(0,6,3,2),mfcol=c(2,1))

plot(temp.daily$date,temp.daily$lichen.mean,type="l",lwd=1.5,
     ylab=expression(paste(T[soil]," (",C*degree,")")),
     xlab="",xaxt="n",col="red",ylim=c(-14,8))
axis.POSIXct(1,at=seq(temp.monthly$date[1],temp.monthly$date[24],by="month"),format="%b",labels=F)
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

legend("bottomleft",c("Lichen","Shrub"),col=c("gray50","black"),lwd=2,bty="n",cex=1.25)
legend("topright","a",bty="n",cex=1.25)

## plot the difference between lichen and shrub
par(cex.lab=1.25,cex.axis=1.25,mar=c(3,6,0,2),mfcol=c(2,1),new=T)
plot(temp.daily$date,temp.daily$lichen.mean-temp.daily$shrub.mean,type="l",lwd=2,
     ylab=expression(paste(T[dif]," (",C*degree,")")),
     xlab="",xaxt="n",ylim=c(-0.5,4.5))
axis.POSIXct(1,at=seq(temp.monthly$date[1],temp.monthly$date[24],by="month"),format="%b",labels=TRUE)
legend("topright","b",bty="n",cex=1.25)
dev.off()

## make an image sort of looking plot
x <- ymd(unique(temp$day))
y <- unique(temp$hour)

z <- matrix(temp$dif,ncol=length(y),nrow=length(x),byrow=T)
image(x,y,z)
image(z,col=heat.colors(10))
jet.colors <-colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

filled.contour(x,y,z, color = jet.colors,ylim=c(0,23))
image.plot(x,y,z,ylim=c(0,23),col=jet.colors(10))

###################
#
#   OLD PLOT CODE
#
###################

# points(temp.monthly$date[2:24],temp.monthly$shrub.mean[2:24],pch=16,col="blue")
# points(temp.monthly$date[2:24],temp.monthly$lichen.mean[2:24],pch=16,col="black")
# 
# segments(temp.monthly$date[2:24],temp.monthly$shrub.mean[2:24]-temp.monthly$shrub.sd[2:24],
#        temp.monthly$date[2:24],temp.monthly$shrub.mean[2:24]+temp.monthly$shrub.sd[2:24],
#        pch=16,col="blue",lwd=2,)

# segments(temp.monthly$date[2:24],temp.monthly$lichen.mean[2:24]-temp.monthly$lichen.sd[2:24],
#          temp.monthly$date[2:24],temp.monthly$lichen.mean[2:24]+temp.monthly$lichen.sd[2:24],
#          pch=16,col="black",lwd=2,)

#plot of avg. daily difference#
pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/pft_Tsoil_hobo_annual_difference.pdf",6,6)
plot(temp.daily$date,temp.daily$lichen.mean-temp.daily$shrub.mean,type="l",lwd=2,
     ylab=expression(paste("Soil Temperature Difference (",C*degree,")")),
     xlab="",xaxt="n")
axis.POSIXct(1,at=seq(temp.monthly$date[2],temp.monthly$date[24],by="month"),format="%b",labels=TRUE)
dev.off()
#plot of avg. monthly difference#
pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/pft_Tsoil_hobo_monthly_difference.pdf",6,6)
plot(seq(temp.monthly$date[1],temp.monthly$date[24],by="month")
     ,temp.monthly$lichen.mean-temp.monthly$shrub.mean,type="b",lwd=2,pch=16,lty=2,
     ylab=expression(paste("Soil Temperature Difference (",C*degree,")")),
     xlab="Month",xaxt="n")
abline(h=0,lwd=1,lty=2)
axis.POSIXct(1,at=seq(temp.monthly$date[2],temp.monthly$date[24],by="month"),format="%b",labels=TRUE)
dev.off()
# box and whisker plot by month #

month.bp <- (temp.monthly[2:24,2])

for(i in 3:19)
{month.bp <- c(month.bp,temp.monthly[2:24,i])}

month.bp <- as.data.frame(month.bp)
month.bp$month <- rep(months(temp.monthly$date[2:24]),18)
month.bp$Sensor <- rep(as.numeric(substr(names(temp.monthly)[2:19],1,7)),each=23)
names(month.bp) <- c("temp","month","Sensor")
month.bp$veg <- veg$Veg[match(month.bp$Sensor,veg$Serial.No.)]
month.bp$date <- rep(temp.monthly$date[2:24],18)
month.bp <- month.bp[order(month.bp$date),]

pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/pft_Tsoil_hobo_monthly.pdf",10,6)
boxplot(temp~veg*date,data=month.bp,col=c("black","blue"),
        name=months(temp.monthly$date[2:24]),xaxt="n",
        ylab=expression(paste("Soil Temperature (",C*degree,")")))

axis(1,at=seq(1.5,45.5,2),months(temp.monthly$date[2:24]))
legend("bottomleft",c("Lichen","Shrub"),fill=c("black","blue"),bty="n")
dev.off()



