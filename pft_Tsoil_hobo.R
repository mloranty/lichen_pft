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
#format date and time#
temp$date <- strptime(temp$date,format="%m/%d/%y %H:%M")

#read data on sensor placement and veg condition#
veg <- read.csv("/Users/mloranty/Google Drive/Documents/Research/NSFBorealFireCherskii_2014-17/Loranty_data/EXB_pft_hobo_Tsoil_ML/hobo_sensor_info.csv",header=T)

#put temp back in chronological order#
# ord <- match(sort(temp$rec),temp$rec)
# temp <- temp[ord,]
# temp$mean <- rowMeans(temp[,3:21],na.rm=T)
# write.csv(temp,file="temp_daily.csv")

#aggregate to daily#
temp.daily <- aggregate(temp[,3:20],by=list(substr(temp$date,1,10)),
                        FUN=mean,na.rm=T)
# remove data logged before sensor deployment#
temp.daily <- temp.daily[23:nrow(temp.daily),]
# format date
temp.daily$date <- strptime(temp.daily$Group.1,format="%Y-%m-%d")

#aggregate to monthly#
temp.monthly <- aggregate(temp.daily[,2:19],by=list(substr(temp.daily$Group.1,1,7)),
                                   FUN=mean,na.rm=T)
temp.monthly$date <- seq(as.POSIXlt("2012-07-15"),as.POSIXlt("2014-06-15"),"month")


###sort and aggregate by vegetation type ###
lichen <- veg[which(veg$Veg == "L"),]
shrub <- veg[which(veg$Veg == "SM"),]

l.rec <- na.omit(match(lichen$Serial.No.,as.numeric(substr(names(temp.daily),1,7))))
s.rec <- na.omit(match(shrub$Serial.No.,as.numeric(substr(names(temp.daily),1,7))))

temp.daily$lichen.mean <- apply(temp.daily[,l.rec],1,FUN=mean,na.rm=T)
temp.daily$lichen.sd <- apply(temp.daily[,l.rec],1,FUN=sd,na.rm=T)
temp.daily$shrub.mean <- apply(temp.daily[,s.rec],1,FUN=mean,na.rm=T)
temp.daily$shrub.sd <- apply(temp.daily[,s.rec],1,FUN=sd,na.rm=T)

l.rec <- na.omit(match(lichen$Serial.No.,as.numeric(substr(names(temp.monthly),1,7))))
s.rec <- na.omit(match(shrub$Serial.No.,as.numeric(substr(names(temp.monthly),1,7))))
temp.monthly$lichen.mean <- apply(temp.monthly[,l.rec],1,FUN=mean,na.rm=T)
temp.monthly$lichen.sd <- apply(temp.monthly[,l.rec],1,FUN=sd,na.rm=T)
temp.monthly$shrub.mean <- apply(temp.monthly[,s.rec],1,FUN=mean,na.rm=T)
temp.monthly$shrub.sd <- apply(temp.monthly[,s.rec],1,FUN=sd,na.rm=T)

#make a sweet plot

pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/pft_Tsoil_hobo_annual.pdf",10,5)
par(cex.lab=1.5,cex.axis=1.5,mar=c(4,6,3,2),bty="n")
plot(temp.daily$Group.1,temp.daily$lichen.mean,type="l",lwd=2,
     ylab=expression(paste("Soil Temperature (",C*degree,")")),
     xlab="",xaxt="n")
axis.POSIXct(1,at=seq(temp.monthly$date[2],temp.monthly$date[24],by="month"),format="%b",labels=TRUE)
lines(temp.daily$Group.1,temp.daily$shrub.mean,type="l",lwd=2,col="blue")
#lines(temp.daily$Group.1,temp.daily$lichen.mean-temp.daily$shrub.mean,type="l",
#      lwd=1,col="lightgray",lty="dashed")

points(temp.monthly$date[2:24],temp.monthly$shrub.mean[2:24],pch=16,col="blue")
points(temp.monthly$date[2:24],temp.monthly$lichen.mean[2:24],pch=16,col="black")

segments(temp.monthly$date[2:24],temp.monthly$shrub.mean[2:24]-temp.monthly$shrub.sd[2:24],
       temp.monthly$date[2:24],temp.monthly$shrub.mean[2:24]+temp.monthly$shrub.sd[2:24],
       pch=16,col="blue",lwd=2,)

segments(temp.monthly$date[2:24],temp.monthly$lichen.mean[2:24]-temp.monthly$lichen.sd[2:24],
         temp.monthly$date[2:24],temp.monthly$lichen.mean[2:24]+temp.monthly$lichen.sd[2:24],
         pch=16,col="black",lwd=2,)
legend("bottomleft",c("Lichen","Shrub"),fill=c("black","blue"),bty="n",cex=1.5)
dev.off()

#plot of avg. daily difference#
pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/pft_Tsoil_hobo_annual_difference.pdf",6,6)
plot(temp.daily$Group.1,temp.daily$lichen.mean-temp.daily$shrub.mean,type="l",lwd=2,
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



