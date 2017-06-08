##########################
#
# script to calculate 
# CO2 and H20 flux from 
# raw flux data
#
# MML 07/27/13
##########################


setwd("c:\\Users\\hkropp\\Google Drive\\lichen")

in.files <- list.files(path=paste0(getwd(),"/raw"), full.names=T)
plotname<-gsub("c:/Users/hkropp/Google Drive/lichen/raw/","", in.files)
plotname2<-gsub(".txt","", plotname)
plot.files <- paste0(getwd(),"/plots/",plotname2,".pdf")
flux.rates <- as.data.frame(in.files)
flux.rates$CO2 <- 9999
flux.rates$H2O <- 9999
# set the initial sample window for slope calc #
flux.rates$CO2.start <- 20
flux.rates$CO2.finish <- 60

flux.rates$H2O.start <- 20
flux.rates$H2O.finish <- 60

for(i in 1:length(in.files))
{
  dat <- read.table(file=in.files[i],header=T,skip=1)
  rC <- c(flux.rates$CO2.start[i]:flux.rates$CO2.finish[i])
  rH <- c(flux.rates$H2O.start[i]:flux.rates$H2O.finish[i])
  c.reg <- lm(dat$CO2.ppm.[rC]~rC)
  h.reg <- lm(dat$H2O.ppt.[rH]~rH)
  
  pdf(file=paste0(getwd(),"/plots/",plotname2[i],".pdf"),5,10)
  par(mfcol=c(2,1),mar=c(4,3,2,2))
  plot(dat$CO2.ppm.,xlab="Time (seconds)",
       ylab= "[CO2] (ppm)",
       main=substr(in.files[i],11,24))
  points(rC,dat$CO2.ppm.[rC],pch=16,col="green")
  lines(rC,predict(c.reg),lwd=2,col="red")
  
  
  plot(dat$H2O.ppt.,xlab="Time (seconds)",
       ylab= "[H2O] (ppt)")
  points(rH,dat$H2O.ppt.[rH],pch=16,col="green")
  lines(rH,predict(h.reg),lwd=2,col="blue")
  dev.off()
  
  flux.rates$CO2[i] <- coefficients(c.reg)[2]
  flux.rates$H2O[i] <- coefficients(h.reg)[2]
  
  flux.rates$date[i] <- as.character(dat$Date.Y.M.D.[1])
  flux.rates$time[i] <- as.character(dat$Time.H.M.S.[1])
}

write.csv(flux.rates,file=paste("flux.rates",Sys.Date(),"csv",sep="."),row.names=F)


########################################################################################
### now go through and examine fluxes and confirm appropriate sampling window        ###
### make any necessary changes, read the file back in, and run it again              ###
### sample code below - but see 'final_flux_calc.R' for next steps                   ###
########################################################################################

# flux.rates <- read.csv(file.choose())
# 
# for(i in 1:length(flux.rates$in.files))
# {
#   dat <- read.table(file=flux.rates$in.files[i],header=T,skip=1)
#   y <- c(flux.rates$start,flux.rates$finish)
#   c.reg <- lm(dat$CO2.ppm.[y]~y)
#   h.reg <- lm(dat$H2O.ppt.[y]~y)
# 
# 
#   flux.rates$CO2[i] <- coefficients(c.reg)[2]
#   flux.rates$H2O[i] <- coefficients(h.reg)[2]
# }
# 
# write.csv(flux.rates,file=paste("flux.rates.final",Sys.Date(),"csv",sep="."))