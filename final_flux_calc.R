##########################
#
# script to calculate 
# CO2 and H20 flux from 
# raw flux data
#
# MML 07/27/13
##########################

rm(list=ls())

setwd("/Users/mloranty/Documents/GitHub/lichen_pft/")
############################################################################
# BEFORE GOING FURTHER IT IS NECESSARY TO HAVE MANUALLY EVALUATED EACH FLUX AS FOLLOWS ###
# 1. Check the timing of flux measurement and adjust as necessary
#     (e.g. is 15-60 seconds OK, or does it saturate at 50?)
# 2. Update the interval for flux calculation in the file.
# 3. make notes of any irregulatities in the flux; 
#     for example, are there spikes, do high H2O concentrations suggest chamber was not fully aired out, etc...
# ONCE THESE STEPS HAVE BEEN TAKEN FINAL FLUX RATES CAN BE CALCULATED BELOW
############################################################################

# read in the file with updated sampling intervals. 
flux.rates <- read.csv(file.choose(),as.is=T)
# crease vector of file names for the plots
plot.files <- paste("flux_plots/",substr(flux.rates$in.files,11,nchar(flux.rates$in.files)),".pdf",sep="")

# now calculate each flux using the updated sample intervals, create a graph
# and append flux rates and R2 values to the data sheet
for(i in 1:length(flux.rates$in.files))
{
  dat <- read.table(file=flux.rates$in.files[i],header=T,skip=1)
  rC <- c(flux.rates$CO2.start[i]:flux.rates$CO2.finish[i])
  rH <- c(flux.rates$H2O.start[i]:flux.rates$H2O.finish[i])
  c.reg <- lm(dat$CO2.ppm.[rC]~rC)
  h.reg <- lm(dat$H2O.ppt.[rH]~rH)
  
  pdf(file=plot.files[i],5,10)
  par(mfcol=c(2,1),mar=c(4,3,2,2))
  plot(dat$CO2.ppm.,xlab="Time (seconds)",
       ylab= "[CO2] (ppm)",
       main=substr(flux.rates$in.files[i],11,24))
  points(rC,dat$CO2.ppm.[rC],pch=16,col="green")
  lines(rC,predict(c.reg),lwd=2,col="red")
  
  plot(dat$H2O.ppt.,xlab="Time (seconds)",
       ylab= "[H2O] (ppt)")
  points(rH,dat$H2O.ppt.[rH],pch=16,col="green")
  lines(rH,predict(h.reg),lwd=2,col="blue")
  dev.off()
  
  flux.rates$CO2[i] <- coefficients(c.reg)[2]
  flux.rates$H2O[i] <- coefficients(h.reg)[2]
  flux.rates$CO2.adjR2[i] <- summary(c.reg)$adj.r.squared
  flux.rates$CO2.p[i] <- summary(c.reg)$coefficients[2,4]
  flux.rates$H2O.adjR2[i] <- summary(h.reg)$adj.r.squared
  flux.rates$H2O.p[i] <- summary(h.reg)$coefficients[2,4]
  
}

write.csv(flux.rates,file=paste("flux.rates.final",Sys.Date(),"csv",sep="."),row.names=F)

