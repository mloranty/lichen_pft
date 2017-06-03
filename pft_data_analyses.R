################################
# analyze plot and flux data
# for pft~lichen paper
#
# MML 02 June 2017
# updated version of old analyses
# from AGU 2013, ouch!
################################
se <- function(x){sd(x,na.rm=T)/sqrt(length(which(is.na(x)==F)))}
require("plyr")

rm(list=ls())
setwd("/Users/mloranty/Google Drive/Documents/Research/field_data/pft_lichen_flux/")

####################################
#read in plot and auxiliary env data
ndvi <- read.csv("env_data/ndvi.csv")
ndvi$veg.type <- as.factor(substr(ndvi$plot,3,3))

#read basal diameter data and calculate biomass (Berner et al, 2015)
shrub <- read.csv("env_data/shrub_data.csv")
shrub$agb <- ifelse(shrub$Genus=="B",
                     28.10*(shrub$BD.cm^2.97),
                     23.53*(shrub$BD.cm^2.83))

veg <- read.csv("env_data/veg_cover.csv")
veg$veg.type <- as.factor(substr(veg$plot,3,3))
  
met <- read.csv("env_data/met.csv")
met$t.diff <- met$Tsurf-met$Tair.met
met$veg.type <- as.factor(substr(met$plot,3,3))
####################################
####################################
# calculate plot-level summaries & collate all plot-level data
plot.dat <- aggregate(ndvi$ndvi, by=list(ndvi$plot),FUN="mean",na.rm=T)
names(plot.dat)[2] <- "ndvi"
plot.dat <- join(plot.dat,aggregate(ndvi$ndvi, by=list(ndvi$plot),FUN="se"))
names(plot.dat)[3] <- "ndvi.se"
plot.dat <- join(plot.dat,aggregate(shrub$agb, by=list(shrub$Plot),FUN="sum",na.rm=T))
names(plot.dat)[4] <- "shrub.agb"
plot.dat <- join(plot.dat,aggregate(met$Ks, by=list(met$plot),FUN="mean",na.rm=T))
names(plot.dat)[5] <- "Ks"
plot.dat <- join(plot.dat,aggregate(met$Ks, by=list(met$plot),FUN="se"))
names(plot.dat)[6] <- "Ks.se"
names(plot.dat)[1] <- "plot"
plot.dat <- join(plot.dat,veg)
plot.dat$veg.type <- as.factor(substr(plot.dat$plot,3,3))

## write a csv for later, etc...
write.csv(plot.dat, file="env_data/plot_data.csv", row.names=F)
####################################
####################################
# calculate treatment level summaries and stats
# 1 = lichen mat
# 2 = shrub/moss
# 3 = shrub (higher biomass than 2, generally)

pft.dat <- aggregate(ndvi$ndvi,by=list(ndvi$veg.type),FUN="mean",na.rm=T)
names(pft.dat)[2] <- "ndvi"
pft.dat <- join(pft.dat,aggregate(ndvi$ndvi,by=list(ndvi$veg.type),FUN="se"))
names(pft.dat)[3] <- "ndvi.se"
t <- aov(ndvi~veg.type,data=ndvi)
t.hsd <- TukeyHSD(t)
pft.stat <- t.hsd$veg.type[,4]

pft.dat <- join(pft.dat,aggregate(plot.dat$shrub.agb,by=list(plot.dat$veg.type),FUN="mean",na.rm=T))
names(pft.dat)[4] <- "agb"
pft.dat <- join(pft.dat,aggregate(plot.dat$shrub.agb,by=list(plot.dat$veg.type),FUN="se"))
names(pft.dat)[5] <- "agb.se"
t <- aov(shrub.agb~veg.type,data=plot.dat)
t.hsd <- TukeyHSD(t)
pft.stat <- cbind(pft.stat,t.hsd$veg.type[,4])

pft.dat <- join(pft.dat,aggregate(met$Ks,by=list(met$veg.type),FUN="mean",na.rm=T))
names(pft.dat)[6] <- "Ks"
pft.dat <- join(pft.dat,aggregate(met$Ks,by=list(met$veg.type),FUN="se"))
names(pft.dat)[7] <- "Ks.se"
t <- aov(Ks~veg.type, data=met)
t.hsd <- TukeyHSD(t)
pft.stat <- cbind(pft.stat,t.hsd$veg.type[,4])

j19 <- which(met$date=="7/19/13")
pft.dat <- join(pft.dat,aggregate(met$td[j19],by=list(met$veg.type[j19]),FUN="mean",na.rm=T))
names(pft.dat)[8] <- "td.7.19"
pft.dat <- join(pft.dat,aggregate(met$td[j19],by=list(met$veg.type[j19]),FUN="se"))
names(pft.dat)[9] <- "td.7.19.se"
t <- aov(td[j19]~veg.type[j19],data=met)
t.hsd <- TukeyHSD(t)
pft.stat <- cbind(pft.stat,t.hsd$veg.type[,4])

a3 <- which(met$date=="8/3/13")
pft.dat <- join(pft.dat,aggregate(met$td[a3],by=list(met$veg.type[a3]),FUN="mean",na.rm=T))
names(pft.dat)[10] <- "td.8.3"
pft.dat <- join(pft.dat,aggregate(met$td[a3],by=list(met$veg.type[a3]),FUN="se"))
names(pft.dat)[11] <- "td.8.3.se"
t <- aov(td[a3]~veg.type[a3],data=met)
t.hsd <- TukeyHSD(t)
pft.stat <- cbind(pft.stat,t.hsd$veg.type[,4])

pft.dat <- join(pft.dat,aggregate(veg$moss.und, by=list(veg$veg.type),FUN="mean"))
names(pft.dat)[12] <- "moss"
pft.dat <- join(pft.dat,aggregate(veg$moss.und, by=list(veg$veg.type),FUN="se"))
names(pft.dat)[13] <- "moss.se"
t <- aov(moss.und~veg.type,data=veg)
t.hsd <- TukeyHSD(t)
pft.stat <- cbind(pft.stat,t.hsd$veg.type[,4])
colnames(pft.stat) <- c("ndvi","agb", "Ks","td7.19","td8.3","moss")

write.csv(pft.dat,file="pft_data.csv",row.names = F)
write.csv(pft.stat,file="pft_plot_stats.csv")

# write a table for paper
table1 <- cbind(c("Lichen","Shrub/Moss","Shrub"),
                paste(round(pft.dat$ndvi,digits=2)," (",round(pft.dat$ndvi.se,digits=2),")",sep=""),
                paste(round(pft.dat$agb,digits=1)," (",round(pft.dat$agb.se,digits=1),")",sep=""),
                paste(round(pft.dat$moss,digits=0)," (",round(pft.dat$moss.se,digits=1),")",sep=""),
                paste(round(pft.dat$Ks,digits=2)," (",round(pft.dat$Ks.se,digits=2),")",sep=""),
                paste(round(pft.dat$td.7.19,digits=2)," (",round(pft.dat$td.7.19.se,digits=1),")",sep=""),
                paste(round(pft.dat$td.8.3,digits=2)," (",round(pft.dat$td.8.3.se,digits=1),")",sep=""))
colnames(table1) <- c("pft","ndvi","shrub.agb","moss","Ks","td7.19","td8.3")
write.csv(table1,file="table1.csv")

####################################
####################################
# read  and calculate fluxes
flux <- read.csv("flux.rates.final.2017-06-02.csv")
# create var to indicate the plot
flux$plot <- as.numeric(substr(as.character(flux$in.files),21,23))
#create var to indicate veg type
flux$veg <- as.factor(substr(as.character(flux$in.files),23,23))
#join met data to calculate fluxes from slopes
flux <- join(flux,met,by=c("date","plot"))

# calculate fluxes from slope msmts using ideal gas law
# 125 = chamber volume in l
# 0.25 = plot/chamber area in m2
# 0.08206 = gas constant
# 100 converts pressure from kPa to atm
# 273.15 converts Tair from C to K
# CO2 in umol/m2/sec
flux$co2.umol <- ((flux$CO2*(125/0.25))*(flux$p.Kpa/100))/(0.08206*(flux$Tair.met+273.15))
#H2O in mmol/m2/sec
flux$h2o.mmol <- ((((flux$H2O*1000)*(125/0.25))*(flux$p.Kpa/100))/(0.08206*(flux$Tair.met+273.15)))/1000

# create vars to indicate light/dark fluxes
flux$dark <- substr(as.character(flux$in.files),24,24)
flux$dark <- replace(flux$dark,list=which(flux$dark=="."),"l")

# now set up data from with all of the met drivers for analysis
d <- which(flux$dark=="d")
l <- which(flux$dark=="l")

met.flux <- join(met,flux[l,c(9,17,35,11,12,36,13,14,15)],by=c("date","plot"))
colnames(met.flux)[19:25] <- c("nee","nee.r2","nee.p","et","et.r2","et.p","nee.et.rank")
met.flux <- join(met.flux,flux[d,c(9,17,35,11,12,15)],by=c("date","plot"))
colnames(met.flux)[26:29] <- c("reco","reco.r2","reco.p","reco.rank")

# exclude low-quality data
# this is somewhat objective; it depends on visual/manual assessment of fluxes
# it is difficult to intepret using fit statistics because, for example
# GPP and Reco can balance out yielding legitimate NEE msmts with a slope
# of 0 and poor fir statistics
#
# but the confidence rating does correlate with fit statistics overall
# see for example; 
# aggregate(flux$CO2.adjR2,by=list(flux$Confidence),FUN="mean") or
# aggregate(flux$CO2.p,by=list(flux$Confidence),FUN="mean")
#
# so we get rid of data with confidence rating less then 3
met.flux$nee[which(met.flux$nee.et.rank<2)] <- NA
met.flux$et[which(met.flux$nee.et.rank<2)] <- NA
met.flux$reco[which(met.flux$reco.rank<2)] <- NA
# get rid of negative et and reco because that's not very likley at all
met.flux$et[which(met.flux$et<0)] <- NA
met.flux$reco[which(met.flux$reco<0)] <- NA
# calculate gpp and omit negative gpp values - also not possible. 
met.flux$gpp <- met.flux$reco-met.flux$nee
met.flux$gpp[which(met.flux$gpp<0)] <- NA
write.csv(met.flux,file="met.flux.csv")

# aggregate to daily values
met.daily <- aggregate(met.flux[,c(6:16,18:19,22,26)],
                       by=list(met.flux$date),FUN="mean",na.rm=T)
met.daily.se <- aggregate(met.flux[,c(6:16,18:19,22,26)],
                          by=list(met.flux$date),FUN="se")

# aggregate to daily by plot/veg type
veg.met.daily <- aggregate(met.flux[,c(6:16,18:19,22,26)],
                           by=list(paste(met.flux$date,met.flux$veg.type,sep=".")),
                           FUN="mean",na.rm=T)
veg.met.daily.se <- aggregate(met.flux[,c(6:16,18:19,22,26)],
                           by=list(paste(met.flux$date,met.flux$veg.type,sep=".")),
                           FUN="se")
write.csv(veg.met.daily,file="met_flux_daily_plot.csv",row.names = F)

# write Table 2 for the paper
table2 <- cbind(veg.met.daily$Group.1,
                paste(round(veg.met.daily$Tsoil,digits=1)," (",round(veg.met.daily.se$Tsoil,digits=1),")",sep=""),
                paste(round(veg.met.daily$Tsurf,digits=1)," (",round(veg.met.daily.se$Tsurf,digits=1),")",sep=""),
                paste(round(veg.met.daily$Tair.met,digits=1)," (",round(veg.met.daily.se$Tair.met,digits=1),")",sep=""),
                paste(round(veg.met.daily$t.diff,digits=1)," (",round(veg.met.daily.se$t.diff,digits=1),")",sep=""),
                paste(round(veg.met.daily$par.met,digits=0)," (",round(veg.met.daily.se$par.met,digits=0),")",sep=""),
                paste(round(veg.met.daily$et,digits=2)," (",round(veg.met.daily.se$et,digits=2),")",sep=""))
colnames(table2) <- c("Date.plot","Tsoil","Tsurf","Tair","Tdiff","PAR","ET")
write.csv(table2,file="table2.csv",row.names = F)

# run anova on daily temp/energy vars
# use four days with consistent sample conditions
d <- unique(met.flux$date)[4:7]

stat.table <- matrix(nrow=12,ncol=8)
stat.table[,1] <- rep(d,each=3)
v <- c(9,10,15,18,14,22)
for(i in 1:length(d))
{
  day <- met.flux[which(met.flux$date==d[i]),]

  for(j in 1:length(v))
  {
    t <- aov(day[,v[j]]~day$veg.type)
    t.hsd <- TukeyHSD(t)
    r <- (i*3-2):(i*3)
    stat.table[r,j+2] <- t.hsd[[1]][,4]
  }
    
}

stat.table[,2] <- rep(c(1.2,1.3,2.3),4)
colnames(stat.table) <- c("date","plot","Tsoil","Tsurf","Tair","Tdiff","PAR","ET")
write.csv(stat.table,file="table2_stats.csv",row.names = F)





################################################################
# OLD CODE FROM PREVIOUS AGU ANALYSES
#BASIC STUFF, SUMMARIES AND ANOVAs
###############################################################
### read and calculate plot shrub biomass ###
shrub <- read.xlsx("plot_env_data.xlsx",sheetName="shrub_biomass")
shrub$plt.gen <- paste(shrub$Genus,shrub$Plot)
shrub$trt <- paste(shrub$Genus,substr(as.character(shrub$Plot),3,3))
plot.shrub <- aggregate(shrub$AGB..g.,list(shrub$plt.gen),FUN=sum,na.rm=T)
avg.shrub <- aggregate(plot.shrub$x,
                       list(paste(substr(plot.shrub$Group.1,1,nchar(plot.shrub$Group.1)-4),
                              substr(plot.shrub$Group.1,nchar(plot.shrub$Group.1),nchar(plot.shrub$Group.1)))),
                             FUN=mean)
avg.shrub.tot <- aggregate(shrub$AGB..g.,list(shrub$Plot),FUN=sum)
colnames(avg.shrub.tot) <- c("plot","biomass")
ndvi <- read.xlsx("plot_env_data.xlsx",sheetName="ndvi")
pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/avg_plot_shrub_biomass.pdf",3.5,6)
par(cex=1.2,mgp=c(2.5,1,0))
barplot(rbind(avg.shrub$x[2:3],avg.shrub$x[4:5])*4,
        col=c("black","gray"),
        names=c("Shrub\nMoss","Shrub"),ylim=c(0,2000),
        ylab=expression(paste("Shrub Biomass (g ",m^-2,")")))
legend("topleft",c("Betula","Salix"),fill=c("black","gray"),bty="n")
dev.off()
############################################################################################
### read and calculate plot percent cover ###
pc <- read.xlsx("plot_env_data.xlsx",sheetName="percent cover",startRow=2)
pc.plot <- aggregate(pc,list(substr(pc$plot,3,3)),FUN=mean,na.rm=T)
pc.plot.se <- aggregate(pc,list(substr(pc$plot,3,3)),FUN=se)
pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/avg_plot_ndvi.pdf",4,6)
par(cex=1.2,mgp=c(2.5,1,0))
barplot(rowMeans(pc.plot[,16:18]),col="black",ylab="NDVI",
        ylim=c(0,0.8),names=c("Lichen","Shrub\nMoss","Shrub"))
dev.off()

pc <- merge(pc,avg.shrub.tot)
plot(pc$biomass,pc$moss.und)
############################################################################################
### read and calculate met ###
met <- read.xlsx("plot_env_data.xlsx",sheetName="met")
met$veg <- substr(met$Plot,3,3)
met.sub <- met[13:87,]

plot.met <- aggregate(met.sub,list(met.sub$Plot),FUN=mean,na.rm=T)
veg.met <- aggregate(met.sub,list(met.sub$veg),FUN=mean,na.rm=T)
veg.met.se <- aggregate(met.sub,list(met.sub$veg),FUN=se)
pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/avg_plot_TD.pdf",4,6)
par(cex=1.2,mgp=c(2.5,1,0))
barplot(veg.met$TD,col="gray",ylab="ALT (cm)",main="Active Layer Thickness",
        ylim=c(80,0),names=c("Lichen","Shrub\nMoss","Shrub"),width=1)
arrows(c(0.7,1.9,3.1),veg.met$TD-veg.met.se$TD,
      c(0.7,1.9,3.1),veg.met$TD+veg.met.se$TD,
      angle=90,code=3)
dev.off()

pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/avg_plot_TC.pdf",4,6)
par(cex=1.2,mgp=c(2.5,1,0))
barplot(veg.met$ThCnd,col="gray",main="Thermal Conductivity",
        ylab=expression(paste("Thermal Conductivity (W",m^-1," ",C*degree^-1,")")),
        ylim=c(0,0.4),names=c("Lichen","Shrub\nMoss","Shrub"))
arrows(c(0.7,1.9,3.1),veg.met$ThCnd-veg.met.se$ThCnd,
       c(0.7,1.9,3.1),veg.met$ThCnd+veg.met.se$ThCnd,
       angle=90,code=3)
dev.off()

pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/avg_plot_TSoil.pdf",4,6)
par(cex=1.2,mgp=c(2.5,1,0))
barplot(veg.met$Tsoil,col="gray",main="Soil Temperature",
        ylab=expression(paste("Soil Temperature (",C*degree,")")),
        ylim=c(0,6),names=c("Lichen","Shrub\nMoss","Shrub"))
arrows(c(0.7,1.9,3.1),veg.met$Tsoil-veg.met.se$Tsoil,
       c(0.7,1.9,3.1),veg.met$Tsoil+veg.met.se$Tsoil,
       angle=90,code=3)
dev.off()

pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/avg_plot_Tsurf.pdf",4,6)
par(cex=1.2,mgp=c(2.5,1,0))
barplot(veg.met$Tsurf,col="gray",main="Surface Temperature",
        ylab=expression(paste("Radiometric Temperature (",C*degree,")")),
        ylim=c(0,20),names=c("Lichen","Shrub\nMoss","Shrub"))
arrows(c(0.7,1.9,3.1),veg.met$Tsurf-veg.met.se$Tsurf,
       c(0.7,1.9,3.1),veg.met$Tsurf+veg.met.se$Tsurf,
       angle=90,code=3)
dev.off()

pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/avg_plot_SM.pdf",4,6)
par(cex=1.2,mgp=c(2.5,1,0))
barplot(veg.met$SM*100,col="gray",main="Soil Moisture",
        ylab="Soil Moisture (%)",ylim=c(0,40),
        names=c("Lichen","Shrub\nMoss","Shrub"))
arrows(c(0.7,1.9,3.1),veg.met$SM*100-veg.met.se$SM*100,
       c(0.7,1.9,3.1),veg.met$SM*100+veg.met.se$SM*100,
       angle=90,code=3)
dev.off()
############################################################################################

ET <- aov(mmol.H2O.m2~veg,light.flux)
ER <- aov(umol.CO2.m2~veg,dark.flux)
light.flux <- light.flux[-which(light.flux$plot==5.1),]
dark.flux <- dark.flux[-which(dark.flux$plot==5.1),]

gpp <- aggregate(light.flux,list(light.flux$veg),FUN=mean,na.rm=T)
gpp.se <- aggregate(light.flux,list(light.flux$veg),FUN=se)

gpp.plot <- aggregate(light.flux,list(light.flux$plot),FUN=mean,na.rm=T)

er <- aggregate(dark.flux,list(dark.flux$veg),FUN=mean,na.rm=T)
er.se <- aggregate(dark.flux,list(dark.flux$veg),FUN=se)
er.plot <- aggregate(dark.flux,list(dark.flux$plot),FUN=mean,na.rm=T)
f.dat <- rbind(gpp$umol.CO2.m2,gpp$gpp,er$umol.CO2.m2)
f.dat.se <- rbind(gpp.se$umol.CO2.m2,gpp.se$gpp,er.se$umol.CO2.m2)

pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/C_flux.pdf",6,6)
par(cex=1.2,mgp=c(2.5,1,0))
barplot(f.dat,beside=T,col=c("black","darkgreen","gray"),main="Carbon Flux",
        ylab=expression(paste(CO[2]," Flux (",mu,"mol",m^-2," ",sec^-1,")")),
        ylim=c(-5,4),names=c("Lichen","Shrub\nMoss","Shrub"),width=1)
arrows(c(1,2,3,5,6,7,9,10,11)+0.5,as.vector(f.dat-f.dat.se),
       c(1,2,3,5,6,7,9,10,11)+0.5,as.vector(f.dat+f.dat.se),
       angle=90,code=3,length=0.1)

legend("bottomleft",c("NEE","GPP",expression(R[ECO])),fill=c("black","darkgreen","gray"),bty="n")
dev.off()

pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/ET.pdf",4,6)
par(cex=1.2,mgp=c(2.5,1,0))
barplot(gpp$mmol.H2O.m2,col="gray",
        ylab=expression(paste(H[2],"O Flux (mmol ",m^-2," ",sec^-1,")")),
        ylim=c(0,1.5),names=c("Lichen","Shrub\nMoss","Shrub"))
arrows(c(0.7,1.9,3.1),gpp$mmol.H2O.m2-gpp.se$mmol.H2O.m2,
       c(0.7,1.9,3.1),gpp$mmol.H2O.m2+gpp.se$mmol.H2O.m2,
       angle=90,code=3)
dev.off()
boxplot(mmol.H2O.m2~veg, data=light.flux)
boxplot(umol.CO2.m2~veg,data=dark.flux)

s <- match(avg.shrub.tot$plot,gpp.plot$Group.1)
gpp.plot$gpp[14] <- NA
gpp.plot$gpp[10] <- NA
gpp.sh <- lm(gpp.plot$gpp[s]~avg.shrub.tot$biomass)

pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/shrub_GPP.pdf",6,6)
par(cex=1.4,mgp=c(2.5,1,0))
plot(avg.shrub.tot$biomass,gpp.plot$gpp[s],pch=19,xlim=c(100,600),
     xlab=paste("Shrub Biomass (g)"),ylim=c(-8,1),
     ylab=expression(paste(" GPP (",mu,"mol C",O[2]," ",m^-2," ",sec^-1,")")))
    abline(gpp.sh,lwd=2,lty="dashed")
  dev.off ()

er.td <- lm(er.plot$umol.CO2.m2~plot.met$TD)
pdf(file="/Users/mloranty/Documents/Research/siberia_data/figures/ER_TD.pdf",6,6)
par(cex=1.4,mgp=c(2.5,1,0))
plot(plot.met$TD,er.plot$umol.CO2.m2,pch=19,
     xlab="Active Layer Thickness (cm)",ylim=c(0,5),
     ylab=expression(paste(R[ECO],"( ",mu,"mol ",m^-2," ",sec^-1,")")))
#abline(er.td,lty="dashed",lwd=2)
dev.off()

dp <- match(paste(met$Date,met$Plot),paste(light.flux$date,light.flux$plot))

plot(met$)