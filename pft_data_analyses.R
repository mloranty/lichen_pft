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

met <- read.csv("env_data/met.csv")

# calculate plot-level summaries
plot.dat <- aggregate(ndvi$ndvi, by=list(ndvi$plot),FUN="mean",na.rm=T)
names(plot.dat)[2] <- c("ndvi")
plot.dat <- join(plot.dat,aggregate(ndvi$ndvi, by=list(ndvi$plot),FUN="se"))
names(plot.dat)[3] <- c("ndvi.se")
plot.dat <- join(plot.dat,aggregate(shrub$agb, by=list(shrub$Plot),FUN="mean",na.rm=T))
names(plot.dat)[4] <- c("agb")
plot.dat <- join(plot.dat,aggregate(shrub$agb, by=list(shrub$Plot),FUN="se"))
names(plot.dat)[5] <- c("agb.se")
plot.dat <- join(plot.dat,aggregate(met$Ks, by=list(met$plot),FUN="mean",na.rm=T))
names(plot.dat)[6] <- c("Ks")
plot.dat <- join(plot.dat,aggregate(met$Ks, by=list(met$plot),FUN="se"))
names(plot.dat)[7] <- c("Ks.se")

####################################
# read  and calculate fluxes
flux <- read.csv("flux.rates.final.2017-06-02.csv")
flux$plot <- as.factor(substr(as.character(flux$in.files),21,23))
flux$veg <- as.factor(substr(as.character(flux$in.files),23,23))

flux <- join()
flux$dark <- substr(as.character(flux$in.files),24,24)
flux$dark <- replace(flux$dark,list=which(flux$dark=="."),"l")


for(i in 9:11)
{ flux[which(flux[,i]==-9999),i] <- NA}  
light.flux <- flux[which(flux$dark=="l"),]
dark.flux <- flux[which(flux$dark=="d"),]
light.flux$gpp <- light.flux$umol.CO2.m2-dark.flux$umol.CO2.m2

light.flux <- light.flux[which(light.flux$Confidence>2),]
dark.flux <- dark.flux[which(dark.flux$Confidence>2),]
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