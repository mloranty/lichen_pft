################################
# analyze plot and flux data
# for pft~lichen paper
#
# MML 02 June 2017
# updated version of old analyses
# from AGU 2013, ouch!
################################
require("plyr")
require("plotrix")
rm(list=ls())
setwd("/Users/mloranty/Documents/GitHub/lichen_pft/")

se <- function(x){sd(x,na.rm=T)/sqrt(length(which(is.na(x)==F)))}
####################################
#read in plot and auxiliary env data
ndvi <- read.csv("data/ndvi.csv")
ndvi$veg.type <- as.factor(substr(ndvi$plot,3,3))

#read basal diameter data and calculate biomass (Berner et al, 2015)
shrub <- read.csv("data/shrub_data.csv")
shrub$agb <- ifelse(shrub$Genus=="B",
                     28.10*(shrub$BD.cm^2.97),
                     23.53*(shrub$BD.cm^2.83))

veg <- read.csv("data/veg_cover.csv")
veg$veg.type <- as.factor(substr(veg$plot,3,3))
  
met <- read.csv("data/met.csv")
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
## set NA shrub biomass to zero, because these plots have zero biomass
plot.dat$shrub.agb[which(is.na(plot.dat$shrub.agb==T))] <- 0
## write a csv for later, etc...
write.csv(plot.dat, file="data/flux_plot_data.csv", row.names=F)
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

write.csv(pft.dat,file="data/pft_flux_plot_data.csv",row.names = F)
write.csv(pft.stat,file="data/pft_flux_plot_stats.csv")

# write Table 3 for paper
table3 <- cbind(c("Lichen","Shrub/Moss","Shrub"),
                paste(round(pft.dat$ndvi,digits=2)," (",round(pft.dat$ndvi.se,digits=2),")",sep=""),
                paste(round(pft.dat$agb,digits=1)," (",round(pft.dat$agb.se,digits=1),")",sep=""),
                paste(round(pft.dat$moss,digits=0)," (",round(pft.dat$moss.se,digits=1),")",sep=""),
                paste(round(pft.dat$Ks,digits=2)," (",round(pft.dat$Ks.se,digits=2),")",sep=""),
                paste(round(pft.dat$td.7.19,digits=2)," (",round(pft.dat$td.7.19.se,digits=1),")",sep=""),
                paste(round(pft.dat$td.8.3,digits=2)," (",round(pft.dat$td.8.3.se,digits=1),")",sep=""))
colnames(table3) <- c("pft","ndvi","shrub.agb","moss","Ks","td7.19","td8.3")
write.csv(table3,file="paper_tables/table3.csv")

#################################################################################################################
#################################################################################################################
# read  and calculate fluxes
flux <- read.csv("flux_processing/flux.rates.final.2017-06-02.csv")
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

# now set up data frame with all of the met drivers for analysis
d <- which(flux$dark=="d")
l <- which(flux$dark=="l")

met.flux <- join(met,flux[l,c(8,16,35,10,11,36,12,13,14)],by=c("date","plot"))
colnames(met.flux)[20:26] <- c("nee","nee.r2","nee.p","et","et.r2","et.p","nee.et.rank")
met.flux <- join(met.flux,flux[d,c(8,16,35,10,11,14)],by=c("date","plot"))
colnames(met.flux)[27:30] <- c("reco","reco.r2","reco.p","reco.rank")

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
# so we get rid of data with confidence rating less then 2
met.flux$nee[which(met.flux$nee.et.rank<2)] <- NA
met.flux$et[which(met.flux$nee.et.rank<2)] <- NA
met.flux$reco[which(met.flux$reco.rank<2)] <- NA
# get rid of negative et and reco because that's not very likley at all
met.flux$et[which(met.flux$et<0)] <- NA
met.flux$reco[which(met.flux$reco<0)] <- NA
# calculate gpp and omit negative gpp values - also not possible. 
met.flux$gpp <- met.flux$reco-met.flux$nee
met.flux$gpp[which(met.flux$gpp<0)] <- NA
# calculate vpd
met.flux$d <- (0.611*exp((17.502*met.flux$Tair)/(met.flux$Tair+240.97)))*(met.flux$rh.met/100)
met.flux$d.met <- (0.611*exp((17.502*met.flux$Tair.met)/(met.flux$Tair.met+240.97)))*(met.flux$rh.met/100)
write.csv(met.flux,file="data/met.flux.csv")
#############################################################
# look at relationship between Reco and temperature
col <- ifelse(met.flux$veg.type==1,"grey60",
              ifelse(met.flux$veg.type==2,"sienna3","royalblue2"))
#pdf(file="reco_plots.pdf",10,6)
tiff(file="/Users/mloranty/Documents/GitHub/lichen_pft/paper_figures/figure5.tiff",
     width=10,height=6,units="in",res=300,compression="lzw",bg="white")
par(mfcol=c(1,2),mar=c(5,4.5,4,0))
plot(met.flux$Tsoil,met.flux$reco,pch=16,col=col,cex.lab=1.5,cex.axis=1.5,
     xlab=expression(paste(T[soil]," (",C*degree,")",sep="")),xlim=c(0,11),
     yaxt="n",ylab=" ")
axis(side = 2,labels=T,tick=T,las=2,cex.axis=1.5)
mtext(expression(paste(R[ECO]," (",mu,"mol ",m^-2," ",sec^-1,")",sep="")),side=2,cex=1.5,line=2.5)
ts <- lm(met.flux$reco~met.flux$Tsoil)
legend("topleft","a",bty="n",cex=1.5)
legend("topright",c("Lichen","Shrub-Moss","Shrub"),bty="n",
       col=c("grey60","sienna3","royalblue2"),pch=16,cex=1.25)
#abline(coef = coefficients(ts),lty="dashed",lwd=1.5)

par(mfcol=c(1,2),mar=c(5,0,4,4.5),new=T)
plot(met.flux$Tair,met.flux$reco,pch=16,col=col,cex.lab=1.5,cex.axis=1.5,
     xlab=expression(paste(T[air]," (",C*degree,")",sep="")),xlim=c(5,33),
     ylab=expression(paste(R[ECO]," (",mu,"mol ",m^-2," ",sec^-1,")",sep="")),
     yaxt="n")
axis(side = 2,labels=F,tick=T,cex.axis=1.5)
ta <- lm(met.flux$reco~met.flux$Tair)
abline(coef = coefficients(ta),lty="dashed",lwd=1.5)
legend("topleft","b",bty="n",cex=1.5)
dev.off()

# aggregate to daily values
met.daily <- aggregate(met.flux[,c(6:16,18,20,23,27)],
                       by=list(met.flux$date),FUN="mean",na.rm=T)
met.daily.se <- aggregate(met.flux[,c(6:16,18,20,23,27)],
                          by=list(met.flux$date),FUN="se")

# aggregate to daily by plot/veg type
veg.met.daily <- aggregate(met.flux[,c(6:16,18,20,23,27)],
                           by=list(paste(met.flux$date,met.flux$veg.type,sep=".")),
                           FUN="mean",na.rm=T)
veg.met.daily.se <- aggregate(met.flux[,c(6:16,18,20,23,27)],
                           by=list(paste(met.flux$date,met.flux$veg.type,sep=".")),
                           FUN="se")
write.csv(veg.met.daily,file="data/met_flux_daily_plot.csv",row.names = F)

# aggregate by vegetation type
veg.met.all <- aggregate(met.flux[,c(6:16,18,20,23,27)],
                         by=list(met.flux$veg.type),
                         FUN="mean",na.rm=T)
veg.met.all.se <- aggregate(met.flux[,c(6:16,18,20,23,27)],
                         by=list(met.flux$veg.type),
                         FUN="se")
write.csv(veg.met.all,file="data/met_flux_daily_veg.csv",row.names = F)

#################################################################################################################
# write Table 4 for the paper
table4 <- cbind(veg.met.daily$Group.1,
                paste(round(veg.met.daily$Tsoil,digits=1)," (",round(veg.met.daily.se$Tsoil,digits=1),")",sep=""),
                paste(round(veg.met.daily$Tsurf,digits=1)," (",round(veg.met.daily.se$Tsurf,digits=1),")",sep=""),
                paste(round(veg.met.daily$Tair.met,digits=1)," (",round(veg.met.daily.se$Tair.met,digits=1),")",sep=""),
                paste(round(veg.met.daily$t.diff,digits=1)," (",round(veg.met.daily.se$t.diff,digits=1),")",sep=""),
                paste(round(veg.met.daily$par.met,digits=0)," (",round(veg.met.daily.se$par.met,digits=0),")",sep=""),
                paste(round(veg.met.daily$nee,digits=2)," (",round(veg.met.daily.se$nee,digits=2),")",sep=""),
                paste(round(veg.met.daily$et,digits=2)," (",round(veg.met.daily.se$et,digits=2),")",sep=""),
                paste(round(veg.met.daily$reco,digits=2)," (",round(veg.met.daily.se$reco,digits=2),")",sep=""))
colnames(table4) <- c("Date.plot","Tsoil","Tsurf","Tair","Tdiff","PAR","NEE","ET","Reco")
write.csv(table4,file="paper_tables/table4.csv",row.names = F)

# run anova on daily temp/energy vars
# use four days with consistent sample conditions
d <- unique(met.flux$date)[4:7]

stat.table <- matrix(nrow=12,ncol=10)
stat.table[,1] <- rep(d,each=3)
v <- c(9,10,11,18,14,20,23,27)
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
colnames(stat.table) <- c("date","plot","Tsoil","Tsurf","Tair","Tdiff","PAR","NEE","ET","Reco")
write.csv(stat.table,file="paper_tables/table4_stats.csv",row.names = F)
#################################################################################################################
table5 <- cbind(veg.met.all$Group.1,
                paste(round(veg.met.all$Tsoil,digits=1)," (",round(veg.met.all.se$Tsoil,digits=1),")",sep=""),
                paste(round(veg.met.all$Tsurf,digits=1)," (",round(veg.met.all.se$Tsurf,digits=1),")",sep=""),
                paste(round(veg.met.all$Tair.met,digits=1)," (",round(veg.met.all.se$Tair.met,digits=1),")",sep=""),
                paste(round(veg.met.all$t.diff,digits=1)," (",round(veg.met.all.se$t.diff,digits=1),")",sep=""),
                paste(round(veg.met.all$par.met,digits=0)," (",round(veg.met.all.se$par.met,digits=0),")",sep=""),
                paste(round(veg.met.all$nee,digits=2)," (",round(veg.met.all.se$nee,digits=2),")",sep=""),
                paste(round(veg.met.all$et,digits=2)," (",round(veg.met.all.se$et,digits=2),")",sep=""),
                paste(round(veg.met.all$reco,digits=2)," (",round(veg.met.all.se$reco,digits=2),")",sep=""))
colnames(table5) <- c("VegType","Tsoil","Tsurf","Tair","Tdiff","PAR","NEE","ET","Reco")
write.csv(table5,file="paper_tables/table5.csv",row.names = F)

stat.table5 <- matrix(nrow=3,ncol=9)
v <- c(9,10,11,18,14,20,23,27)

for(j in 1:length(v))
  {
    t <- aov(met.flux[,v[j]]~met.flux$veg.type)
    t.hsd <- TukeyHSD(t)
    r <- (i*3-2):(i*3)
    stat.table5[,j+1] <- t.hsd[[1]][,4]
  }


stat.table5[,1] <- rep(c(1.2,1.3,2.3))
colnames(stat.table5) <- c("veg","Tsoil","Tsurf","Tair","Tdiff","PAR","NEE","ET","Reco")
write.csv(stat.table5,file="paper_tables/table5_stats.csv",row.names = F)
#################################################################################################################
# compare canopy cover with lichen cover
#################################################################################################################
u.veg <- read.csv("data/understory_cover.csv",header=TRUE)
cnpy <- read.csv("data/canopy_cover.csv", header=TRUE)
lich <- aggregate(u.veg$lichen,by=list(u.veg$Site),FUN=mean,na.rm=T)
lich.se <- aggregate(u.veg$lichen,by=list(u.veg$Site),FUN=se)
names(lich) <- c("stand","lichen")
names(lich.se) <- c("stand","lichen.se")

cvr <- aggregate(cnpy$Canopy..,by=list(cnpy$Site),FUN=mean)
cvr.se <- aggregate(cnpy$Canopy..,by=list(cnpy$Site),FUN=se)
names(cvr) <- c("stand","cvr")
names(cvr.se) <- c("stand","cvr.se")

y4 <- join(lich,lich.se)
y4 <- join(y4,cvr)
y4 <- join(y4,cvr.se)

rm(cvr,cvr.se,lich,lich.se)
dg <- read.csv("data/dg_lichen_cover.csv",header=T)
cvr.all <- join(y4,dg,type="full")
#regression of lichen cover versus canopy cover
veg.reg <- lm(y4$lichen~y4$cvr)
veg.reg2 <- lm(log(lichen)~cvr,data=cvr.all)
# make a plot for the paper
# tiff(file="/Users/mloranty/Documents/GitHub/lichen_pft/paper_figures/figure2.tiff",
#      width=6,height=5,units="in",res=300,compression="lzw",bg="white")
# plot(y4$cvr,y4$lichen,pch=16,cex.axis=1.5,cex.lab=1.5,
#      xlim=c(0,50),xlab="Canopy Cover (%)",
#      ylim=c(0,40),ylab=" ",yaxt="n")
# axis(side = 2,labels=T,tick=T,las=2,cex.axis=1.5)
# mtext("Lichen Cover (%)",side=2,cex=1.5,line=3)
# abline(coef = coefficients(veg.reg),lty="dashed",lwd=1.5)
# dev.off()

## all stands with exponential model
cov.val <- seq(0,80,1)
lich.exp <- exp(predict.lm(veg.reg2,data.frame(cvr=cov.val)))
tiff(file="/Users/mloranty/Documents/GitHub/lichen_pft/paper_figures/figure2.tiff",
     width=6,height=5,units="in",res=300,compression="lzw",bg="white")
plot(cvr.all$cvr,cvr.all$lichen,pch=16,cex.axis=1.5,cex.lab=1.5,
     xlim=c(0,80),xlab="Canopy Cover (%)",
     ylim=c(0,50),ylab=" ",yaxt="n")
plotCI(cvr.all$cvr,cvr.all$lichen,cvr.all$lichen.se,err="y",add=T,sfrac=0.01,gap=T)
plotCI(cvr.all$cvr,cvr.all$lichen,cvr.all$cvr.se,err="x",add=T)

axis(side = 2,labels=T,tick=T,las=2,cex.axis=1.5)
mtext("Lichen Cover (%)",side=2,cex=1.5,line=3)
lines(cov.val,lich.exp,lty="dashed",lwd=1.75)
text(20,45,"log(y) = -0.022x + 2.8",pos=4)
text(20,40,expression(paste(r^2," = 0.18, p<0.01",sep=" ")),pos=4)
dev.off()

################################################################
