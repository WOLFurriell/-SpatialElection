library(ggplot2)
library(sp)
library(gridExtra)
library(lattice)
library(RColorBrewer)
library(classInt)
library(rgdal)

rm(list=ls())
# importando o shape.

brasil_2000 <- readOGR("D:/PIC/shapes/SHAPE_2000_FINAL/shape_2000.shp",
                       "shape_2000", encoding='UTF-8')

brasil_2006 <- readOGR("D:/PIC/shapes/SHAPE_2007_FINAL/shape_2006.shp",
                       "shape_2006", encoding='UTF-8')

brasil_2010 <- readOGR("D:/PIC/shapes/SHAPE_R/brasil_munic2/Brasil_cities.shp",
                       "Brasil_cities", encoding='UTF-8')

brasil_2010<-na.omit(brasil_2010)

##--------------------------------------------------------------
# iNICIANDO A CONSTRUÇÃO DOS MAPAS TEMÁTICOS
##-------------------------------------------------------------
####################################################################
###----USANDO SPPLOT-----###########################

x_1998<-brasil_2006$P_13_1998-brasil_2006$P_45_1998
brasil_2006$e_1998<-x_1998/max(abs(x_1998),na.rm = T)

x_2002<-brasil_2006$P_13_2002-brasil_2006$P_45_2002
brasil_2006$e_2002<-x_2002/max(abs(x_2002),na.rm = T)

x_2006<-brasil_2006$P_13_2006-brasil_2006$P_45_2006
brasil_2006$e_2006<-x_2006/max(abs(x_2006),na.rm = T)

x_2010<-brasil_2006$P_13_2010-brasil_2006$P_45_2010
brasil_2006$e_2010<-x_2010/max(abs(x_2010),na.rm = T)

x_2014<-brasil_2006$P_13_2014-brasil_2006$P_45_2014
brasil_2006$e_2014<-x_2014/max(abs(x_2014),na.rm = T)

x_mean<-brasil_2010$ptmean-brasil_2010$psdbmean
brasil_2010$E_mean<-x_mean/max(abs(x_mean),na.rm = T)

my.palette1<-c("#031182","#0E117B","#191275",
               "#24126E","#2F1368","#3B1362",
               "#46145B","#511455","#5C154E",
               "#671548","#731642","#7E163B",
               "#891735","#94172E","#9F1828",
               "#AB1822","#B6191B","#C11915",
               "#CC1A0E","#D71A08","#E31B02")

#===================================================================
# Mapas ============================================================
brks.eq1<-classIntervals(brasil_2006$e_1998,n=21,style="equal")
e_1998<-spplot(brasil_2006,"e_1998",col.regions=my.palette1,cuts = 7,
               sub="PSDB x PT 1998",col=NA,at=brks.eq1$brks)

brks.eq2<-classIntervals(brasil_2006$e_2002,n=21,style="equal")
e_2002<-spplot(brasil_2006,"e_2002",col.regions = my.palette1,sub="PSDB x PT 2002",
              col=NA,at=brks.eq2$brks)

brks.eq3<-classIntervals(brasil_2006$e_2006,n=21,style="equal")
e_2006<-spplot(brasil_2006,"e_2006",col.regions = my.palette1,sub="PSDB x PT 2006",
               col=NA,at=brks.eq3$brks)

brks.eq4<-classIntervals(brasil_2006$e_2010,n=21,style="equal")
e_2010<-spplot(brasil_2006,"e_2010",col.regions = my.palette1,
               sub="PSDB x PT 2010",col=NA,at=brks.eq4$brks)

brks.eq5<-classIntervals(brasil_2006$e_2014,n=21,style="equal")
e_2014<-spplot(brasil_2006,"e_2014",col.regions = my.palette1,
               sub="PSDB x PT 2014",col=NA,at=brks.eq5$brks)

brks.eq6<-classIntervals(brasil_2010$E_mean,n=21,style="equal")
E_mean<-spplot(brasil_2010,"E_mean",col.regions = my.palette1,
               sub="PSDB x PT Média",col=NA,at=brks.eq6$brks)

map_ptXpsdb<-grid.arrange(e_1998,e_2002,e_2006,e_2010,e_2014,E_mean,ncol=3,nrow=2)
dir<-"D:/PIC/Poster/map_ptXpsdb.jpeg"
ggsave(dir, plot = map_ptXpsdb, width = 17, height = 7, dpi = 1400)
