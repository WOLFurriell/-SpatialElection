library(ggplot2); library(ggthemes); library(rgdal); library(reshape);
library(gridExtra); library(dplyr); library(lattice)
options(OutDec = ",")

rm(list = ls())

# importando o shape --------------------------------------------------
brasilshape <- readOGR("D:/PIC/shapes/SHAPE_R/brasil_munic2/Brasil_cities.shp",
                       "Brasil_cities", encoding='UTF-8')
#---------------------------------------------------------------------
# Manipulação ----------------------------------------------
#------------------------------------------------------------

ptreg1 <- as.data.frame(brasilshape[,c("P_13_1998","P_13_2002","P_13_2006",
                                     "P_13_2010","P_13_2014","regiao")])
ptreg  <- as.data.frame(ptreg1 %>% group_by(regiao) %>%
                       summarise_each(funs(mean(., na.rm=TRUE))))
ptreg$partido <- "PT"

psdbreg1 <- as.data.frame(brasilshape[,c("P_45_1998","P_45_2002","P_45_2006",
                                       "P_45_2010","P_45_2014","regiao")])
psdbreg  <- as.data.frame(psdbreg1%>%group_by(regiao)%>%
                         summarise_each(funs(mean(., na.rm=TRUE))))
psdbreg$partido <- "PSDB"


mdata1 <- melt(ptreg, id = c("regiao", "partido"))
mdata2 <- melt(psdbreg, id = c("regiao", "partido"))

regiao_all <- rbind(mdata1,mdata2)
regiao_all$variable <- as.character(regiao_all$variable)
word <- regiao_all$variable
regiao_all$year <- substr(word,nchar(word)-3, nchar(word))

#---------------------------------------------------------------------
# Gráficos
#------------------------------------------------------------
# CORES
cbbPalette <- c("#00147E","#990000")
tema1<-  theme_igray()  +  
  theme(axis.text.x = element_text(angle = 40, hjust = 1,color="black"),
  axis.text.y = element_text(color="black"),
  strip.background = element_rect(fill="black"),
  strip.text.x = element_text(size=10, color="white",face="bold"))

tema2<-  theme_igray()  +  
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        strip.background = element_rect(fill="black"),
        strip.text.x = element_text(size=10, color="white",face="bold"))

#---------------------------------------------------------------------
#-REGIÃO -----------------------------------------------------------
#---------------------------------------------------------------------

preg <- ggplot(regiao_all, aes(x=year,y=value*100,group=partido)) + 
  geom_line(aes(colour=partido),size=1) + 
  geom_point(aes(colour=partido),size=2) + 
  labs(y="% Voto", x=NULL)  +  facet_grid(.~regiao) + 
  ggtitle("Resultados por região") + 
  labs(colour = "") + scale_color_manual(values=cbbPalette,
                                       name="Partidos") + tema1

#---------------------------------------------------------------------
#-- ANO ---------------------------------------------------------
#---------------------------------------------------------------------
pyear <- ggplot(regiao_all, aes(x=regiao,y=value*100,group=partido)) + 
  geom_bar(aes(fill=partido), stat="identity",position=position_dodge())  + 
  ggtitle("Resultados por ano")  + 
  labs(y="% Voto", x=NULL)  +  facet_grid(.~year)  + 
  scale_fill_manual(values=cbbPalette,name="Parties") + tema1

reg_year <- grid.arrange(preg,pyear,ncol=1,nrow=2)
dir      <- "D:/PIC/Poster/reg_year.pdf"
ggsave(dir, plot = reg_year, width = 16.2, height = 6, dpi = 350)

#---------------------------------------------------------------------
#-- Box-plot ---------------------------------------------------------
#---------------------------------------------------------------------
# PT
ptbox <- brasilshape[,c("P_13_1998","P_13_2002","P_13_2006","P_13_2010",
                      "P_13_2014","regiao")]
ptbox <- as.data.frame(ptbox)
ptbox$partido <- "PT"

ptbox          <- melt(ptbox, id = c("regiao","partido"))
ptbox$variable <- as.character(ptbox$variable)
ptbox$year     <- substr(ptbox$variable,nchar(ptbox$variable)-3, 9)
# PSDB
psdbbox <- brasilshape[,c("P_45_1998","P_45_2002","P_45_2006","P_45_2010",
                        "P_45_2014","regiao")]
psdbbox          <- as.data.frame(psdbbox)
psdbbox$partido  <- "PSDB"
psdbbox          <- melt(psdbbox, id=c("regiao","partido"))
psdbbox$variable <- as.character(psdbbox$variable)
psdbbox$year     <- substr(psdbbox$variable,nchar(psdbbox$variable)-3,9)
names(ptbox)
names(psdbbox)
ambos <- rbind(ptbox,psdbbox)
names(ambos)
#--BOXPLOT ----------------------------------------------------------

boxplot <- ggplot(ambos, aes(x=factor(year),y=value*100))  +  
  geom_boxplot(aes(fill = factor(partido)),colour="black")  + 
  ggtitle("Results by region") + 
  scale_fill_manual(values=cbbPalette,name="Parties") + 
  labs(colour = "") + labs(y="% Voto", x=NULL) + 
  tema2
dir <- "D:/PIC/Poster/boxplot.pdf"
ggsave(dir, plot = boxplot, width = 6, height = 5, dpi = 350)

#----------------------------------------------------------------
#- Gráfico geral ------------------------------------------------
#-----------------------------------------------------------------
geral           <- read.table("D:/PIC/BANCOS/Geral.csv",sep=";",header = T,dec = ".")
geral1          <- melt(geral, id=c("X"))
geral1$variable <- as.character(geral1$variable)
geral1          <- subset(geral1,geral1$variable!="via3")

flevels <- rev(geral1$X)
p <- ggplot(geral1, aes(x=X,y=value*100,group=variable)) + 
        geom_line(aes(colour=variable),size=1) + 
        geom_point(aes(colour=variable),size=3) + 
        expand_limits(x=c(1988,2015)) + 
          scale_color_manual(values=c("navyblue","#990000")) + 
          labs(y="% Voto", x=NULL) + scale_x_discrete(limits=flevels) + 
          ggtitle("Presidential election results in 1989 - 2014") + 
          labs(colour = "Party") + tema2

box_p <- grid.arrange(p,boxplot,ncol=2,nrow=1)
dir   <- "D:/PIC/Poster/box_p.pdf"
ggsave(dir, plot = box_p, width = 16.5, height = 4, dpi = 350)














