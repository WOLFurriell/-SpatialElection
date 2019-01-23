#=====================================================================
# regressão Beta PIC
#=====================================================================

rm(list=ls())
library(rgdal)
library(betareg)
library(simplexreg)
library(openxlsx)
library(lmtest) 
library(tidyverse)
library(xtable)
#=====================================================================
# Importando os dados

brasilshape <- readOGR("D:/PIC/shapes/SHAPE_R/brasil_munic2/Brasil_cities.shp",
                       "Brasil_cities", encoding='UTF-8')
#=====================================================================
# Importando os dados
setwd("D:/Estatística/Modelos Lineares Generalizados MESTRADO/Trabalhos/Trabalho_final/trab_final_BETA")

idh<-read.table("IDHM2010.txt", sep = "\t", header=T)
idh<-na.omit(idh)
idh$regiao <- as.factor(idh$regiao)
idh2<-read.xlsx("idhcompleto.xlsx")
names(idh2)
subidh2 <- idh2 %>% filter(ANO == 2010)
IDH2010 <- merge(subidh2, idh, by = "Codmun7")

IDH2010<-subset(IDH2010,regiao != "Distrito Federal")

IDH2010$AGUA_ESGOTO<-IDH2010$AGUA_ESGOTO/100
IDH2010$MORT1<-IDH2010$MORT1/100
IDH2010$T_FLSUPER<-IDH2010$T_FLSUPER/100
IDH2010$PINDCRI<-IDH2010$PINDCRI/100

#---------------------------------------------------------------------
# Manipulação ----------------------------------------------

PTreg<-as.data.frame(brasilshape)
dim(PTreg)

PTreg$LAT<-coordinates(brasilshape)[,1]
PTreg$LONG<-coordinates(brasilshape)[,2]

colnames(PTreg)[8]<-"Codmun7"
dados <- merge(PTreg,IDH2010,by=c("Codmun7"))

dados$popurb<-dados$pesourb.y/dados$pesotot.y
dados$EMP<-dados$EMP/100
dados$T_FBSUPER<-dados$T_FBSUPER/100

#=====================================================================
# Análise descritiva

dir<-"D:/PIC/RELATORIO/graph/"
setwd(dir)

g1<-ggplot(dados, aes(x=IDHM_R,y=P_13_2010,color=regiao.x))+
  geom_point() + ylab("Proporção de votos no PT") + 
  xlab("IDH dimensão renda") +
  scale_colour_hue(l=50,name="Região");g1
ggsave(g1, file="g1.jpeg", width=9, height=6,dpi=300)

g2<-ggplot(dados, aes(x=IDHM_E,y=P_13_2010,color=regiao.x))+
  geom_point() + ylab("Proporção de votos no PT") + 
  xlab("IDH dimensão educação") +
  scale_colour_hue(l=50,name="Região")  
ggsave(g2, file="g2.jpeg", width=9, height=6,dpi=300)

g3<-ggplot(dados, aes(x=IDHM_L,y=P_13_2010,color=regiao.x))+
  geom_point() + ylab("Proporção de votos no PT") + 
  xlab("IDH dimensão longevidade") +
  scale_colour_hue(l=50,name="Região")
ggsave(g3, file="g3.jpeg", width=9, height=6,dpi=300)

g4<-ggplot(dados, aes(x=T_FBSUPER,y=P_13_2010,color=regiao.x))+
  geom_point() + ylab("Proporção de votos no PT") + 
  xlab("Acesso ao ensino superior") +
  scale_colour_hue(l=50,name="Região")
ggsave(g4, file="g4.jpeg", width=9, height=6,dpi=300)

g5<-ggplot(dados, aes(x=GINI,y=P_13_2010,color=regiao.x))+
  geom_point() + ylab("Proporção de votos no PT") + 
  xlab("GINI") +
  scale_colour_hue(l=50,name="Região")
ggsave(g5, file="g5.jpeg", width=9, height=6,dpi=300)

g6<-ggplot(dados, aes(x=popurb,y=P_13_2010,color=regiao.x))+
  geom_point() + ylab("Proporção de votos no PT") + 
  xlab("População urbana") +
  scale_colour_hue(l=50,name="Região")
ggsave(g6, file="g6.jpeg", width=9, height=6,dpi=300)

g7<-ggplot(dados, aes(x=TRABCC,y=P_13_2010,color=regiao.x))+
  geom_point() + ylab("Proporção de votos no PT") + 
  xlab("Empregados com carteira") +
  scale_colour_hue(l=50,name="Região")
ggsave(g7, file="g6.jpeg", width=9, height=6,dpi=300)

box1<-ggplot(dados, aes(regiao.x,P_13_2010,colour=regiao.x,fill=regiao.x)) +
  geom_boxplot(colour ="black") + geom_jitter(alpha=0.3) +
  scale_colour_hue(l=50,name="Região") + labs(fill="Região")+
  labs(x = "Região",y="Proporção de votos no PT") + guides(colour=FALSE)
ggsave(box1, file="box1.jpeg", width=9, height=6,dpi=300)

#=====================================================================
# Modelo de regressão Beta

names(dados)

mlogit <- betareg(P_13_2010 ~ IDHM_R+IDHM_L+IDHM_E+T_FBSUPER+GINI+
                  popurb+EMP+LAT+LONG+regiao.y
                  |regiao.y, data = dados,
                  link = "logit", link.phi = "log")

mlogit2 <- betareg(P_13_2010 ~ IDHM_R+IDHM_L+IDHM_E+T_FBSUPER+GINI+
                    popurb+EMP+LAT+LONG+regiao.y
                  |1, data = dados,
                  link = "probit", link.phi = "log")

lrtest(mlogit2,mlogit)
xtable(lrtest(mlogit2,mlogit))
result<-summary(mlogit)

result<-summary(mlogit)

xtable(result$coefficients$mean)

xtable(result$coefficients$precision)

#=====================================================================
# Coeficientes

library(broom)
coef <- tidy(mlogit, conf.int = TRUE)
coef<-coef[coef$component=="precision",]
coef<-coef[!coef$term=="(Intercept)",]
cbbPalette <- c("#00147E", "#990000")
coef$p.id<-ifelse(coef$p.value<0.05,"p<0.05","p=>0.05")%>%as.factor()
gcoef<- ggplot(coef, aes(term, estimate,colour=p.id))+coord_flip() +
  geom_point(aes(colour=p.id),size=3) + 
  geom_hline(yintercept=0,size=1) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  labs(title = "Coeficientes para a precisão do modelo") + 
  scale_color_manual(values=cbbPalette,name="Valor P") +
  ylab("Estimativas") + xlab("Variáveis")
gcoef

ggsave(gcoef, file="coefprecisao.jpg", width=10, height=8,dpi=300)

#=====================================================================
# Resíduos

mbeta<-mlogit

jpeg("residbeta.jpg",units="in",width =12,height=7,res = 600)
par(mfrow=c(1,2))
plot(mbeta, which = 1, type = "sweighted2", 
     sub.caption = "",col="#000066",pch=20,cex =.9,lty=1,
     panel.first = c(abline(h = 1, lwd=700, col = "gray90")))
grid(lwd = 1.5,col="white")
abline(h=0,col="red",lwd=2)
plot(mbeta, which = 4, type = "sweighted2", 
     sub.caption = "",col="#000066",pch=20,cex =.9,lty=1,
     panel.first = c(abline(h = 1, lwd=700, col = "gray90")))
grid(lwd = 1.5,col="white")
abline(h=0,col="red",lwd=2)
dev.off()

h<-gleverage(mbeta)
hi<-ifelse(h>(2*20)/4063,h,NA)
lev<-data.frame(h,hi,seq(1,length(hi)),predict(mbeta))
colnames(lev) <- c("h","hid", "id","pred")

jpeg("levcook.jpg",units="in",width =12,height=7,res = 600)
par(mfrow=c(1,2))
plot(mbeta, which = 2, type = "sweighted2", 
     sub.caption = "",col="#000066",pch=20,lty=1,lwd=1)
abline(h=(2*(20-1))/4063,col="red",lwd=1)
grid(lwd = 1.5)
plot(mbeta, which = 3, type = "sweighted2", 
     sub.caption = "",col="#000066",pch=20,lty=1,cex =.9,
     panel.first = c(abline(h = 1, lwd=1600, col = "gray90")))
grid(lwd = 1.5,col="gray80")
text(x=lev$pred, y=lev$hi, labels=lev$id,cex =.6,pos = 4)
points(x=lev$pred, y=lev$hi, col="red")
dev.off()

jpeg("predEnv.jpg",units="in",width =8,height=6,res = 600)
plot(mlogit2, which = 5, sub.caption = "",col="#000066",pch=20,cex =.8,
     lty=1)
dev.off()
