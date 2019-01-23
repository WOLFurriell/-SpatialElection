#-----------------------------------------------
###   MANIPULAÇÕES NO SHAPEFILE  #####
#-----------------------------------------------
# importando o shape.
library(rgdal)
brasilshape  <-  readOGR("D:/PIC/shapes/SHAPE_R/brasil_munic2/Brasil_cities.shp",
                       "Brasil_cities", encoding='UTF-8')
names(brasilshape)

#---------------------------------------------------------------
# Média e CV por partido

pt <- brasilshape[,c("P_13_1998","P_13_2002","P_13_2006","P_13_2010","P_13_2014")]
pt <- as.data.frame(pt)
brasilshape$ptmean <- rowMeans(pt, na.rm = T, dims = 1)
brasilshape$ptcv <- apply(pt,1,function(x){100*(sd(x)/mean(x))})
maximo <- apply(pt,2,max,na.rm=TRUE)
mediana <- apply(pt,2,median,na.rm=TRUE)
media <- apply(pt,2,mean,na.rm=TRUE)
minimo <- apply(pt,2,min,na.rm=TRUE)
dev <- apply(pt,2,sd,na.rm=TRUE)
cv <- (dev/media)*100
ptdesc <- cbind(minimo,media,mediana,maximo,dev,cv)

psdb <- brasilshape[,c("P_45_1998","P_45_2002","P_45_2006","P_45_2010","P_45_2014")]
psdb <- as.data.frame(psdb)
brasilshape$psdbmean <- rowMeans(psdb, na.rm = T, dims = 1)
brasilshape$psdbcv <- apply(psdb,1,function(x){100*(sd(x)/mean(x))})
apply(psdb,2,max,na.rm=TRUE)

via3 <- brasilshape[,c("P_23_1998","P_40_2002","P_50_2006","P_43_2010","P_40_2014")]
via3 <- as.data.frame(via3)
brasilshape$via3mean <- rowMeans(via3, na.rm = T, dims = 1)
brasilshape$via3cv <- apply(via3,1,function(x){100*(sd(x)/mean(x))})
apply(via3,2,max,na.rm=TRUE)

#-----------------------------------------------------------------
# Criando Grandes Regiões
brasilshape$regiao <- brasilshape$uf
brasilshape$regiao <- as.character(brasilshape$regiao)
#Sul
brasilshape$regiao[brasilshape$regiao=="PR"] <- "Sul"
brasilshape$regiao[brasilshape$regiao=="SC"] <- "Sul"
brasilshape$regiao[brasilshape$regiao=="RS"] <- "Sul"
#Sudeste
brasilshape$regiao[brasilshape$regiao=="SP"] <- "Sudeste"
brasilshape$regiao[brasilshape$regiao=="MG"] <- "Sudeste"
brasilshape$regiao[brasilshape$regiao=="ES"] <- "Sudeste"
brasilshape$regiao[brasilshape$regiao=="RJ"] <- "Sudeste"
#Centro-oeste
brasilshape$regiao[brasilshape$regiao=="DF"] <- "Centro-oeste"
brasilshape$regiao[brasilshape$regiao=="MT"] <- "Centro-oeste"
brasilshape$regiao[brasilshape$regiao=="MS"] <- "Centro-oeste"
brasilshape$regiao[brasilshape$regiao=="GO"] <- "Centro-oeste"
#Norte
brasilshape$regiao[brasilshape$regiao=="PA"] <- "Norte"
brasilshape$regiao[brasilshape$regiao=="AM"] <- "Norte"
brasilshape$regiao[brasilshape$regiao=="AC"] <- "Norte"
brasilshape$regiao[brasilshape$regiao=="RO"] <- "Norte"
brasilshape$regiao[brasilshape$regiao=="AP"] <- "Norte"
brasilshape$regiao[brasilshape$regiao=="RR"] <- "Norte"
brasilshape$regiao[brasilshape$regiao=="TO"] <- "Norte"
#Nordeste
brasilshape$regiao[brasilshape$regiao=="MA"] <- "Nordeste"
brasilshape$regiao[brasilshape$regiao=="PI"] <- "Nordeste"
brasilshape$regiao[brasilshape$regiao=="BA"] <- "Nordeste"
brasilshape$regiao[brasilshape$regiao=="CE"] <- "Nordeste"
brasilshape$regiao[brasilshape$regiao=="RN"] <- "Nordeste"
brasilshape$regiao[brasilshape$regiao=="PB"] <- "Nordeste"
brasilshape$regiao[brasilshape$regiao=="PE"] <- "Nordeste"
brasilshape$regiao[brasilshape$regiao=="AL"] <- "Nordeste"
brasilshape$regiao[brasilshape$regiao=="SE"] <- "Nordeste"
table(brasilshape$regiao)

#-----------------------------------------------------------
#Exportando shape
require(rgdal)
writeOGR(obj=brasilshape, dsn="D:/PIC/shapes/SHAPE_R/brasil_munic2", 
         layer="Brasil_cities", driver="ESRI Shapefile")


names(brasilshape)






# Cortar a proporção de votos por faixas
brasilshape$cuts <- cut(brasilshape$P_13_1998,breaks = 5)
cortes <- cut(brasilshape$voto_fr,breaks = 5)
niveis  <-  levels(cortes)
cores  <-  heat.colors(length(levels(cortes)))
levels(cortes)  <-  cores 
