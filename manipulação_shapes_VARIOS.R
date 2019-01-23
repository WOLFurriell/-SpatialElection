#################################################################################
#---------------------------------------------------------
#  ADEQUANDO OS SHAPES
#---------------------------------------------------------
#################################################################################

library(rgdal)
require(maptools)

rm(list = ls())
dir <- "D:/PIC/shapes/shape_2007"

files <- list.files(dir,".shp");files
files <- substr(files,1,length(files)-19)
codfiles <- substr(files,1,2)
codfiles <- as.numeric(codfiles)

uf <- read.table("D:/PIC/COD_UF.txt")
uf$V1 <- as.character(uf$V1)
coduf <- substr(uf$V1,1,2) #Retirando os cods
nomeuf <- substr(uf$V1,nchar(uf$V1)-1,length(uf$V1));nomeuf #Retirando as Ufs
coduf <- as.numeric(coduf)
cbind(coduf,codfiles,nomeuf)

lista <- list()
for (i in 1:length(files)){
lista[[i]]  <-  readOGR(dir, files[i], encoding="Latin1")
if (codfiles[i] == coduf[i] ){
  lista[[i]]$UF <- nomeuf[i]  
  }
}
uid <- 1

poly.data <-  lista[[1]]
n  <-  length(slot(poly.data, "polygons"))
poly.data  <-  spChFIDs(poly.data, as.character(uid:(uid+n-1)))
uid  <-  uid + n

for (i in 2:length(files)) {
  temp.data  <-  lista[[i]]
  n  <-  length(slot(temp.data, "polygons"))
  temp.data  <-  spChFIDs(temp.data, as.character(uid:(uid+n-1)))
  uid  <-  uid + n
  poly.data  <-  spRbind(poly.data,temp.data)
}

shape <- poly.data

shape$NOME <- as.character(shape$NOME)
shape$NOME <- toupper(shape$NOME)
shape$NOME <- iconv(shape$NOME,to="ASCII//TRANSLIT")
shape$cod <- paste(shape$UF,shape$NOME,sep = "")
library(stringr)
shape$cod <- str_replace_all(shape$cod, " ", "")
shape <- shape[order(shape$cod),]

library(reshape)
names(shape)
shape  <-  rename(shape, c(UF="uf",NOME="cidade",CODIGO="codigo_ibg"))

writeOGR(obj=shape, dsn="D:/PIC/shapes/shape_2007", 
         layer="shape_2007", driver="ESRI Shapefile")

