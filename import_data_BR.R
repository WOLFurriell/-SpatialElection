library(data.table)  
library(dplyr)
require(bit64)
library(maptools)
library(agricolae)
library(plotrix)
require(classInt) 
library(reshape)
library(sqldf)
library(stringr)
library(tm)

#*****************************************************************************
#IMPORTAÇÃO E MERGE DE TODOS OS BANCOS DO TSE PARA TODOS OS ANOS
#*****************************************************************************

rm(list = ls())

wd<-"D:/PIC/BANCOS_PRESIDENTE/VOTO_PRESIDENTE/"
setwd(wd)
files <- list.files(path = wd,pattern = ".txt");files
lista <- lapply(files[2:5], fread, sep=";",header=F,drop=c(13:28,30))
br_tse <- rbindlist(lista)

write.table(br_tse,'D:/PIC/BANCOS_PRESIDENTE/MERGE/votacao_candidato_munzona_BR.txt',
            quote = F,sep=",",col.names = F,row.names = F)

#*****************************************************************************
#JUNTAR OS BANCOS DE 1998 ATÉ 2014
#*****************************************************************************

rm(list=ls())

wd<-"D:/PIC/BANCOS_PRESIDENTE/MERGE"
setwd(wd)
files <- list.files(path = wd,pattern = ".txt");files
lista <- lapply(files[1:2], fread, sep=",",header=F,na.strings=" ",drop=c(1,2,7,11))
all_tse <- rbindlist(lista)
#edit(all_tse)
saveRDS(all_tse, file="all_tse.Rds")

write.table(all_tse,'D:/PIC/BANCOS_PRESIDENTE/MERGE/all_tse_secaoBR.txt',
            quote = F,sep=",",col.names = F,row.names = F)

#*****************************************************************************
#OBTENÇÃO DA PROPORÇÃO DE VOTOS POR CANDIDATO
#*****************************************************************************

all_tse<-readRDS("all_tse.Rds")

#all_tse<-read.table("all_tse_secaoBR.txt",sep = ",")
all_tse <- rename(all_tse, c(V3="ano",V4='turno',V5='eleicao',V6='uf',V8='cod',V9='cidade',
                             V10='secao',V12='partido',V13='voto'))

#Excluindo as var inuteis
#myvars <- names(all_tse) %in% c("V1","V2", "V7","V11")
#all_tse <- all_tse[!myvars]

#Retirando acentuação e padronizando valores
all_tse$cidade <- iconv(all_tse$cidade,to="ASCII//TRANSLIT")
all_tse$voto<-as.integer(all_tse$voto)

#Organizando para o 1° turno
turno1<-subset(all_tse,all_tse$turno == 1)
turno1 <- turno1[order(turno1$ano,turno1$uf,turno1$cidade),] 
septse<-split(turno1,turno1$ano)

#Somando e contando as linhas do numero de votos por cidade e por eleição separados em lista
sumturno1 <- with(septse,lapply(septse, function (x) {aggregate(voto ~ (uf+cidade),x, sum)}))
sumpart <- with(septse,lapply(septse, function (x) {aggregate(voto ~ ano + uf + cidade + partido,
                                                              x, sum)}))
nturno1 <- with(sumpart,lapply(sumpart, function (x){aggregate(voto ~ (uf+cidade)
                                                               ,x,function(y){NROW(y)})}))
#Ordenar para repetir
sumturno1<-with(sumturno1,lapply(sumturno1,function (x) {x[order(x$uf,x$cidade),]}))
sumpart<-with(sumpart,lapply(sumpart,function (x) {x[order(x$uf,x$cidade),]}))
nturno1<-with(nturno1,lapply(nturno1,function (x) {x[order(x$uf,x$cidade),]}))

#repetir a total de votos segundo a quantidade de linha por cidade e ano em lista
for (i in 1:5){
  sumpart[[i]]$total <- rep(sumturno1[[i]]$voto,nturno1[[i]]$voto)
  print(i)}

#*****************************************************************************
#OBTENÇÃO DA PROPORÇÃO DE VOTOS POR CANDIDATO
#*****************************************************************************

#Fazer a proporção de votos
for (i in 1:5){
  sumpart[[i]]$voto_fr <- round((sumpart[[i]]$voto/sumpart[[i]]$total),4)
  print(i)}
edit(sumpart)
#Verificando se deu certo a proporção
teste <- with(sumpart,lapply(sumpart, function (x) {aggregate(voto_fr ~ (uf+cidade),x, sum)}))

#salvando o novo banco em RDS
saveRDS(sumpart, file="FINAL_TSEresultados.Rds")

## Criando um código
dados_tse<-readRDS("FINAL_TSEresultados.Rds")

for (i in 1:5){
  dados_tse[[i]]$cod<-paste(dados_tse[[i]]$uf, dados_tse[[i]]$cidade,sep = "")
  print(i)}
for (i in 1:5){
  dados_tse[[i]]$cod<-str_replace_all(dados_tse[[i]]$cod, " ", "")
  print(i)}
for (i in 1:5){
  dados_tse[[i]]$controle<-dados_tse[[i]]$cod
  print(i)}

dados_tse<-with(dados_tse,lapply(dados_tse,function (x) {x[order(x$cod),]}))

wd<-"D:/PIC/BANCOS_PRESIDENTE/MERGE"
setwd(wd)

#salvando o novo banco em RDS
saveRDS(dados_tse, file="FINAL_TSEresultados.Rds")

##-----------------------------------------------------
# Fazendo as alterações no shapefile

library(rgdal)

brasilshape <- readOGR("D:/PIC/shapes/BRASIL", "municipios_2010",
                  encoding='UTF-8')
names(brasilshape)
par(mar=c(0,0,0,0))
x11()
plot(brasil)

brasilshape$nome<-toupper(brasilshape$nome)
brasilshape$nome<-iconv(brasilshape$nome,to="ASCII//TRANSLIT")
brasilshape$cod<-paste(brasilshape$uf,brasilshape$nome,sep = "")
brasilshape$cod<-str_replace_all(brasilshape$cod, " ", "")
brasilshape<-brasilshape[order(brasilshape$cod),]

#################################################################################
#################################################################################
#---------------------------------------------------------
#REALIZANDO O MERGE ENTRE OS BANCOS
#---------------------------------------------------------
#################################################################################

rm(list = ls())
dados_tse <- readRDS("D:/PIC/BANCOS_PRESIDENTE/MERGE/FINAL_TSEresultados.Rds")
#Renomeando a var nome para cidade
names(brasilshape)
names(brasilshape)[2] <- "cidade"

#selecionando o ano e partido de interesse
#ESSE LOOP RETORNA UMA LISTA COM OS 3 PARTIDOS COM MAIS VOTOS EM CASA ELEIÇÃO 
n<-0
lista<-list()
x<-c(13,23,40,43,45,50)    
  for (i in 1:5){
    for (j in x){
n<-n+1      
lista[[n]] <-subset(dados_tse[[i]],
               dados_tse[[i]]$partido == j & dados_tse[[i]]$uf != 'ZZ')
  names(lista)[n] <- paste("P",j,names(dados_tse[i]), sep = "_")
    print(i)
    print(j)
    print(n)  
  }
}

library(reshape)
for (i in 1:30){
lista[[i]]<-rename(lista[[i]],c(voto_fr = names(lista)[i]))
print(i)
}

# REALIZANDO OS DROPS DAS VARIÁVEIS DE NÃO INTERESSE
pos<-c(1,2,5,7,9,11,13,17,18,19,22,23,25,27,29)
drops <- c("ano","partido","voto","total","controle")
for (i in pos){
    lista[[i]][ ,drops]<- list(NULL)
  print(names(lista[i]))
}

##ORDENANDO OS DADOS

lista<-lapply(lista,function(x){x[order(x$cod),]})
brasilshape<-brasilshape[order(brasilshape$cod),]

#REALIZANDO O MERGE DOS BANCOS 

brasilnew <- merge(brasilshape,lista[[1]],by=c("cod","uf","cidade"))
for (i in pos[2:length(pos)]){
brasilnew <- merge(brasilnew,lista[[i]],by=c("cod","uf","cidade"))
}


cbind(names(brasilnew))
names(brasilnew)
#P_13_1998
#P_23_1998
#P_45_1998
#P_13_2002
#P_45_2002
#P_40_2002
#P_13_2006
#P_45_2006
#P_50_2006
#P_13_2010
#P_45_2010
#P_43_2010
#P_13_2014
#P_45_2014
#P_40_2014

#EXPORTAR O SHAPEFILE
require(rgdal)
writeOGR(obj=brasilnew, dsn="D:/PIC/shapes/SHAPE_R/brasil_munic", 
         layer="Brasil_cities", driver="ESRI Shapefile")

saveRDS(brasilnew, file="D:/PIC/shapes/SHAPE_R/SHAPE_BRASIL.Rds")

