#Script para selecionar apenas atributos importantes da base
#Librarys
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(readr)
library(plyr)
library(dplyr)
library(Hmisc)
library(caret)

# Define o Diretorio de trabalho
workdir = "C:/Users/Suporte-CERCOMP/Documents/TCC/Scripts/CMF"
setwd(workdir)

#Leitura da Base de Dados
filename <- "CMF_raw.csv"
base <- read.csv(file=filename, sep=",", header=TRUE, strip.white=TRUE)

#Seleciona apenas atributos importantes

dados <- select(base, contains("depresd"), contains("lessint"), contains("moodelev"), contains("curdepr"),
				contains("curenjoy"), contains("curmania"), contains("abnirrit"), contains("curirrit"),
				contains("abnanx"), contains("deprmd"), contains("depsleep"), contains("depinter"),
				contains("depguilt"), contains("depse"), contains("depenerg"), contains("depconcn"),
				contains("depdist"), contains("depappet"), contains("deppmr"), contains("deppma"), 
				contains("elvselfe"), contains("elvsleep"), contains("elvtalk"), contains("elvfoi"),
				contains("elvdistr"), contains("elvgdact"), contains("elvpma"), contains("elvhrb"), 
				contains("depsi"), contains("silnwl"), contains("sipassiv"), contains("siactive"), 
				contains("depslmin"), contains("depslmax"), contains("tstcat"), contains("svcat"), 
				contains("smspi"), contains("smshallu"), contains("smsior"), contains("smsdelus"),
				contains("smsocd"))
				
#Preprocessamento: Muda todos os missing values para N/A			
dados$depresd <- revalue(dados$depresd, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$lessint <- revalue(dados$lessint, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$moodelev <- revalue(dados$moodelev, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$curdepr <- revalue(dados$curdepr, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$curenjoy <- revalue(dados$curenjoy, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$curmania <- revalue(dados$curmania, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$abnirrit <- revalue(dados$abnirrit, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$curirrit <- revalue(dados$curirrit, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$abnanx <- revalue(dados$abnanx, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$deprmd <- revalue(dados$deprmd, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depsleep <- revalue(dados$depsleep, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depinter <- revalue(dados$depinter, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depguilt <- revalue(dados$depguilt, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depse <- revalue(dados$depse, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depenerg <- revalue(dados$depenerg, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depconcn <- revalue(dados$depconcn, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depdist <- revalue(dados$depdist, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depappet <- revalue(dados$depappet, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$deppmr <- revalue(dados$deppmr, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$deppma <- revalue(dados$deppma, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$elvselfe <- revalue(dados$elvselfe, c("Not on original form" = "N/A", "Unknown" = "N/A", "Refused" = "N/A"))
dados$elvsleep <- revalue(dados$elvsleep, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$elvtalk <- revalue(dados$elvtalk, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$elvfoi <- revalue(dados$elvfoi, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$elvdistr <- revalue(dados$elvdistr, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$elvgdact <- revalue(dados$elvgdact, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$elvpma <- revalue(dados$elvpma, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$elvhrb <- revalue(dados$elvhrb, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depsi <- revalue(dados$depsi, c("Not on original form" = "N/A", "Unknown" = "N/A", "Refused" = "N/A"))
dados$silnwl <- revalue(dados$silnwl, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$sipassiv <- revalue(dados$sipassiv, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$siactive <- revalue(dados$siactive, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depslmin <- revalue(dados$depslmin, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$depslmax <- revalue(dados$depslmax, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$tstcat <- revalue(dados$tstcat, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$svcat <- revalue(dados$svcat, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$smspi <- revalue(dados$smspi, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$smshallu <- revalue(dados$smshallu, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$smsior <- revalue(dados$smsior, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$smsdelus <- revalue(dados$smsdelus, c("Not on original form" = "N/A", "Unknown" = "N/A"))
dados$smsocd <- revalue(dados$smsocd, c("Not on original form" = "N/A", "Unknown" = "N/A"))
				
#Salva a base

write.csv(dados, file="baselineReduzidaCat.csv", row.names=FALSE, na="")