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
filename <- "baselineReduzidaCat.csv"
dados <- read.csv(file=filename, sep=",", header=TRUE, strip.white=TRUE)

#Transformar N/A em -99
dados$depresd <- revalue(dados$depresd, c("N/A" = -99))
dados$lessint <- revalue(dados$lessint, c("N/A" = -99))
dados$moodelev <- revalue(dados$moodelev, c("N/A" = -99))
dados$curdepr <- revalue(dados$curdepr, c("N/A" = -99))
dados$curenjoy <- revalue(dados$curenjoy, c("N/A" = -99))
dados$curmania <- revalue(dados$curmania, c("N/A" = -99))
dados$abnirrit <- revalue(dados$abnirrit, c("N/A" = -99))
dados$curirrit <- revalue(dados$curirrit, c("N/A" = -99))
dados$abnanx <- revalue(dados$abnanx, c("N/A" = -99))
dados$deprmd <- revalue(dados$deprmd, c("N/A" = -99))
dados$depsleep <- revalue(dados$depsleep, c("N/A" = -99))
dados$depinter <- revalue(dados$depinter, c("N/A" = -99))
dados$depguilt <- revalue(dados$depguilt, c("N/A" = -99))
dados$depse <- revalue(dados$depse, c("N/A" = -99))
dados$depenerg <- revalue(dados$depenerg, c("N/A" = -99))
dados$depconcn <- revalue(dados$depconcn, c("N/A" = -99))
dados$depdist <- revalue(dados$depdist, c("N/A" = -99))
dados$depappet <- revalue(dados$depappet, c("N/A" = -99))
dados$deppmr <- revalue(dados$deppmr, c("N/A" = -99))
dados$deppma <- revalue(dados$deppma, c("N/A" = -99))
dados$elvselfe <- revalue(dados$elvselfe, c("N/A" = -99))
dados$elvsleep <- revalue(dados$elvsleep, c("N/A" = -99))
dados$elvtalk <- revalue(dados$elvtalk, c("N/A" = -99))
dados$elvfoi <- revalue(dados$elvfoi, c("N/A" = -99))
dados$elvdistr <- revalue(dados$elvdistr, c("N/A" = -99))
dados$elvgdact <- revalue(dados$elvgdact, c("N/A" = -99))
dados$elvpma <- revalue(dados$elvpma, c("N/A" = -99))
dados$elvhrb <- revalue(dados$elvhrb, c("N/A" = -99))
dados$depsi <- revalue(dados$depsi, c("N/A" = -99))
dados$silnwl <- revalue(dados$silnwl, c("N/A" = -99))
dados$sipassiv <- revalue(dados$sipassiv, c("N/A" = -99))
dados$siactive <- revalue(dados$siactive, c("N/A" = -99))
dados$depslmin <- revalue(dados$depslmin, c("N/A" = -99))
dados$depslmax <- revalue(dados$depslmax, c("N/A" = -99))
dados$tstcat <- revalue(dados$tstcat, c("N/A" = -99))
dados$svcat <- revalue(dados$svcat, c("N/A" = -99))
dados$smspi <- revalue(dados$smspi, c("N/A" = -99))
dados$smshallu <- revalue(dados$smshallu, c("N/A" = -99))
dados$smsior <- revalue(dados$smsior, c("N/A" = -99))
dados$smsdelus <- revalue(dados$smsdelus, c("N/A" = -99))
dados$smsocd <- revalue(dados$smsocd, c("N/A" = -99))

#Transformar dados categoricos em numericos

dados$depslmin <- revalue(dados$depslmin, c("[-1,0)" = 0,"[0,1)" = 1,"[1,3)" = 2,"[3,5)" = 3,"[5,7)" = 4,
										  "[7,9)" = 5,"[9,11)" = 6,"[11,13)" = 7,"[13,15)" = 8,"[15,17)" = 9,
										  "[17,19)" = 10,"[19,21)" = 11,"[21,23]" = 12))
										  
dados$depslmax <- revalue(dados$depslmax, c("[-1,0)" = 0,"[0,1)" = 1,"[1,3)" = 2,"[3,5)" = 3,"[5,7)" = 4,
										  "[7,9)" = 5,"[9,11)" = 6,"[11,13)" = 7,"[13,15)" = 8,"[15,17)" = 9,
										  "[17,19)" = 10,"[19,21)" = 11,"[21,23)" = 12, "[23,24]" = 13))
										  
dados$silnwl <- revalue(dados$silnwl, c("Yes" = 1, "No" = 0))
dados$sipassiv <- revalue(dados$sipassiv, c("Yes" = 1, "No" = 0))
dados$siactive <- revalue(dados$siactive, c("Yes" = 1, "No" = 0))

dados$depresd <- revalue(dados$depresd, c("[0,1)" = 0, "[1,11)" = 1,"[11,21)" = 2,"[21,31)" = 3,"[31,41)" = 4,
											"[41,51)" = 5,"[51,61)" = 6,"[61,71)" = 7,"[71,81)" = 8,
											"[81,91)" = 9,"[91,100]" = 10))

dados$lessint <- revalue(dados$lessint, c("[1,11)" = 1,"[11,21)" = 2,"[21,31)" = 3,"[31,41)" = 4,
											"[41,51)" = 5,"[51,61)" = 6,"[61,71)" = 7,"[71,81)" = 8,
											"[81,91)" = 9,"[91,100]" = 10))
											
dados$moodelev <- revalue(dados$moodelev, c("[1,11)" = 1,"[11,21)" = 2,"[21,31)" = 3,"[31,41)" = 4,
											"[41,51)" = 5,"[51,61)" = 6,"[61,71)" = 7,"[71,81)" = 8,
											"[81,91)" = 9,"[91,100]" = 10))
											
dados$abnirrit <- revalue(dados$abnirrit, c("[0,1)" = 0, "[1,11)" = 1,"[11,21)" = 2,"[21,31)" = 3,"[31,41)" = 4,
											"[41,51)" = 5,"[51,61)" = 6,"[61,71)" = 7,"[71,81)" = 8,
											"[81,91)" = 9,"[91,100]" = 10))
											
dados$abnanx <- revalue(dados$abnanx, c("[1,11)" = 1,"[11,21)" = 2,"[21,31)" = 3,"[31,41)" = 4,
											"[41,51)" = 5,"[51,61)" = 6,"[61,71)" = 7,"[71,81)" = 8,
											"[81,91)" = 9,"[91,100]" = 10))
											
dados$curdepr <- revalue(dados$curdepr, c("Definite" = 1, "No" = 0, "Probable" = 2))

dados$curenjoy <- revalue(dados$curenjoy, c("Definite" = 1, "No" = 0, "Probable" = 2))

dados$curmania <- revalue(dados$curmania, c("Definite" = 1, "No" = 0, "Probable" = 2))

dados$curirrit <- revalue(dados$curirrit, c("Definite" = 1, "No" = 0, "Probable" = 2))

write.csv(dados, file="baseNumerica.csv", row.names=FALSE, na="")

