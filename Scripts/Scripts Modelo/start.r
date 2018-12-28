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
workdir = "C:/Users/Suporte-CERCOMP/Documents/TCC/Scripts"
setwd(workdir)

#Leitura da Base de Dados
filename <- "baseline.csv"
base <- read.csv(file=filename, sep=",", header=TRUE, strip.white=TRUE)

#Seleciona apenas atributos importantes

dados <- select(base, contains("stepid"), "tryhsui", contains("deprmd"), contains("depsleep"), contains("depinter"), contains("depguilt"), 
				contains("depconcn"), contains("depappet"), contains("deppmr"), contains("dage1st"), contains("mage1st"), contains("depsi"), contains("depenerg"),
				contains("depsev"), contains("depse"), contains("depdist"), contains("deppma"), contains("elvselfe"), "hxsui",
				contains("elvsleep"), contains("elvtalk"), contains("elvfoi"), contains("elvdistr"), contains("elvgdact"), contains("elvhrb"),
				contains("elvpma"), contains("majdepc"), contains("majdepp"), contains("moodgenc"), contains("moodgenp"), contains("moodsubc"),
				contains("moodsubp"), contains("manicc"), contains("hmanicc"), contains("manicp"), contains("hmanicp"), contains("typeiic"),
				contains("typeiip"), contains("clinstat"), contains("manlast"), contains("deppast"), contains("mangenc"), contains("mangenp"),
				contains("hypogenc"), contains("hypogenp"), contains("mansubc"), contains("mansubp"), contains("hyposubc"), contains("hyposubp"),
				contains("anxdisc"), contains("anxdisp"), contains("anxsubc"), contains("anxsubp"), contains("pdnoagc"), contains("pdnoagp"),
				contains("pdagorc"), contains("pdagorp"), contains("agorc"), contains("agorp"), contains("phobiac"), contains("phobiap"), 
				contains("ocdcur"), contains("ocdc"), contains("ocdpast"), contains("ocdp"), contains("ocdgenc"), contains("ocdgenp"), 
				contains("ocdsubc"), contains("ocdsubp"), contains("ptsdcur"), contains("ptsdc"), contains("ptsdpast"), contains("ptsdp"),
				contains("alcdepc"), contains("alcdepp"), contains("alcabusc"), contains("alcabusp"), contains("drugdepc"), contains("drugdepp"),
				contains("drugabc"), contains("drugabp"), contains("psychp"), contains("bpsychp"), contains("ipsychp"), contains("psychc"), 
				contains("bpsychc"), contains("ipsychc"), contains("schizc"), contains("schizp"), contains("schaffc"), contains("schaffp"), 
				contains("schphrc"), contains("schphrp"), contains("deldisc"), contains("deldisp"), contains("psygenc"), contains("psygenp"),
				contains("psysubc"), contains("psysubp"), contains("psynosc"), contains("psynosp"), contains("moodnosp"), contains("deppsyc"),
				contains("deppsyp"), contains("bulpurgc"), contains("bulpurgp"), contains("bulnonpc"), contains("bulnonpp"), contains("anbingec"),
				contains("anbingep"), contains("anrestrc"), contains("anrestrp"), contains("genanxc"), contains("genanxp"), contains("ganxgenc"), 
				contains("ganxgenp"), contains("gadsubc"), contains("gadsubp"), contains("addkidc"), contains("addkidp"), contains("addadltc"), 
				contains("addadltp"), contains("depsleepcat"), contains("depsicat"))
				
#Preprocessamento: Muda todos os missing values para N/A			

dados$majdepc <- revalue(dados$majdepc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$majdepp <- revalue(dados$majdepp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$moodgenc <- revalue(dados$moodgenc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$moodgenp <- revalue(dados$moodgenp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$moodsubc <- revalue(dados$moodsubc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$moodsubp <- revalue(dados$moodsubp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$manicc <- revalue(dados$manicc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$hmanicc <- revalue(dados$hmanicc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$manicp <- revalue(dados$manicp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$hmanicp <- revalue(dados$hmanicp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$typeiic <- revalue(dados$typeiic, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$typeiip <- revalue(dados$typeiip, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$mangenc <- revalue(dados$mangenc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$clinstat <- revalue(dados$clinstat, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$hxsui <- revalue(dados$hxsui, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$tryhsui <- revalue(dados$tryhsui, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$manlast <- revalue(dados$manlast, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$deppast <- revalue(dados$deppast, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$dage1st <- revalue(dados$dage1st, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$mage1st <- revalue(dados$mage1st, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$mangenp <- revalue(dados$mangenp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$hypogenc <- revalue(dados$hypogenc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$hypogenp <- revalue(dados$hypogenp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$mansubc <- revalue(dados$mansubc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$mansubp <- revalue(dados$mansubp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$hyposubc <- revalue(dados$hyposubc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$hyposubp <- revalue(dados$hyposubp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$anxdisc <- revalue(dados$anxdisc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$anxdisp <- revalue(dados$anxdisp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$anxsubc <- revalue(dados$anxsubc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$anxsubp <- revalue(dados$anxsubp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$pdnoagc <- revalue(dados$pdnoagc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$pdnoagp <- revalue(dados$pdnoagp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$pdagorc <- revalue(dados$pdagorc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$pdagorp <- revalue(dados$pdagorp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$agorc <- revalue(dados$agorc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$agorp <- revalue(dados$agorp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$phobiac <- revalue(dados$phobiac, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$phobiap <- revalue(dados$phobiap, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ocdcur <- revalue(dados$ocdcur, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ocdc <- revalue(dados$ocdc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ocdpast <- revalue(dados$ocdpast, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ocdp <- revalue(dados$ocdp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ocdgenc <- revalue(dados$ocdgenc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ocdgenp <- revalue(dados$ocdgenp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ocdsubc <- revalue(dados$ocdsubc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ocdsubp <- revalue(dados$ocdsubp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ptsdcur <- revalue(dados$ptsdcur, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ptsdc <- revalue(dados$ptsdc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ptsdpast <- revalue(dados$ptsdpast, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ptsdp <- revalue(dados$ptsdp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$alcdepc <- revalue(dados$alcdepc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$alcdepp <- revalue(dados$alcdepp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$alcabusc <- revalue(dados$alcabusc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$alcabusp <- revalue(dados$alcabusp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$drugdepc <- revalue(dados$drugdepc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$drugdepp <- revalue(dados$drugdepp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$drugabc <- revalue(dados$drugabc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$drugabp <- revalue(dados$drugabp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$psychp <- revalue(dados$psychp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$bpsychp <- revalue(dados$bpsychp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ipsychp <- revalue(dados$ipsychp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$psychc <- revalue(dados$psychc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$bpsychc <- revalue(dados$bpsychc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ipsychc <- revalue(dados$ipsychc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$schizc <- revalue(dados$schizc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$schizp <- revalue(dados$schizp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$schaffc <- revalue(dados$schaffc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$schaffp <- revalue(dados$schaffp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$schphrc <- revalue(dados$schphrc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$schphrp <- revalue(dados$schphrp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$deldisc <- revalue(dados$deldisc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$deldisp <- revalue(dados$deldisp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$psygenc <- revalue(dados$psygenc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$psygenp <- revalue(dados$psygenp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$psysubc <- revalue(dados$psysubc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$psysubp <- revalue(dados$psysubp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$psynosc <- revalue(dados$psynosc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$psynosp <- revalue(dados$psynosp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$moodnosp <- revalue(dados$moodnosp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$deppsyc <- revalue(dados$deppsyc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$deppsyp <- revalue(dados$deppsyp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$bulpurgc <- revalue(dados$bulpurgc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$bulpurgp <- revalue(dados$bulpurgp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$bulnonpc <- revalue(dados$bulnonpc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$bulnonpp <- revalue(dados$bulnonpp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$anbingec <- revalue(dados$anbingec, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$anbingep <- revalue(dados$anbingep, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$anrestrc <- revalue(dados$anrestrc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$anrestrp <- revalue(dados$anrestrp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$genanxc <- revalue(dados$genanxc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$genanxp <- revalue(dados$genanxp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ganxgenc <- revalue(dados$ganxgenc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$ganxgenp <- revalue(dados$ganxgenp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$gadsubc <- revalue(dados$gadsubc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$gadsubp <- revalue(dados$gadsubp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$addkidc <- revalue(dados$addkidc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$addkidp <- revalue(dados$addkidp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$addadltc <- revalue(dados$addadltc, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$addadltp <- revalue(dados$addadltp, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depsleepcat <- revalue(dados$depsleepcat, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depsicat <- revalue(dados$depsicat, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$stepid <- revalue(dados$stepid, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$deprmd <- revalue(dados$deprmd, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depsleep <- revalue(dados$depsleep, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depinter <- revalue(dados$depinter, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depguilt <- revalue(dados$depguilt, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depenerg <- revalue(dados$depenerg, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depconcn <- revalue(dados$depconcn, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depappet <- revalue(dados$depappet, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$deppmr <- revalue(dados$deppmr, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depsi <- revalue(dados$depsi, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depsev <- revalue(dados$depsev, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depse <- revalue(dados$depse, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$depdist <- revalue(dados$depdist, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$deppma <- revalue(dados$deppma, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$elvselfe <- revalue(dados$elvselfe, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$elvsleep <- revalue(dados$elvsleep, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$elvtalk <- revalue(dados$elvtalk, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$elvfoi <- revalue(dados$elvfoi, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$elvdistr <- revalue(dados$elvdistr, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$elvgdact <- revalue(dados$elvgdact, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$elvhrb <- revalue(dados$elvhrb, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$elvpma <- revalue(dados$elvpma, c("Unknown" = "N/A", "Refused" = "N/A"))
dados$deprmd <- revalue(dados$deprmd, c("Not on original form" = "N/A"))
dados$dage1stcat <- revalue(dados$dage1stcat, c("Unknown" = "N/A"))
dados$mage1stcat <- revalue(dados$mage1stcat, c("Unknown" = "N/A"))	
				
#Salva a base

write.csv(dados, file="baselineReduzidaCat.csv", row.names=FALSE, na="")