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
workdir = "C:/Users/Stick/Desktop/TCC/Scripts"
setwd(workdir)

#Leitura da Base de Dados
filename <- "baseline.csv"
base <- read.csv(file=filename, sep=",", header=TRUE, strip.white=TRUE)

#Seleciona apenas atributos importantes

dados <- select(base, contains("stepid"), contains("dayscons_ade"), contains("dayscons_weeks_ade"), contains("dayscons_mini"),
				contains("dayscons_weeks_mini"), contains("dayscons_x"), contains("dayscons_weeks_x"), contains("dayscons_y"), 
				contains("dayscons_weeks_y"), contains("certcode_ade"), contains("certcode_mini"), contains("certcode"), 
				contains("deprmd"), contains("depsleep"), contains("depinter"), contains("depguilt"), contains("depenerg"), 
				contains("depconcn"), contains("depappet"), contains("deppmr"), contains("dage1st"), contains("mage1st"), contains("depsi"),
				contains("depsev"), contains("depse"), contains("depdist"), contains("deppma"), contains("elvselfe"), 
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
				
#Salva a base

write.csv(dados, file="baselineReduzida.csv", row.names=FALSE, na="")
				
#Transformar dados categoricos em numericos Yes = 1, No = 0, N/A = -1, Unknown = -2, Refused = -3

dados$majdepc <- revalue(dados$majdepc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$majdepp <- revalue(dados$majdepp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$moodgenc <- revalue(dados$moodgenc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$moodgenp <- revalue(dados$moodgenp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$moodsubc <- revalue(dados$moodsubc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$moodsubp <- revalue(dados$moodsubp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$manicc <- revalue(dados$manicc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$hmanicc <- revalue(dados$hmanicc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$manicp <- revalue(dados$manicp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$hmanicp <- revalue(dados$hmanicp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$typeiic <- revalue(dados$typeiic, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$typeiip <- revalue(dados$typeiip, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$mangenc <- revalue(dados$mangenc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$clinstat <- revalue(dados$clinstat, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$manlast <- revalue(dados$manlast, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$deppast <- revalue(dados$deppast, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$dage1st <- revalue(dados$dage1st, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$mage1st <- revalue(dados$mage1st, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$mangenp <- revalue(dados$mangenp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$hypogenc <- revalue(dados$hypogenc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$hypogenp <- revalue(dados$hypogenp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$mansubc <- revalue(dados$mansubc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$mansubp <- revalue(dados$mansubp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$hyposubc <- revalue(dados$hyposubc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$hyposubp <- revalue(dados$hyposubp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$anxdisc <- revalue(dados$anxdisc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$anxdisp <- revalue(dados$anxdisp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$anxsubc <- revalue(dados$anxsubc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$anxsubp <- revalue(dados$anxsubp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$pdnoagc <- revalue(dados$pdnoagc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$pdnoagp <- revalue(dados$pdnoagp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$pdagorc <- revalue(dados$pdagorc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$pdagorp <- revalue(dados$pdagorp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$agorc <- revalue(dados$agorc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$agorp <- revalue(dados$agorp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$phobiac <- revalue(dados$phobiac, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$phobiap <- revalue(dados$phobiap, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ocdcur <- revalue(dados$ocdcur, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ocdc <- revalue(dados$ocdc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ocdpast <- revalue(dados$ocdpast, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ocdp <- revalue(dados$ocdp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ocdgenc <- revalue(dados$ocdgenc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ocdgenp <- revalue(dados$ocdgenp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ocdsubc <- revalue(dados$ocdsubc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ocdsubp <- revalue(dados$ocdsubp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ptsdcur <- revalue(dados$ptsdcur, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ptsdc <- revalue(dados$ptsdc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ptsdpast <- revalue(dados$ptsdpast, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ptsdp <- revalue(dados$ptsdp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$alcdepc <- revalue(dados$alcdepc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$alcdepp <- revalue(dados$alcdepp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$alcabusc <- revalue(dados$alcabusc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$alcabusp <- revalue(dados$alcabusp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$drugdepc <- revalue(dados$drugdepc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$drugdepp <- revalue(dados$drugdepp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$drugabc <- revalue(dados$drugabc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$drugabp <- revalue(dados$drugabp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$psychp <- revalue(dados$psychp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$bpsychp <- revalue(dados$bpsychp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ipsychp <- revalue(dados$ipsychp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$psychc <- revalue(dados$psychc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$bpsychc <- revalue(dados$bpsychc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ipsychc <- revalue(dados$ipsychc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$schizc <- revalue(dados$schizc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$schizp <- revalue(dados$schizp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$schaffc <- revalue(dados$schaffc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$schaffp <- revalue(dados$schaffp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$schphrc <- revalue(dados$schphrc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$schphrp <- revalue(dados$schphrp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$deldisc <- revalue(dados$deldisc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$deldisp <- revalue(dados$deldisp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$psygenc <- revalue(dados$psygenc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$psygenp <- revalue(dados$psygenp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$psysubc <- revalue(dados$psysubc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$psysubp <- revalue(dados$psysubp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$psynosc <- revalue(dados$psynosc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$psynosp <- revalue(dados$psynosp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$moodnosp <- revalue(dados$moodnosp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$deppsyc <- revalue(dados$deppsyc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$deppsyp <- revalue(dados$deppsyp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$bulpurgc <- revalue(dados$bulpurgc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$bulpurgp <- revalue(dados$bulpurgp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$bulnonpc <- revalue(dados$bulnonpc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$bulnonpp <- revalue(dados$bulnonpp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$anbingec <- revalue(dados$anbingec, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$anbingep <- revalue(dados$anbingep, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$anrestrc <- revalue(dados$anrestrc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$anrestrp <- revalue(dados$anrestrp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$genanxc <- revalue(dados$genanxc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$genanxp <- revalue(dados$genanxp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ganxgenc <- revalue(dados$ganxgenc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$ganxgenp <- revalue(dados$ganxgenp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$gadsubc <- revalue(dados$gadsubc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$gadsubp <- revalue(dados$gadsubp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$addkidc <- revalue(dados$addkidc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$addkidp <- revalue(dados$addkidp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$addadltc <- revalue(dados$addadltc, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$addadltp <- revalue(dados$addadltp, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depsleepcat <- revalue(dados$depsleepcat, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depsicat <- revalue(dados$depsicat, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$stepid <- revalue(dados$stepid, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$dayscons_ade <- revalue(dados$dayscons_ade, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$dayscons_weeks_ade <- revalue(dados$dayscons_weeks_ade, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$dayscons_mini <- revalue(dados$dayscons_mini, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$dayscons_weeks_mini <- revalue(dados$dayscons_weeks_mini, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$dayscons_x <- revalue(dados$dayscons_x, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$dayscons_weeks_x <- revalue(dados$dayscons_weeks_x, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$dayscons_y <- revalue(dados$dayscons_y, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$dayscons_weeks_y <- revalue(dados$dayscons_weeks_y, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$certcode_ade <- revalue(dados$certcode_ade, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$certcode_mini <- revalue(dados$certcode_mini, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$certcode <- revalue(dados$certcode, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$deprmd <- revalue(dados$deprmd, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depsleep <- revalue(dados$depsleep, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depinter <- revalue(dados$depinter, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depguilt <- revalue(dados$depguilt, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depenerg <- revalue(dados$depenerg, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depconcn <- revalue(dados$depconcn, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depappet <- revalue(dados$depappet, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$deppmr <- revalue(dados$deppmr, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depsi <- revalue(dados$depsi, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depsev <- revalue(dados$depsev, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depse <- revalue(dados$depse, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$depdist <- revalue(dados$depdist, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$deppma <- revalue(dados$deppma, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$elvselfe <- revalue(dados$elvselfe, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$elvsleep <- revalue(dados$elvsleep, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$elvtalk <- revalue(dados$elvtalk, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$elvfoi <- revalue(dados$elvfoi, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$elvdistr <- revalue(dados$elvdistr, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$elvgdact <- revalue(dados$elvgdact, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$elvhrb <- revalue(dados$elvhrb, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))
dados$elvpma <- revalue(dados$elvpma, c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))

#Casos especificos (que não possuem somente valores Yes, No, N/A, Unknown e Refused)

dados$deprmd <- revalue(dados$deprmd, c("Not on original form" = -4)) 

dados$depsicat <- revalue(dados$depsicat, c("Nearly every day, persistent" = 0, "Nearly every day, persistent, death thoughts" = 1,
						  "Nearly every day, persistent, LNWL" = 2, "Nearly every day, persistent, LNWL, death thoughts" = 3, 
						  "Nearly every day, persistent, suicidal thoughts, urges to self harm" = 4, "No suicide ideation" = 5, 
						  "Persistent, death thoughts" = 6, "Persistent, LNWL, death thoughts" = 7, "Persistent, LNWL, suicidal thoughts" = 8,
						  "Persistent, suicidal thoughts" = 9, "Rare fleeting" = 10, "Rare fleeting, death thoughts" = 11, 
						  "Rare fleeting, LNWL" = 12, "Rare fleeting, LNWL, death thoughts" = 13, "Rare fleeting, LNWL, suicidal thoughts" = 14,
						  "Several days, brief, LNWL, suicidal thoughts" = 15, "Several days, brief, suicidal thoughts" = 16, 
						  "Several days, fleeting" = 17, "Several days, fleeting, LNWL" = 18, "Several days, fleeting, LNWL, death thoughts" = 19,
						  "Several days, fleeting, LNWL, suicidal thoughts" = 20, "Several days, fleeting, suicidal thoughts" = 21,
						  "Several days, fleeting/persistent, LNWL, death/suicidal thoughts" = 22, "Several days, persistent, death thoughts" = 23,
						  "Several days, persistent/brief, LNWL, death/suicidal thoughts" = 24))
						  
dados$depsleepcat <- revalue(dados$depsleepcat, c("Any sleep decrease" = 0, "Any sleep increase" = 1, "Sleep decrease < 1 hr/d from normal" = 2,
							 "Sleep decrease >= 1 hr/d from normal" = 3, "Sleep decrease below 50% of normal" = 4, "Sleep decrease to 50% of normal" = 5,
							 "Sleep increase < 1 hr/d from normal" = 6, "Sleep increase >= 1 hr/d from normal" = 7, "Sleep increase 50% above normal" = 8,
							 "Sleep increase up to 50% of normal" = 9, "Sleep normal" = 10))

dados$dage1stcat <- revalue(dados$dage1stcat, c("[0,5)" = 1, "[10,15)" = 2, "[15,20)" = 3, "[20,25)" = 4, "[25,30)" = 5,
							"[30,35)" = 6, "[35,40)" = 7, "[40,45)" = 8, "[45,50)" = 9, "[5,10)" = 10, "[50,55)" = 11,
							"[55,60)" = 12, "[60,65)" = 13, "[65,70)" = 14, "[70,71]" = 15))
							
dados$mage1stcat <- revalue(dados$mage1stcat, c("[0,5)" = 1, "[10,15)" = 2, "[15,20)" = 3, "[20,25)" = 4, "[25,30)" = 5,
							"[30,35)" = 6, "[35,40)" = 7, "[40,45)" = 8, "[45,50)" = 9, "[5,10)" = 10, "[50,55)" = 11, 
							"[55,60)" = 12, "[60,65)" = 13, "[65,70)" = 14, "[70,73]" = 15))
							
dados$clinstat <- revalue(dados$clinstat, c("Continued SXs" = 1, "Depression" = 2, "Hypomania" = 3, "Mania" = 4, 
						  "Mixed/Cycling" = 5, "Recovered" = 6, "Recovering" = 7, "Roughening" = 8))
						  
dados$manlast <- revalue(dados$manlast, c("0" = 0, "1" = 1, "2" = 2, "[3,5)" = 3, "[5,10)" = 4, "[10,20)" = 5, "[20,50)" = 6, "[50,98]" = 7))

dados$deppast <- revalue(dados$deppast, c("0" = 0, "1" = 1, "2" = 2, "[3,5)" = 3, "[5,10)" = 4, "[10,20)" = 5, "[20,50]" = 6))
							 
write.csv(dados, file="baseNumerica.csv", row.names=FALSE, na="")

