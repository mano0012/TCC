#Librarys
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(readr)
library(dplyr)
library(Hmisc)
library(caret)

# Define o Diretorio de trabalho
workdir = "C:/Users/Suporte-CERCOMP/Documents/Salvini/Scripts"
setwd(workdir)

#Salvar arquivo
salvar.arquivo <- function(df, nome) {
  write.csv(df, file=nome, row.names=FALSE, na="")
}

#Leitura da Base de Dados
filename <- "baseline.csv"
dados <- read.csv(file=filename, sep=",", header=TRUE, strip.white=TRUE)

attr <- select(dados, contains("stepid"), contains("dayscons"), contains("certcode"),
			   contains("majdepc"), contains("majdepp"), contains("moodgenc"), 
			   contains("moodgenp"), contains("moodsubc"), contains("moodsubp"),
			   contains("manicc"), contains("manicp"), contains("hmanicc"), 
			   contains("hmanicp"), contains("typeiic"), contains("typeiip"), 
			   contains("mangenc"), contains("mangenp"), contains("hypogenc"), 
			   contains("hypogenp"), contains("mansubc"), contains("mansubp"), 
			   contains("hyposubc"), contains("hyposubp"), contains("anxdisc"), 
			   contains("anxdisp"), contains("anxsubc"), contains("anxsubp"), 
			   contains("pdnoagc"), contains("pdnoagp"), contains("pdagorc"), 
			   contains("pdagorp"), contains("agorc"), contains("agorp"), 
			   contains("phobiac"), contains("phobiap"), contains("ocdc"), 
			   contains("ocdp"), contains("ocdgenc"), contains("ocdgenp"), 
			   contains("ocdsubc"), contains("ocdsubp"), contains("ptsdc"), 
			   contains("ptsdp"), contains("alcdepc"), contains("alcdepp"), 
			   contains("alcabusc"), contains("alcabusp"), contains("drugdepc"), 
			   contains("drugdepp"), contains("drugabc"), contains("drugabp"), 
			   contains("psychp"), contains("psychc"), contains("schizc"), 
			   contains("schizp"), contains("schaffc"), contains("schaffp"), 
			   contains("schphrc"), contains("schphrp"), contains("bpsychc"), 
			   contains("bpsychp"), contains("deldisc"), contains("deldisp"), 
			   contains("psygenc"), contains("psygenp"), contains("psysubc"), 
			   contains("psysubp"), contains("psynosc"), contains("psynosp"), 
			   contains("moodnosp"), contains("deppsyc"), contains("deppsyp"), 
			   contains("ipsychc"), contains("ipsychp"), contains("bulpurgc"), 
			   contains("bulpurgp"), contains("bulnonpc"), contains("bulnonpp"), 
			   contains("anbingec"), contains("anbingep"), contains("anrestrc"), 
			   contains("anrestrp"), contains("genanxc"), contains("genanxp"), 
			   contains("ganxgenc"), contains("ganxgenp"), contains("gadsubc"), 
			   contains("gadsubp"), contains("addkidc"), contains("addkidp"), 
			   contains("addadltc"), contains("addadltp"), contains("deprmd"), 
			   contains("depsleep"), contains("depinter"), contains("depguilt"), 
			   contains("depenerg"), contains("depconcn"), contains("depappet"), 
			   contains("deppmr"), contains("depsi"), contains("depse"), 
			   contains("depdist"), contains("deppma"), contains("elvselfe"), 
			   contains("elvsleep"), contains("elvtalk"), contains("elvfoi"), 
			   contains("elvdistr"), contains("elvgdact"), contains("elvhrb"), 
			   contains("elvpma"))

numericalBase <- select(attr, "stepid", "dayscons_ade", "dayscons_weeks_ade",
					    "dayscons_mini", "dayscons_weeks_mini", "dayscons_x",
						"dayscons_weeks_x", "dayscons_y", "dayscons_weeks_y",
						"certcode_ade", "certcode_mini", "certcode", "deprmd",
						"depsleep", "depinter", "depguilt", "depenerg",
						"depconcn", "depappet", "deppmr", "depsi", "depsev",
						"depse", "depdist", "deppma", "elvselfe", "elvsleep",
						"elvtalk", "elvfoi", "elvdistr", "elvgdact", "elvhrb",
						"elvpma")
						
categoricalBase <- select(attr, "majdepc", "majdepp", "moodgenc", "moodgenp",
						  "moodsubc", "moodsubp", "manicc", "hmanicc", "manicp",
						  "hmanicp", "typeiic", "typeiip", "mangenc", "mangenp",
						  "hypogenc", "hypogenp", "mansubc", "mansubp",
						  "hyposubc", "hyposubp", "anxdisc", "anxdisp", "anxsubc",
						  "anxsubp", "pdnoagc", "pdnoagp", "pdagorc", "pdagorp",
						  "agorc", "agorp", "phobiac", "phobiap", "ocdcur", "ocdc",
						  "ocdpast", "ocdp", "ocdgenc", "ocdgenp", "ocdsubc",
						  "ocdsubp", "ptsdcur", "ptsdc", "ptsdpast", "ptsdp",
						  "alcdepc", "alcdepp", "alcabusc", "alcabusp", "drugdepc",
						  "drugdepp", "drugabc", "drugabp", "psychp", "bpsychp",
						  "ipsychp", "psychc", "bpsychc", "ipsychc", "schizc", "schizp",
						  "schaffc", "schaffp", "schphrc", "schphrp", "deldisc",
						  "deldisp", "psygenc", "psygenp", "psysubc", "psysubp",
						  "psynosc", "psynosp", "moodnosp", "deppsyc", "deppsyp",
						  "bulpurgc", "bulpurgp", "bulnonpc", "bulnonpp", "anbingec",
						  "anbingep", "anrestrc", "anrestrp", "genanxc", "genanxp",
						  "ganxgenc", "ganxgenp", "gadsubc", "gadsubp", "addkidc",
						  "addkidp", "addadltc", "addadltp", "depsleepcat", "depsicat")
						  
salvar.arquivo(numericalBase, "NumericalBase.csv")
salvar.arquivo(categoricalBase, "CategoricalBase.csv")