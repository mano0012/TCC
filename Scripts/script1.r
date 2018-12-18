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
filename <- "preBase.csv"
dados <- read.csv(file=filename, sep=",", header=TRUE, strip.white=TRUE)


#Organizar Yes = 1, No = 0, N/A = -1, Unknown = -2
#k <- revalue(dados$coluna, c("No"=0, "Yes"=1, "N/A" = -1, "Unknown" = -2))

