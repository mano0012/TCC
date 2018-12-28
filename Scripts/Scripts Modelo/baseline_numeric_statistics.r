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
workdir = "C:/Users/Suporte-CERCOMP/Documents/TCC/Scripts/ADE/BaseNumerica"
setwd(workdir)

filename <- "baseNumerica.csv"
dados <- read.csv(file=filename, sep=",", header=TRUE, strip.white=TRUE)

dados$stepid <- NULL

sink(file="baseline_numeric_statistics.txt")
result = describe(dados)
print(result)
sink()

set.seed(7)
correlationMatrix <- cor(dados, use = "complete.obs", method = c("pearson"))

sink(file="correlationMatrix.txt")
print(correlationMatrix)
sink()

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.95, verbose = FALSE, names = TRUE, exact = FALSE)
sink(file="highlyCorrelated.txt")
print(highlyCorrelated)
sink()