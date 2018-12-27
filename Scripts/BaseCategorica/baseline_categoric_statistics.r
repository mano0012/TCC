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
workdir = "C:/Users/Stick/Desktop/TCC/Scripts/BaseCategorica"
setwd(workdir)

filename <- "baseCategorica.csv"
base <- read.csv(file=filename, sep=",", header=TRUE, strip.white=TRUE)

sink(file="baseline_categoric_statistics.txt")
result = describe(base)
print(result)
sink()