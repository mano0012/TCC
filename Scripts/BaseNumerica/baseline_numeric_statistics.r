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
workdir = "C:/Users/Stick/Desktop/TCC/Scripts/BaseNumerica"
setwd(workdir)

filename <- "baseNumerica.csv"
base <- read.csv(file=filename, sep=",", header=TRUE, strip.white=TRUE)

sink(file="baseline_numeric_statistics.txt")
result = describe(base)
print(result)
sink()

set.seed(7)
correlationMatrix <- cor(base, use = "complete.obs", method = c("pearson", "kendall", "spearman"))

sink(file="correlationMatrix.txt")
print(correlationMatrix)
sink()

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.95, verbose = FALSE, names = TRUE, exact = FALSE)
sink(file="highlyCorrelated.txt")
print(highlyCorrelated)
sink()