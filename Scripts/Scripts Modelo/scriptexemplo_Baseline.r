# Define o Diretorio de trabalho
#setwd("C:\Users\Suporte-CERCOMP\Documents\Salvini")

#Define Diretorio de Trabalho
workdir = "C:\Users\Suporte-CERCOMP\Documents\Salvini"
setwd(workdir)

#Funcao Salvar arquivo
salvar.arquivo <- function(df, nome) {
  write.csv(df, file=nome, row.names=FALSE, na="")
}

#Leitura da Base de Dados
library(readr)
filename <- "baseline.csv"
dados <- read.csv(file=filename, sep=",", header=TRUE, strip.white=TRUE)

#Cabeçalho com 6 primeiras linhas da Base de Dados
head(dados)

#Obtem as classes de cada variavel
names(dados)

#Imprime a Estrutura da Base 
str(dados)

#Decodifica as Classes dos Atributos que não sao do tipo integer/real/float/text
library(dplyr)
#OPGP: GENERAL_PRACTITIONER_FAMILY 
opgp.cod <- data.frame(opgp = c(1,0,-4,-5,-6,-7,-8), opgp_cat = c("Yes","No",NA,NA,NA,NA,NA))
dados <- left_join(dados, opgp.cod, by = "opgp")

#Analise Estatistica
library(Hmisc)
describe(dados) 
result = describe(dados)
sink(file="baseline_statistics.txt")
print(result)
sink()

#Analise Exploratoria
#Plot das Variaveis para Comparação
library(ggplot2)
library(ggfortify)
library(gridExtra)

#VISIT: STEP_VISIT 
plot1 <- ggplot(dados, aes(x=visit)) + geom_bar() + labs(title="STEP_VISIT UNCODED")
ggsave("lrift_visit.png", plot=plot1)
plot2 <- ggplot(dados, aes(x=visit_cat)) + geom_bar() + labs(title="STEP_VISIT DECODED")
ggsave("lrift_visit_cat.png", plot=plot2)
grid.arrange(plot1, plot2, ncol=2)

# Analise de Importancia de Atributos
library(dplyr)
base <- select(dados[,1:191], -pathway)

library(caret)
set.seed(7)
correlationMatrix <- cor(base, use = "complete.obs", method = c("pearson", "kendall", "spearman"))
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.95, verbose = FALSE, names = TRUE, exact = FALSE)
print(highlyCorrelated)

plothighlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.95)
plot(plothighlyCorrelated)

#Imprime a Base decodificada
salvar.arquivo(dados, "baseline1.csv")