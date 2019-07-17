# Tarefa 2 (Avaliação Algoritmos) - Análise de um dataset de sessões de compras pela internet

# -------------------------
# Instalação de Pacotes Externos
# -------------------------
#install.packages('ggplot2', dependencies=TRUE)
#install.packages('GGally', dependencies=TRUE)

# -------------------------
# Carregando as LIBs
# -------------------------
library(ggplot2)
library(GGally)

# Setando diretório onde estão os arquivos
setwd("D:\\Pessoal\\Projects\\PosBigdata\\Analytics_R\\Tarefa_2-AAlgoritmos")
# Carregando dataset
dsIntencaoCompras <- read.csv("online_shoppers_intention.csv", sep=",", dec=".")

summary(dsIntencaoCompras)

# Transformando as variáveis não numéricas e numéricas
# Variáveis booleanas serão transformadas em 0 (FALSE) e 1 (TRUE)
dsIntencaoCompras$numRevenue <- 0
dsIntencaoCompras[dsIntencaoCompras$Revenue == FALSE,]$numRevenue <- 0
dsIntencaoCompras[dsIntencaoCompras$Revenue == TRUE,]$numRevenue <- 1

dsIntencaoCompras$numWeekend <- 0
dsIntencaoCompras[dsIntencaoCompras$Weekend == FALSE,]$numWeekend <- 0
dsIntencaoCompras[dsIntencaoCompras$Weekend == TRUE,]$numWeekend <- 1

# Transformando a variável "VisitorType" com os seguintes valores:
# New_Visitor = 0
# Returning_Visitor = 1
# Other = 2
dsIntencaoCompras$numVisitorType <- 0
dsIntencaoCompras[dsIntencaoCompras$VisitorType == 'New_Visitor',]$numVisitorType <- 0
dsIntencaoCompras[dsIntencaoCompras$VisitorType == 'Returning_Visitor',]$numVisitorType <- 1
dsIntencaoCompras[dsIntencaoCompras$VisitorType == 'Other',]$numVisitorType <- 2

# Transformando a variável "Month" com os valores numéricos:
dsIntencaoCompras$numMonth <- 0
dsIntencaoCompras[dsIntencaoCompras$Month == 'Feb',]$numMonth <- 2
dsIntencaoCompras[dsIntencaoCompras$Month == 'Mar',]$numMonth <- 3
dsIntencaoCompras[dsIntencaoCompras$Month == 'May',]$numMonth <- 5
dsIntencaoCompras[dsIntencaoCompras$Month == 'June',]$numMonth <- 6
dsIntencaoCompras[dsIntencaoCompras$Month == 'Jul',]$numMonth <- 7
dsIntencaoCompras[dsIntencaoCompras$Month == 'Aug',]$numMonth <- 8
dsIntencaoCompras[dsIntencaoCompras$Month == 'Sep',]$numMonth <- 9
dsIntencaoCompras[dsIntencaoCompras$Month == 'Oct',]$numMonth <- 10
dsIntencaoCompras[dsIntencaoCompras$Month == 'Nov',]$numMonth <- 11
dsIntencaoCompras[dsIntencaoCompras$Month == 'Dec',]$numMonth <- 12


# Histograma de Revenue pela variável Weekend
ggplot(dsIntencaoCompras, aes(numRevenue)) + 
  geom_histogram(fill='blue', color ='black', binwidth = 0.1) + 
  xlab("Revenue (0=False e 1=True)") + ylab("Frequência") + ggtitle("Histograma de Revenue por Weekend") +
  facet_wrap(~Weekend)

# Histograma de Revenue pela variável VisitorType
ggplot(dsIntencaoCompras, aes(numRevenue)) + 
  geom_histogram(fill='blue', color ='black', binwidth = 0.1) + 
  xlab("Revenue (0=False e 1=True)") + ylab("Frequência") + ggtitle("Histograma de Revenue por VisitorType") +
  facet_wrap(~VisitorType)

# Histograma de SpecialDay pela variável Revenue
ggplot(dsIntencaoCompras, aes(SpecialDay)) + 
  geom_histogram(fill='blue', color ='black', binwidth = 0.1) + 
  xlab("SpecialDay") + ylab("Frequência") + ggtitle("Histograma de SpecialDay por Revenue") +
  facet_wrap(~Revenue)

# Histograma da variável numérica Month pela variável Revenue
ggplot(dsIntencaoCompras, aes(numMonth)) + 
  geom_histogram(fill='blue', color ='black', binwidth = 0.5) + 
  xlab("Month") + ylab("Frequência") + ggtitle("Histograma de Month por Revenue") +
  facet_wrap(~Revenue)

# PairsPlot das outras variáveis numéricas, para verificar se existe correlação entre elas
ggpairs(dsIntencaoCompras, columns = 1:9, 
        ggplot2::aes(colour=Revenue), title = "Intenção de Campras Online")

# Testando as variáveis "OperatingSystems", "Browser" e "Region" com as outras variáveis
#ggpairs(dsIntencaoCompras, columns = c(1,2,3,4,5,6,7,8,12), 
#       ggplot2::aes(colour=Revenue), title = "Intenção de Campras Online")
#
#ggpairs(dsIntencaoCompras, columns = c(1,2,3,4,5,6,7,8,13), 
#       ggplot2::aes(colour=Revenue), title = "Intenção de Campras Online")
# 
#ggpairs(dsIntencaoCompras, columns = c(1,2,3,4,5,6,7,8,14), 
#       ggplot2::aes(colour=Revenue), title = "Intenção de Campras Online")

# Como as variáveis "PageValues", "OperatingSystems", "Browser" e "Region" não possuem boa correlação
# com as outras variáveis para definirmos o modelo, vamos gerar um novo dataset com apenas os valores numéricos
# e apenas com variáveis relevantes
dsNewIntencaoCompras = dsIntencaoCompras[,c(1,2,3,4,5,6,7,8,10,20,21,22,19)]
head(dsNewIntencaoCompras)

