setwd("D:/Pessoal/Projects/PosBigdata/ProjBlocoA")

#install.packages('ggplot2', dependencies=TRUE)
#install.packages('GGally', dependencies=TRUE)
#install.packages('caret', dependencies = TRUE)
#install.packages('party', dependencies=TRUE)
#install.packages('rpart', dependencies=TRUE)
#install.packages('rpart.plot', dependencies=TRUE)

library(ggplot2)
library(GGally)
library(dplyr)
library(ggthemes)
library(grid)
library(caTools)
library(caret)
library(party)
library(rpart)
library(rpart.plot)


##################################################################################
# Funções Úteis ##################################################################
##################################################################################
# Função para converter as variáveis Factor em valores Char
unfactorize <- function(df){
  for(i in which(sapply(df, class) == "factor")) df[[i]] = as.character(df[[i]])
  return(df)
}

# Função para converter tipos de variáveis
convert_types <- function(obj,types){
  out <- lapply(1:length(obj),FUN = function(i){FUN1 <- 
    switch(types[i],
           character = as.character,
           numeric = as.numeric,
           factor = as.factor); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  as.data.frame(out)
}


##################################################################################
# Carregando dataset de Cidades x IDH ############################################
##################################################################################
dsCities <- read.csv("BRAZIL_CITIES_v2.csv", sep=";", dec=".")
summary(dsCities)
str(dsCities)

dsCities <- replace(dsCities, is.na(dsCities), 0)

# Definindo as cidades que possuem tecnologia 2G de Dados Móveis
dsCities$SUPORTE_2G <- ifelse(dsCities$CLARO_2G == 1 | dsCities$NEXTEL_2G == 1 |
                                dsCities$OI_2G == 1 | dsCities$TIM_2G == 1 |
                                dsCities$VIVO_2G == 1, 1, 0)
# Definindo as cidades que possuem tecnologia 3G de Dados Móveis
dsCities$SUPORTE_3G <- ifelse(dsCities$CLARO_3G == 1 | dsCities$NEXTEL_3G == 1 |
                                dsCities$OI_3G == 1 | dsCities$TIM_3G == 1 |
                                dsCities$VIVO_3G == 1, 1, 0)
# Definindo as cidades que possuem tecnologia 4G de Dados Móveis
dsCities$SUPORTE_4G <- ifelse(dsCities$CLARO_4G == 1 | dsCities$NEXTEL_4G == 1 |
                                dsCities$OI_4G == 1 | dsCities$TIM_4G == 1 |
                                dsCities$VIVO_4G == 1, 1, 0)
dsCities$SUPORTE_2G <- factor(dsCities$SUPORTE_2G, labels=c('Não', 'Sim'), levels = c(0,1))
dsCities$SUPORTE_3G <- factor(dsCities$SUPORTE_3G, labels=c('Não', 'Sim'), levels = c(0,1))
dsCities$SUPORTE_4G <- factor(dsCities$SUPORTE_4G, labels=c('Não', 'Sim'), levels = c(0,1))
summary(dsCities)

# Histograma de Cidades com suporte a 2G por estado
ggplot(dsCities, aes(x = STATE, fill=SUPORTE_2G)) +
  geom_histogram(stat="count", color ='black', alpha=.7) +
  xlab("Cidades por UF") + ylab("Frequência") + 
  ggtitle("Histograma de Cidades com suporte a 2G por estado") +
  theme_gray()

# Histograma de Cidades com suporte a 3G por estado
ggplot(dsCities, aes(x = STATE, fill=SUPORTE_3G)) +
  geom_histogram(stat="count", color ='black', alpha=.7) +
  xlab("Cidades por UF") + ylab("Frequência") + 
  ggtitle("Histograma de Cidades com suporte a 3G por estado") +
  theme_gray()

# Histograma de Cidades com suporte a 4G por estado
ggplot(dsCities, aes(x = STATE, fill=SUPORTE_4G)) +
  geom_histogram(stat="count", color ='black', alpha=.7) +
  xlab("Cidades por UF") + ylab("Frequência") + 
  ggtitle("Histograma de Cidades com suporte a 4G por estado") +
  theme_gray()


# Definindo qual tecnologia de dados móveis por Operadora (Operadora Oi)
dsCities$SUPORTE_OI <- ifelse(dsCities$OI_2G == 1 & dsCities$OI_3G == 0 & dsCities$OI_4G == 0, 2,
                              ifelse(dsCities$OI_2G == 1 & dsCities$OI_3G == 1 & dsCities$OI_4G == 0, 3,
                                     ifelse(dsCities$OI_2G == 1 & dsCities$OI_3G == 1 & dsCities$OI_4G == 1, 4, 0)))
# Definindo qual tecnologia de dados móveis por Operadora (Operadora NEXTEL)
dsCities$SUPORTE_NEXTEL <- ifelse(dsCities$NEXTEL_2G == 1 & dsCities$NEXTEL_3G == 0 & dsCities$NEXTEL_4G == 0, 2,
                                ifelse(dsCities$NEXTEL_2G == 1 & dsCities$NEXTEL_3G == 1 & dsCities$NEXTEL_4G == 0, 3,
                                     ifelse(dsCities$NEXTEL_2G == 1 & dsCities$NEXTEL_3G == 1 & dsCities$NEXTEL_4G == 1, 4, 0)))
# Definindo qual tecnologia de dados móveis por Operadora (Operadora Vivo)
dsCities$SUPORTE_VIVO <- ifelse(dsCities$VIVO_2G == 1 & dsCities$VIVO_3G == 0 & dsCities$VIVO_4G == 0, 2,
                                  ifelse(dsCities$VIVO_2G == 1 & dsCities$VIVO_3G == 1 & dsCities$VIVO_4G == 0, 3,
                                         ifelse(dsCities$VIVO_2G == 1 & dsCities$VIVO_3G == 1 & dsCities$VIVO_4G == 1, 4, 0)))
# Definindo qual tecnologia de dados móveis por Operadora (Operadora Tim)
dsCities$SUPORTE_TIM <- ifelse(dsCities$TIM_2G == 1 & dsCities$TIM_3G == 0 & dsCities$TIM_4G == 0, 2,
                                ifelse(dsCities$TIM_2G == 1 & dsCities$TIM_3G == 1 & dsCities$TIM_4G == 0, 3,
                                       ifelse(dsCities$TIM_2G == 1 & dsCities$TIM_3G == 1 & dsCities$TIM_4G == 1, 4, 0)))
# Definindo qual tecnologia de dados móveis por Operadora (Operadora Tim)
dsCities$SUPORTE_CLARO <- ifelse(dsCities$CLARO_2G == 1 & dsCities$CLARO_3G == 0 & dsCities$CLARO_4G == 0, 2,
                               ifelse(dsCities$CLARO_2G == 1 & dsCities$CLARO_3G == 1 & dsCities$CLARO_4G == 0, 3,
                                      ifelse(dsCities$CLARO_2G == 1 & dsCities$CLARO_3G == 1 & dsCities$CLARO_4G == 1, 4, 0)))
dsCities$SUPORTE_OI <- factor(dsCities$SUPORTE_OI, labels=c('SEM COBERTURA','2G','3G','4G'), levels = c(0,2,3,4))
dsCities$SUPORTE_NEXTEL <- factor(dsCities$SUPORTE_NEXTEL, labels=c('SEM COBERTURA','2G','3G','4G'), levels = c(0,2,3,4))
dsCities$SUPORTE_VIVO <- factor(dsCities$SUPORTE_VIVO, labels=c('SEM COBERTURA','2G','3G','4G'), levels = c(0,2,3,4))
dsCities$SUPORTE_TIM <- factor(dsCities$SUPORTE_TIM, labels=c('SEM COBERTURA','2G','3G','4G'), levels = c(0,2,3,4))
dsCities$SUPORTE_CLARO <- factor(dsCities$SUPORTE_CLARO, labels=c('SEM COBERTURA','2G','3G','4G'), levels = c(0,2,3,4))
dsCities <- dsCities[,c(-83,-84,-85,-86,-87,-88,-89,-90,-91,-92,-93,-94,-95,-96,-97)]
summary(dsCities)

# Boxplot do Suporte de dados móvel da OI por IDHM
ggplot(data=dsCities, aes(x=SUPORTE_OI, y=IDHM)) +
  geom_boxplot(aes(fill=SUPORTE_OI)) + 
  xlab("Suporte Dados Móvel OI") + ylab("IDHM") + 
  ggtitle("Boxplot do Suporte de dados móvel da OI por IDHM") +
  theme_solarized()

# Boxplot do Suporte de dados móvel da OI por IDHM Renda
ggplot(data=dsCities, aes(x=SUPORTE_OI, y=IDHM_Renda)) +
  geom_boxplot(aes(fill=SUPORTE_OI)) + 
  xlab("Suporte Dados Móvel OI") + ylab("IDHM Renda") + 
  ggtitle("Boxplot do Suporte de dados móvel da OI por IDHM Renda") +
  theme_solarized()

# Histograma de Cidades com suporte da OI por estado
ggplot(dsCities, aes(x = STATE, fill=SUPORTE_OI)) +
  geom_histogram(stat="count", color ='black', alpha=.7) +
  xlab("Cidades por UF") + ylab("Frequência") + 
  ggtitle("Histograma de Cidades com suporte de dados móveis da OI por estado") +
  theme_gray()


# Boxplot do Suporte de dados móvel da TIM por IDHM
ggplot(data=dsCities, aes(x=SUPORTE_TIM, y=IDHM)) +
  geom_boxplot(aes(fill=SUPORTE_TIM)) + 
  xlab("Suporte Dados Móvel TIM") + ylab("IDHM") + 
  ggtitle("Boxplot do Suporte de dados móvel da TIM por IDHM") +
  theme_solarized()

# Boxplot do Suporte de dados móvel da TIM por IDHM Renda
ggplot(data=dsCities, aes(x=SUPORTE_TIM, y=IDHM_Renda)) +
  geom_boxplot(aes(fill=SUPORTE_TIM)) + 
  xlab("Suporte Dados Móvel TIM") + ylab("IDHM Renda") + 
  ggtitle("Boxplot do Suporte de dados móvel da TIM por IDHM Renda") +
  theme_solarized()

# Histograma de Cidades com suporte da TIM por estado
ggplot(dsCities, aes(x = STATE, fill=SUPORTE_TIM)) +
  geom_histogram(stat="count", color ='black', alpha=.7) +
  xlab("Cidades por UF") + ylab("Frequência") + 
  ggtitle("Histograma de Cidades com suporte de dados móveis da TIM por estado") +
  theme_gray()


# Boxplot do Suporte de dados móvel da VIVO por IDHM
ggplot(data=dsCities, aes(x=SUPORTE_VIVO, y=IDHM)) +
  geom_boxplot(aes(fill=SUPORTE_VIVO)) + 
  xlab("Suporte Dados Móvel VIVO") + ylab("IDHM") + 
  ggtitle("Boxplot do Suporte de dados móvel da VIVO por IDHM") +
  theme_solarized()

# Boxplot do Suporte de dados móvel da VIVO por IDHM Renda
ggplot(data=dsCities, aes(x=SUPORTE_VIVO, y=IDHM_Renda)) +
  geom_boxplot(aes(fill=SUPORTE_VIVO)) + 
  xlab("Suporte Dados Móvel VIVO") + ylab("IDHM Renda") + 
  ggtitle("Boxplot do Suporte de dados móvel da VIVO por IDHM Renda") +
  theme_solarized()

# Histograma de Cidades com suporte da VIVO por estado
ggplot(dsCities, aes(x = STATE, fill=SUPORTE_VIVO)) +
  geom_histogram(stat="count", color ='black', alpha=.7) +
  xlab("Cidades por UF") + ylab("Frequência") + 
  ggtitle("Histograma de Cidades com suporte de dados móveis da VIVO por estado") +
  theme_gray()


# Boxplot do Suporte de dados móvel da Claro por IDHM
ggplot(data=dsCities, aes(x=SUPORTE_CLARO, y=IDHM)) +
  geom_boxplot(aes(fill=SUPORTE_CLARO)) + 
  xlab("Suporte Dados Móvel CLARO") + ylab("IDHM") + 
  ggtitle("Boxplot do Suporte de dados móvel da CLARO por IDHM") +
  theme_solarized()

# Boxplot do Suporte de dados móvel da CLARO por IDHM Renda
ggplot(data=dsCities, aes(x=SUPORTE_CLARO, y=IDHM_Renda)) +
  geom_boxplot(aes(fill=SUPORTE_CLARO)) + 
  xlab("Suporte Dados Móvel CLARO") + ylab("IDHM Renda") + 
  ggtitle("Boxplot do Suporte de dados móvel da CLARO por IDHM Renda") +
  theme_solarized()

# Histograma de Cidades com suporte da CLARO por estado
ggplot(dsCities, aes(x = STATE, fill=SUPORTE_CLARO)) +
  geom_histogram(stat="count", color ='black', alpha=.7) +
  xlab("Cidades por UF") + ylab("Frequência") + 
  ggtitle("Histograma de Cidades com suporte de dados móveis da CLARO por estado") +
  theme_gray()


# Gerando o gráfico de correlação entre as variáveis
ggcorr(dsCities[,c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)], palette = "RdBu", label = TRUE)

# Gerando o gráfico de correlação entre as variáveis
ggcorr(dsCities[,c(20,21,22,23,24,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45)], palette = "RdBu", label = TRUE)

# Gerando o gráfico de correlação entre as variáveis
ggcorr(dsCities[,c(20,21,22,23,24,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66)], palette = "RdBu", label = TRUE)

dsCitiesNew <- dsCities[,c(-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,
                           -46,-47,-48,-49,-50,-51,-52,-53,-54,-55,-56,-57,-58,-59,-60,-61,-62,-63,-64,-65,-66)]

str(dsCitiesNew)


### Árvore de Decisão ###

# Divide o dataframe em treinamento e teste, com taxa de 80% na coluna 'SUPORTE_OI'
dfCitiesSuporte_OI <- dsCitiesNew[,c(-1,-2,-3,-4,-10,-11,-12,-13,-14,-15,-16,-17,-19,-20,-21,-22,-23,-24,-25,-26,
                                     -27,-29,-30,-31,-38,-39,-42,-43,-44,-45,-47,-48,-49,-51,-52,-53,-54)]
str(dfCitiesSuporte_OI)
splitting = sample.split(dfCitiesSuporte_OI$SUPORTE_OI, SplitRatio = 0.8)
df_arvore_treinamento= subset(dfCitiesSuporte_OI, splitting == TRUE)
df_arvore_teste = subset(dfCitiesSuporte_OI, splitting == FALSE)

# Classificação
classifier <- rpart(formula = SUPORTE_OI ~ ., data = df_arvore_treinamento)
print(classifier)
rpart.plot(classifier)

# Previsão
prediction <- predict(classifier, newdata = df_arvore_teste[,-17], type = 'class')
print(prediction)

# Matriz de confusão
table_prediction <- table(df_arvore_teste[,17],prediction)
print(table_prediction)
confusionMatrix(table_prediction)
# Acurácia: 61%