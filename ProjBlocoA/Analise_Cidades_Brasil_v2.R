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
library(ggmap)
library(e1071)


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
dsCities <- read.csv("BRAZIL_CITIES_v4_UTF8.csv", sep=";", dec=".")
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





### Naive Bayes ###

# Remove colunas desnecessárias do dataset
ds_naivebayes <- subset( dsCities, select = c(IDHM.Ranking.2010,
                                              IDHM,
                                              IDHM_Renda,
                                              IDHM_Longevidade,
                                              IDHM_Educacao,
                                              #ESTIMATED_POP,
                                              GDP_CAPITA,
                                              HOTELS,
                                              #BEDS,
                                              #Pr_Agencies,
                                              #Pu_Agencies,
                                              Pr_Bank,
                                              Pu_Bank,
                                              #Cars,
                                              #Motorcycles,
                                              #POST_OFFICES,
                                              SUPORTE_4G) )


# Define o valor da semente
set.seed(12092019)

# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'SUPORTE_4G'
splitting = sample.split(ds_naivebayes$SUPORTE_4G, SplitRatio = 0.7)
ds_naivebayes_treinamento = subset(ds_naivebayes, splitting == TRUE)
ds_naivebayes_teste = subset(ds_naivebayes, splitting == FALSE)

str(ds_naivebayes_treinamento)
str(ds_naivebayes_teste)

# Classificação (x: dataframe de treinamento sem a coluna 'SUPORTE_4G' / y: coluna 'SUPORTE_4G' do dataframe de treinamento)
classifier <- naiveBayes(x = subset( ds_naivebayes_treinamento, select = -c(SUPORTE_4G) ), y = ds_naivebayes_treinamento$SUPORTE_4G )
print(classifier)

# Previsão
prediction <- predict(classifier, newdata = subset( ds_naivebayes_teste, select = -c(SUPORTE_4G) ))

# Tabela de previsões (coluna 'SUPORTE_4G' do dataframe de teste)
table_prediction <- table(ds_naivebayes_teste$SUPORTE_4G,prediction)
print(table_prediction)

# Matriz de confusão
confusionMatrix(table_prediction)

# >>> Acurácia: 70%
# *** Se incluir a populaçã no dataset, a acurácia do modelo cai para 64%





# Definindo cobertura 2G, 3G e 4G
# 2G: Possui cobertura somente 2G
# 3G: Possui cobertura 3G e não possui 4G
# 4G: Possui cobertura 4G
dsCities$COBERTURA <- ifelse(dsCities$SUPORTE_2G == "Sim" &
                             dsCities$SUPORTE_3G == "Não" &
                             dsCities$SUPORTE_4G == "Não", "2G",
                             ifelse(dsCities$SUPORTE_3G == "Sim" &
                                    dsCities$SUPORTE_4G == "Não", "3G",
                                    ifelse(dsCities$SUPORTE_4G == "Sim", "4G", "SEM COBERTURA")))


# API Key
register_google(key = "***************************************")


# Mapa do Brasi
map <- get_map(location = 'Brazil', zoom = 4, scale = 2)
#map <- get_map(location = 'Rio de Janeiro', zoom = 8)

ggmap(map)


# Mapa de Cobertura por IDH
mapCoberturaIDHM <- ggmap(map) +
  geom_point(data = dsCities, mapping = aes(x = dsCities$LONG, y = dsCities$LAT, 
                                            col = COBERTURA, size = IDHM))

mapCoberturaIDHM


# Mapa de Cobertura por População
mapCoberturaPop <- ggmap(map) +
  geom_point(data = dsCities, mapping = aes(x = dsCities$LONG, y = dsCities$LAT, 
                                            col = COBERTURA, size = IBGE_POP))

mapCoberturaPop


# Mapa de Cobertura 2G, 3G e 4G
mapCobertura <- ggmap(map) +
  geom_point(aes(x = dsCities$LONG, y = dsCities$LAT, color = COBERTURA), data = dsCities, alpha = 1, size = .5) +
  scale_color_manual(values=c("#0000FF", "#FF00FF", "#006600"))

mapCobertura


# Calculo do IDH médio por tecnologia

# Cobertura somente 2G
median(dsCities[dsCities$COBERTURA=="2G",]$IDHM)
#0.66

# Cobertura 3G e sem cobertura 4G
median(dsCities[dsCities$COBERTURA=="3G",]$IDHM)
#0.602

# Cobertura 4G
median(dsCities[dsCities$COBERTURA=="4G",]$IDHM)
#0.68

# Qualquer tipo de cobertura
median(dsCities[dsCities$COBERTURA %in% c("2G","3G","4G"),]$IDHM)
#0.665

# Cidades que possuem IDHM >= 0.68 e não possuem cobertura 4G
oportunidade4G_IDH <- dsCities[dsCities$IDHM >= 0.68 & dsCities$COBERTURA != "4G",]

View(oportunidade4G_IDH$CIDADE_UF)

# Mapa de Oportunidade 4G
mapOportunidade4G_IDH <- ggmap(map) +
  geom_point(aes(x = oportunidade4G_IDH$LONG, y = oportunidade4G_IDH$LAT), data = oportunidade4G_IDH, alpha = 1, size = .5, color = "#006600" )

mapOportunidade4G_IDH






# Calculo da População média por tecnologia

# Cobertura somente 2G
median(dsCities[dsCities$COBERTURA=="2G",]$ESTIMATED_POP)
#3404

# Cobertura 3G e sem cobertura 4G
median(dsCities[dsCities$COBERTURA=="3G",]$ESTIMATED_POP)
#6522

# Cobertura 4G
median(dsCities[dsCities$COBERTURA=="4G",]$ESTIMATED_POP)
#13976

# Qualquer tipo de cobertura
median(dsCities[dsCities$COBERTURA %in% c("2G","3G","4G"),]$ESTIMATED_POP)
#11591

# Cidades que possuem População >= 13976 e não possuem cobertura 4G
oportunidade4G_POP <- dsCities[dsCities$ESTIMATED_POP >= 13976 & dsCities$COBERTURA != "4G",]

View(oportunidade4G_POP$CIDADE_UF)

# Mapa de Oportunidade 4G
mapOportunidade4G_POP <- ggmap(map) +
  geom_point(aes(x = oportunidade4G_POP$LONG, y = oportunidade4G_POP$LAT), data = oportunidade4G_POP, alpha = 1, size = .5, color = "#006600" )

mapOportunidade4G_POP



