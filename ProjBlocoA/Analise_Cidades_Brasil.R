setwd("D:/Pessoal/Projects/PosBigdata/ProjBlocoA")

#install.packages('ggplot2', dependencies=TRUE)
#install.packages('GGally', dependencies=TRUE)

library(ggplot2)
library(GGally)
library(dplyr)


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
