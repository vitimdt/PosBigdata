setwd("D:/Pessoal/Projects/PosBigdata/ProjBlocoA")

#install.packages('ggplot2', dependencies=TRUE)
#install.packages('GGally', dependencies=TRUE)
#install.packages(c('ggthemes', 'reshape2'))
#install.packages('tsne')

library(ggplot2)
library(GGally)
library(dplyr)
library(reshape2)
library(sjlabelled)
library(tsne)

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
# Carregando dataset de Qualidade SMP ############################################
##################################################################################
dsQualidade <- read.csv("Qualidades_Dados_SMP_Filtrado.csv", sep=";", dec=",")

# Removendo colunas irrelevantes
dsQualidade <- dsQualidade[,c(-1, -4, -6, -7, -8, -9, -10, -11, -12, -13, -14)]

summary(dsQualidade)
str(dsQualidade)

#dsQualidade <- replace(dsQualidade, dsQualidade=="NO", NA)
#dsQualidade <- replace(dsQualidade, dsQualidade=="NI", NA)

dsQualidade$Media <- rowMeans(dsQualidade[,4:66], na.rm=TRUE)

dsQualidade$jan.16 <- ifelse(is.na(dsQualidade$jan.16), dsQualidade$Media, dsQualidade$jan.16)
dsQualidade$fev.16 <- ifelse(is.na(dsQualidade$fev.16), dsQualidade$Media, dsQualidade$fev.16)
dsQualidade$mar.16 <- ifelse(is.na(dsQualidade$mar.16), dsQualidade$Media, dsQualidade$mar.16)
dsQualidade$abr.16 <- ifelse(is.na(dsQualidade$abr.16), dsQualidade$Media, dsQualidade$abr.16)
dsQualidade$mai.16 <- ifelse(is.na(dsQualidade$mai.16), dsQualidade$Media, dsQualidade$mai.16)
dsQualidade$jun.16 <- ifelse(is.na(dsQualidade$jun.16), dsQualidade$Media, dsQualidade$jun.16)
dsQualidade$jul.16 <- ifelse(is.na(dsQualidade$jul.16), dsQualidade$Media, dsQualidade$jul.16)
dsQualidade$ago.16 <- ifelse(is.na(dsQualidade$ago.16), dsQualidade$Media, dsQualidade$ago.16)
dsQualidade$set.16 <- ifelse(is.na(dsQualidade$set.16), dsQualidade$Media, dsQualidade$set.16)
dsQualidade$out.16 <- ifelse(is.na(dsQualidade$out.16), dsQualidade$Media, dsQualidade$out.16)
dsQualidade$nov.16 <- ifelse(is.na(dsQualidade$nov.16), dsQualidade$Media, dsQualidade$nov.16)
dsQualidade$dez.16 <- ifelse(is.na(dsQualidade$dez.16), dsQualidade$Media, dsQualidade$dez.16)

dsQualidade$jan.17 <- ifelse(is.na(dsQualidade$jan.17), dsQualidade$Media, dsQualidade$jan.17)
dsQualidade$fev.17 <- ifelse(is.na(dsQualidade$fev.17), dsQualidade$Media, dsQualidade$fev.17)
dsQualidade$mar.17 <- ifelse(is.na(dsQualidade$mar.17), dsQualidade$Media, dsQualidade$mar.17)
dsQualidade$abr.17 <- ifelse(is.na(dsQualidade$abr.17), dsQualidade$Media, dsQualidade$abr.17)
dsQualidade$mai.17 <- ifelse(is.na(dsQualidade$mai.17), dsQualidade$Media, dsQualidade$mai.17)
dsQualidade$jun.17 <- ifelse(is.na(dsQualidade$jun.17), dsQualidade$Media, dsQualidade$jun.17)
dsQualidade$jul.17 <- ifelse(is.na(dsQualidade$jul.17), dsQualidade$Media, dsQualidade$jul.17)
dsQualidade$ago.17 <- ifelse(is.na(dsQualidade$ago.17), dsQualidade$Media, dsQualidade$ago.17)
dsQualidade$set.17 <- ifelse(is.na(dsQualidade$set.17), dsQualidade$Media, dsQualidade$set.17)
dsQualidade$out.17 <- ifelse(is.na(dsQualidade$out.17), dsQualidade$Media, dsQualidade$out.17)
dsQualidade$nov.17 <- ifelse(is.na(dsQualidade$nov.17), dsQualidade$Media, dsQualidade$nov.17)
dsQualidade$dez.17 <- ifelse(is.na(dsQualidade$dez.17), dsQualidade$Media, dsQualidade$dez.17)

dsQualidade$jan.18 <- ifelse(is.na(dsQualidade$jan.18), dsQualidade$Media, dsQualidade$jan.18)
dsQualidade$fev.18 <- ifelse(is.na(dsQualidade$fev.18), dsQualidade$Media, dsQualidade$fev.18)
dsQualidade$mar.18 <- ifelse(is.na(dsQualidade$mar.18), dsQualidade$Media, dsQualidade$mar.18)
summary(dsQualidade)



# Filtrando do dataset da Operadora Vivo com a média dos meses disponíveis dos tres indicadores (SMP8, SMP10, SMP11D)
dsQualidadeAgrupPorEmpresaeUF <- dsQualidade %>% 
  group_by(NomeFantasia, UF) %>% 
  summarise(
    media_2016 = mean((jan.16+fev.16+mar.16+abr.16+mai.16+jun.16+jul.16+ago.16+set.16+out.16+nov.16+dez.16)/12),
    media_2017 = mean((jan.17+fev.17+mar.17+abr.17+mai.17+jun.17+jul.17+ago.17+set.17+out.17+nov.17+dez.17)/12),
    media_2018 = mean((jan.18+fev.18+mar.18)/3))
head(dsQualidadeAgrupPorEmpresaeUF)

ggplot() +
  geom_bar(data = dsQualidadeAgrupPorEmpresaeUF,
           aes(x = UF, y = media_2018), 
           stat = "identity",
           fill = "orange"
  ) + facet_grid(NomeFantasia ~ .) + xlab("Estados") + ylab("2018") + ggtitle("Qualidade Serviço SMP")


##################################################################################
# Carregando dataset de Assinantes SMP ###########################################
##################################################################################
dsAssinantes <- read.csv("Assinantes/Assinantes_SMP_Empresa_UF_Anual.csv", sep=";", dec=",")

summary(dsAssinantes)
str(dsAssinantes)

dsAssinantes <- replace(dsAssinantes, is.na(dsAssinantes), 0)

dsAssinantesAgrupPorEmpresaeUF <- dsAssinantes %>% 
  group_by(NomeFantasia, UF) %>% 
  summarise(
    ass_media_2016 = sum((X01.01.2016 + X01.02.2016 + X01.03.2016 + X01.04.2016 + X01.05.2016 + X01.06.2016 +
                          X01.07.2016 + X01.08.2016 + X01.09.2016 + X01.10.2016 + X01.11.2016 + X01.12.2016)/12),
    ass_media_2017 = sum((X01.01.2017 + X01.02.2017 + X01.03.2017 + X01.04.2017 + X01.05.2017 + X01.06.2017 +
                          X01.07.2017 + X01.08.2017 + X01.09.2017 + X01.10.2017 + X01.11.2017 + X01.12.2017)/12),
    ass_media_2018 = sum((X01.01.2018 + X01.02.2018 + X01.03.2018)/3))
head(dsAssinantesAgrupPorEmpresaeUF)

ggplot() +
  geom_bar(data = dsAssinantesAgrupPorEmpresaeUF,
           aes(x = UF, y = ass_media_2018), 
           stat = "identity",
           fill = "orange"
  ) + facet_grid(NomeFantasia ~ .) + xlab("Estados") + ylab("2018") + ggtitle("Quantidade Assinantes SMP")

##################################################################################
# Carregando dataset de Reclamções SMP ###########################################
##################################################################################
dsReclamacoes <- read.csv("ReclamaçõesANATEL/Reclamacoes_SMP_Consolidado_Anual.csv", sep=";", dec=",")

summary(dsReclamacoes)
str(dsReclamacoes)

dsReclamacoes <- dsReclamacoes[dsReclamacoes$Tipo == 'Reclamação',]

dsReclamacoesAgrupado <- dsReclamacoes %>% 
  group_by(Ano, Mes, NomeFantasia, UF) %>% summarise(QtdeReclamacoes = sum(QtdeSolic))
summary(dsReclamacoesAgrupado)

dsReclamacoesAgrupadoPorMeses <- distinct(dsReclamacoes, NomeFantasia, UF)

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 1), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_01_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-3)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 2), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_02_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-4)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 3), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_03_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-5)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 4), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_04_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-6)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 5), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_05_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-7)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 6), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_06_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-8)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 7), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_07_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-9)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 8), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_08_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-10)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 9), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_09_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-11)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 10), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_10_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-12)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 11), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_11_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-13)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2016 & Mes == 12), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_12_2016 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-14)]


# Ano 2017
dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 1), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_01_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-15)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 2), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_02_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-16)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 3), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_03_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-17)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 4), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_04_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-18)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 5), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_05_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-19)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 6), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_06_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-20)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 7), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_07_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-21)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 8), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_08_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-22)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 9), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_09_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-23)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 10), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_10_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-24)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 11), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_11_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-25)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2017 & Mes == 12), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_12_2017 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-26)]


# Ano 2018
dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2018 & Mes == 1), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_01_2018 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-27)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2018 & Mes == 2), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_02_2018 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-28)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2018 & Mes == 3), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_03_2018 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-29)]

summary(dsReclamacoesAgrupadoPorMeses)

dsReclamacoesAgrupadoPorMeses <- replace(dsReclamacoesAgrupadoPorMeses, is.na(dsReclamacoesAgrupadoPorMeses), 0)

dsReclamacoesAnual <- dsReclamacoesAgrupadoPorMeses %>% 
  group_by(NomeFantasia, UF) %>% 
  summarise(
    recl_media_2016 = sum((Rec_01_2016 + Rec_02_2016 + Rec_03_2016 + Rec_04_2016 + Rec_05_2016 + Rec_06_2016 +
                             Rec_07_2016 + Rec_08_2016 + Rec_09_2016 + Rec_10_2016 + Rec_11_2016 + Rec_12_2016)/12),
    recl_media_2017 = sum((Rec_01_2017 + Rec_02_2017 + Rec_03_2017 + Rec_04_2017 + Rec_05_2017 + Rec_06_2017 +
                             Rec_07_2017 + Rec_08_2017 + Rec_09_2017 + Rec_10_2017 + Rec_11_2017 + Rec_12_2017)/12),
    recl_media_2018 = sum((Rec_01_2018 + Rec_02_2018 + Rec_03_2018)/3))
summary(dsReclamacoesAnual)

ggplot() +
  geom_bar(data = dsReclamacoesAnual,
           aes(x = UF, y = recl_media_2018), 
           stat = "identity",
           fill = "orange"
  ) + facet_grid(NomeFantasia ~ .) + xlab("Estados") + ylab("2018") + ggtitle("Reclamações ANATEL - SMP")

##################################################################################
# Juntando os Dataframes #########################################################
##################################################################################
dsQualidadeComReclamacoes <- merge(x=dsQualidadeAgrupPorEmpresaeUF, y=dsAssinantesAgrupPorEmpresaeUF,
                                   by=c('NomeFantasia','UF'), all.x = TRUE)
dsQualidadeComReclamacoes <- merge(x=dsQualidadeComReclamacoes, y=dsReclamacoesAgrupadoPorMeses,
                                   by=c('NomeFantasia','UF'), all.x = TRUE)
dsQualidadeComReclamacoes <- replace(dsQualidadeComReclamacoes, is.na(dsQualidadeComReclamacoes), 0)
summary(dsQualidadeComReclamacoes)

dsQualidadeComReclamacoes$PercRec_jan_2018 <- 
  ((with(dsQualidadeComReclamacoes, Rec_01_2018 * 100)) / dsQualidadeComReclamacoes$ass_jan_2018)
dsQualidadeComReclamacoes$PercRec_fev_2018 <- 
  ((with(dsQualidadeComReclamacoes, Rec_02_2018 * 100)) / dsQualidadeComReclamacoes$ass_fev_2018)
dsQualidadeComReclamacoes$PercRec_mar_2018 <- 
  ((with(dsQualidadeComReclamacoes, Rec_03_2018 * 100)) / dsQualidadeComReclamacoes$ass_mar_2018)

# Transformando os valores das variáveis Factor em valores númericos
#dsQualidadeComReclamacoes$NomeFantasia <- factor(dsQualidadeComReclamacoes$NomeFantasia, levels=c('Claro', 'Oi', 'Tim', 'Vivo'), labels=c(1,2,3,4))

#dsQualidadeComReclamacoes$UF <- factor(dsQualidadeComReclamacoes$UF, levels=c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG',
#                                         'MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO'),
#                                labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27))

# Transformando variáveis Factor em Character no Dataframe
#dsQualidadeComReclamacoes <- unfactorize(dsQualidadeComReclamacoes)

# Transformando todos os tipos das variáveis em Numeric
#types <- c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
#           "numeric","numeric","numeric")
#dsQualidadeComReclamacoes <- convert_types(dsQualidadeComReclamacoes, types)
str(dsQualidadeComReclamacoes)
summary(dsQualidadeComReclamacoes)


#dsQualidadeComReclamacoes <- dsQualidadeComReclamacoes[,c(-6,-7,-8)]
# Gerando o gráfico de correlação entre as variáveis
ggcorr(dsQualidadeComReclamacoes, palette = "RdBu", label = TRUE)


##################################################################################
# Aplicando algoritmo de redução e clusterização #################################
##################################################################################
# Reduzindo as variáveis em duas
tsne_reduction <- tsne(dsQualidadeComReclamacoes[,3:14], k = 2, perplexity = 30, epoch = 100)
resultados <- as.data.frame(tsne_reduction)
resultados$Empresa <- dsQualidadeComReclamacoes$NomeFantasia
str(resultados)
ggplot(data = resultados, aes(x = V1, y = V2)) +
  geom_point(aes(color = Empresa)) + 
  theme_bw()

# Executando o método do cotovelo com todas as variáveis para verificar um bom número de clusters
kmean_withinss <- function(k) {
  cluster <- kmeans(resultados[,-3], k)
  return (cluster$tot.withinss)
}
# Set maximum cluster 
max_k <-6
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

elbow <- data.frame(2:max_k, wss)

# Plot the graph with gglop
ggplot(elbow, aes(x=X2.max_k, y=wss)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1, 15, by = 1),labs(x='Número de Agrupamentos'))

# Executar o Kmeans de todas as variáveis com 4 clusters
km <- kmeans(resultados[,-3], 4, nstart=100)
# Visualizando.
plot(resultados[,-3], col=(km$cluster+1) ,
     main="Resultado do K-médias com 4 agrupamentos", pch=20, cex=2)

km$cluster <- as.factor(km$cluster)
ggplot(resultados, aes(x=V1, y=V2, color=km$cluster)) +
  geom_point()



##################################################################################
# Criando Dataframes com valores invertidos ######################################
##################################################################################
# Ajustando o Dataframe de Qualidades ############################################
dsQualidadeInvertidos <- melt(dsQualidadeAgrupPorEmpresaeUF,id=c("NomeFantasia","UF"))
dsQualidadeInvertidos$variable <- factor(dsQualidadeInvertidos$variable, levels=c('jan_18', 'fev_18', 'mar_18'), 
                                             labels=c('28-01-2018','28-02-2018','28-03-2018'))
summary(dsQualidadeInvertidos)
dsQualidadeInvertidos <- transform(dsQualidadeInvertidos,variable = as.character(variable))
# Definindo novo nome de cabeçalho para as variáveis
names(dsQualidadeInvertidos) <- c( "NomeFantasia", "UF", "Mes", "Qualidade")
str(dsQualidadeInvertidos)

# Criando o gráfico
#ggplot(dsQualidadeInvertidos[with(dsQualidadeInvertidos,NomeFantasia == 'Claro'),],
#       aes(x=variable,y=value,colour=UF,group=UF)) + 
#  geom_line() +
#  theme(axis.text.x = element_text(angle=90, hjust=1))


# Ajustando o Dataframe de Assinantes ############################################
dsAssinantesInvertidos <- melt(dsAssinantesAgrupPorEmpresaeUF,id=c("NomeFantasia","UF"))
dsAssinantesInvertidos$variable <- factor(dsAssinantesInvertidos$variable, levels=c('ass_jan_2018', 'ass_fev_2018', 'ass_mar_2018'), 
                                         labels=c('28-01-2018','28-02-2018','28-03-2018'))
summary(dsAssinantesInvertidos)
dsAssinantesInvertidos <- transform(dsAssinantesInvertidos,variable = as.character(variable))
# Definindo novo nome de cabeçalho para as variáveis
names(dsAssinantesInvertidos) <- c( "NomeFantasia", "UF", "Mes", "NumAssinantes")
str(dsAssinantesInvertidos)


# Ajustando o Dataframe de Reclamações ###########################################
dsReclamacoesInvertidos <- melt(dsReclamacoesAgrupadoPorMeses,id=c("NomeFantasia","UF"))
dsReclamacoesInvertidos$variable <- factor(dsReclamacoesInvertidos$variable, levels=c('Rec_01_2018', 'Rec_02_2018', 'Rec_03_2018'), 
                                          labels=c('28-01-2018','28-02-2018','28-03-2018'))
summary(dsReclamacoesInvertidos)
dsReclamacoesInvertidos <- transform(dsReclamacoesInvertidos,variable = as.character(variable))
# Definindo novo nome de cabeçalho para as variáveis
names(dsReclamacoesInvertidos) <- c( "NomeFantasia", "UF", "Mes", "NumReclamacoes")
str(dsReclamacoesInvertidos)


# Merge dos Dataframes em um novo Dataframe ######################################
dsIndicadoresPorEmprUFMes <- merge(x=dsQualidadeInvertidos, y=dsAssinantesInvertidos,
                                   by=c('NomeFantasia','UF','Mes'), all.x = TRUE)
dsIndicadoresPorEmprUFMes <- merge(x=dsIndicadoresPorEmprUFMes, y=dsReclamacoesInvertidos,
                                   by=c('NomeFantasia','UF','Mes'), all.x = TRUE)
dsIndicadoresPorEmprUFMes$PercReclamacoes <- 
  (with(dsIndicadoresPorEmprUFMes, (NumReclamacoes * 100) / NumAssinantes))
dsIndicadoresPorEmprUFMes$Mes <- as.POSIXct(dsIndicadoresPorEmprUFMes$Mes, format='%d-%m-%Y')
dsIndicadoresPorEmprUFMes$MesNum <- as.integer(dsIndicadoresPorEmprUFMes$Mes)
str(dsIndicadoresPorEmprUFMes)

summary(dsIndicadoresPorEmprUFMes[dsIndicadoresPorEmprUFMes$NomeFantasia == 'Vivo' &
                                    dsIndicadoresPorEmprUFMes$UF == 'RJ',])
ggplot(dsIndicadoresPorEmprUFMes[dsIndicadoresPorEmprUFMes$NomeFantasia == 'Vivo' &
                                   dsIndicadoresPorEmprUFMes$UF == 'RJ',], 
       aes(x=MesNum, y=Qualidade)) + geom_point()

