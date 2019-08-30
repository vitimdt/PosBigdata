setwd("D:/Pessoal/Projects/PosBigdata/ProjBlocoA")

#install.packages('ggplot2', dependencies=TRUE)
#install.packages('GGally', dependencies=TRUE)
#install.packages(c('ggthemes', 'reshape2'))

library(ggplot2)
library(GGally)
library(dplyr)
library(reshape2)
library(sjlabelled)

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
#dsQualidade$mai.12 <- ifelse(is.na(dsQualidade$mai.12), dsQualidade$Media, dsQualidade$mai.12)
#dsQualidade$jun.12 <- ifelse(is.na(dsQualidade$jun.12), dsQualidade$Media, dsQualidade$jun.12)
#dsQualidade$jul.12 <- ifelse(is.na(dsQualidade$jul.12), dsQualidade$Media, dsQualidade$jul.12)
#dsQualidade$ago.12 <- ifelse(is.na(dsQualidade$ago.12), dsQualidade$Media, dsQualidade$ago.12)
#dsQualidade$set.12 <- ifelse(is.na(dsQualidade$set.12), dsQualidade$Media, dsQualidade$set.12)
#dsQualidade$out.12 <- ifelse(is.na(dsQualidade$out.12), dsQualidade$Media, dsQualidade$out.12)
#dsQualidade$nov.12 <- ifelse(is.na(dsQualidade$nov.12), dsQualidade$Media, dsQualidade$nov.12)
#dsQualidade$dez.12 <- ifelse(is.na(dsQualidade$dez.12), dsQualidade$Media, dsQualidade$dez.12)

#dsQualidade$jan.13 <- ifelse(is.na(dsQualidade$jan.13), dsQualidade$Media, dsQualidade$jan.13)
#dsQualidade$fev.13 <- ifelse(is.na(dsQualidade$fev.13), dsQualidade$Media, dsQualidade$fev.13)
#dsQualidade$mar.13 <- ifelse(is.na(dsQualidade$mar.13), dsQualidade$Media, dsQualidade$mar.13)
#dsQualidade$abr.13 <- ifelse(is.na(dsQualidade$abr.13), dsQualidade$Media, dsQualidade$abr.13)
#dsQualidade$mai.13 <- ifelse(is.na(dsQualidade$mai.13), dsQualidade$Media, dsQualidade$mai.13)
#dsQualidade$jun.13 <- ifelse(is.na(dsQualidade$jun.13), dsQualidade$Media, dsQualidade$jun.13)
#dsQualidade$jul.13 <- ifelse(is.na(dsQualidade$jul.13), dsQualidade$Media, dsQualidade$jul.13)
#dsQualidade$ago.13 <- ifelse(is.na(dsQualidade$ago.13), dsQualidade$Media, dsQualidade$ago.13)
#dsQualidade$set.13 <- ifelse(is.na(dsQualidade$set.13), dsQualidade$Media, dsQualidade$set.13)
#dsQualidade$out.13 <- ifelse(is.na(dsQualidade$out.13), dsQualidade$Media, dsQualidade$out.13)
#dsQualidade$nov.13 <- ifelse(is.na(dsQualidade$nov.13), dsQualidade$Media, dsQualidade$nov.13)
#dsQualidade$dez.13 <- ifelse(is.na(dsQualidade$dez.13), dsQualidade$Media, dsQualidade$dez.13)

#dsQualidade$jan.14 <- ifelse(is.na(dsQualidade$jan.14), dsQualidade$Media, dsQualidade$jan.14)
#dsQualidade$fev.14 <- ifelse(is.na(dsQualidade$fev.14), dsQualidade$Media, dsQualidade$fev.14)
#dsQualidade$mar.14 <- ifelse(is.na(dsQualidade$mar.14), dsQualidade$Media, dsQualidade$mar.14)
#dsQualidade$abr.14 <- ifelse(is.na(dsQualidade$abr.14), dsQualidade$Media, dsQualidade$abr.14)
#dsQualidade$mai.14 <- ifelse(is.na(dsQualidade$mai.14), dsQualidade$Media, dsQualidade$mai.14)
#dsQualidade$jun.14 <- ifelse(is.na(dsQualidade$jun.14), dsQualidade$Media, dsQualidade$jun.14)
#dsQualidade$jul.14 <- ifelse(is.na(dsQualidade$jul.14), dsQualidade$Media, dsQualidade$jul.14)
#dsQualidade$ago.14 <- ifelse(is.na(dsQualidade$ago.14), dsQualidade$Media, dsQualidade$ago.14)
#dsQualidade$set.14 <- ifelse(is.na(dsQualidade$set.14), dsQualidade$Media, dsQualidade$set.14)
#dsQualidade$out.14 <- ifelse(is.na(dsQualidade$out.14), dsQualidade$Media, dsQualidade$out.14)
#dsQualidade$nov.14 <- ifelse(is.na(dsQualidade$nov.14), dsQualidade$Media, dsQualidade$nov.14)
#dsQualidade$dez.14 <- ifelse(is.na(dsQualidade$dez.14), dsQualidade$Media, dsQualidade$dez.14)

#dsQualidade$jan.15 <- ifelse(is.na(dsQualidade$jan.15), dsQualidade$Media, dsQualidade$jan.15)
#dsQualidade$fev.15 <- ifelse(is.na(dsQualidade$fev.15), dsQualidade$Media, dsQualidade$fev.15)
#dsQualidade$mar.15 <- ifelse(is.na(dsQualidade$mar.15), dsQualidade$Media, dsQualidade$mar.15)
#dsQualidade$abr.15 <- ifelse(is.na(dsQualidade$abr.15), dsQualidade$Media, dsQualidade$abr.15)
#dsQualidade$mai.15 <- ifelse(is.na(dsQualidade$mai.15), dsQualidade$Media, dsQualidade$mai.15)
#dsQualidade$jun.15 <- ifelse(is.na(dsQualidade$jun.15), dsQualidade$Media, dsQualidade$jun.15)
#dsQualidade$jul.15 <- ifelse(is.na(dsQualidade$jul.15), dsQualidade$Media, dsQualidade$jul.15)
#dsQualidade$ago.15 <- ifelse(is.na(dsQualidade$ago.15), dsQualidade$Media, dsQualidade$ago.15)
#dsQualidade$set.15 <- ifelse(is.na(dsQualidade$set.15), dsQualidade$Media, dsQualidade$set.15)
#dsQualidade$out.15 <- ifelse(is.na(dsQualidade$out.15), dsQualidade$Media, dsQualidade$out.15)
#dsQualidade$nov.15 <- ifelse(is.na(dsQualidade$nov.15), dsQualidade$Media, dsQualidade$nov.15)
#dsQualidade$dez.15 <- ifelse(is.na(dsQualidade$dez.15), dsQualidade$Media, dsQualidade$dez.15)

#dsQualidade$jan.16 <- ifelse(is.na(dsQualidade$jan.16), dsQualidade$Media, dsQualidade$jan.16)
#dsQualidade$fev.16 <- ifelse(is.na(dsQualidade$fev.16), dsQualidade$Media, dsQualidade$fev.16)
#dsQualidade$mar.16 <- ifelse(is.na(dsQualidade$mar.16), dsQualidade$Media, dsQualidade$mar.16)
#dsQualidade$abr.16 <- ifelse(is.na(dsQualidade$abr.16), dsQualidade$Media, dsQualidade$abr.16)
#dsQualidade$mai.16 <- ifelse(is.na(dsQualidade$mai.16), dsQualidade$Media, dsQualidade$mai.16)
#dsQualidade$jun.16 <- ifelse(is.na(dsQualidade$jun.16), dsQualidade$Media, dsQualidade$jun.16)
#dsQualidade$jul.16 <- ifelse(is.na(dsQualidade$jul.16), dsQualidade$Media, dsQualidade$jul.16)
#dsQualidade$ago.16 <- ifelse(is.na(dsQualidade$ago.16), dsQualidade$Media, dsQualidade$ago.16)
#dsQualidade$set.16 <- ifelse(is.na(dsQualidade$set.16), dsQualidade$Media, dsQualidade$set.16)
#dsQualidade$out.16 <- ifelse(is.na(dsQualidade$out.16), dsQualidade$Media, dsQualidade$out.16)
#dsQualidade$nov.16 <- ifelse(is.na(dsQualidade$nov.16), dsQualidade$Media, dsQualidade$nov.16)
#dsQualidade$dez.16 <- ifelse(is.na(dsQualidade$dez.16), dsQualidade$Media, dsQualidade$dez.16)

#dsQualidade$jan.17 <- ifelse(is.na(dsQualidade$jan.17), dsQualidade$Media, dsQualidade$jan.17)
#dsQualidade$fev.17 <- ifelse(is.na(dsQualidade$fev.17), dsQualidade$Media, dsQualidade$fev.17)
#dsQualidade$mar.17 <- ifelse(is.na(dsQualidade$mar.17), dsQualidade$Media, dsQualidade$mar.17)
#dsQualidade$abr.17 <- ifelse(is.na(dsQualidade$abr.17), dsQualidade$Media, dsQualidade$abr.17)
#dsQualidade$mai.17 <- ifelse(is.na(dsQualidade$mai.17), dsQualidade$Media, dsQualidade$mai.17)
#dsQualidade$jun.17 <- ifelse(is.na(dsQualidade$jun.17), dsQualidade$Media, dsQualidade$jun.17)
#dsQualidade$jul.17 <- ifelse(is.na(dsQualidade$jul.17), dsQualidade$Media, dsQualidade$jul.17)
#dsQualidade$ago.17 <- ifelse(is.na(dsQualidade$ago.17), dsQualidade$Media, dsQualidade$ago.17)
#dsQualidade$set.17 <- ifelse(is.na(dsQualidade$set.17), dsQualidade$Media, dsQualidade$set.17)
#dsQualidade$out.17 <- ifelse(is.na(dsQualidade$out.17), dsQualidade$Media, dsQualidade$out.17)
#dsQualidade$nov.17 <- ifelse(is.na(dsQualidade$nov.17), dsQualidade$Media, dsQualidade$nov.17)
#dsQualidade$dez.17 <- ifelse(is.na(dsQualidade$dez.17), dsQualidade$Media, dsQualidade$dez.17)

dsQualidade$jan.18 <- ifelse(is.na(dsQualidade$jan.18), dsQualidade$Media, dsQualidade$jan.18)
dsQualidade$fev.18 <- ifelse(is.na(dsQualidade$fev.18), dsQualidade$Media, dsQualidade$fev.18)
dsQualidade$mar.18 <- ifelse(is.na(dsQualidade$mar.18), dsQualidade$Media, dsQualidade$mar.18)
summary(dsQualidade)

# Filtrando do dataset da Operadora Vivo com a média dos meses disponíveis dos tres indicadores (SMP8, SMP10, SMP11D)
dsQualidadeAgrupPorEmpresaeUF <- dsQualidade %>% 
  group_by(NomeFantasia, UF) %>% 
  summarise(
    #mai_12 = mean(mai.12), jun_12 = mean(jun.12),
    #jul_12 = mean(jul.12), ago_12 = mean(ago.12), set_12 = mean(set.12), out_12 = mean(out.12), nov_12 = mean(nov.12), dez_12 = mean(dez.12),
    #jan_13 = mean(jan.13), fev_13 = mean(fev.13), mar_13 = mean(mar.13), abr_13 = mean(abr.13), mai_13 = mean(mai.13), jun_13 = mean(jun.13),
    #jul_13 = mean(jul.13), ago_13 = mean(ago.13), set_13 = mean(set.13), out_13 = mean(out.13), nov_13 = mean(nov.13), dez_13 = mean(dez.13),
    #jan_14 = mean(jan.14), fev_14 = mean(fev.14), mar_14 = mean(mar.14), abr_14 = mean(abr.14), mai_14 = mean(mai.14), jun_14 = mean(jun.14),
    #jul_14 = mean(jul.14), ago_14 = mean(ago.14), set_14 = mean(set.14), out_14 = mean(out.14), nov_14 = mean(nov.14), dez_14 = mean(dez.14),
    #jan_15 = mean(jan.15), fev_15 = mean(fev.15), mar_15 = mean(mar.15), abr_15 = mean(abr.15), mai_15 = mean(mai.15), jun_15 = mean(jun.15),
    #jul_15 = mean(jul.15), ago_15 = mean(ago.15), set_15 = mean(set.15), out_15 = mean(out.15), nov_15 = mean(nov.15), dez_15 = mean(dez.15),
    #jan_16 = mean(jan.16), fev_16 = mean(fev.16), mar_16 = mean(mar.16), abr_16 = mean(abr.16), mai_16 = mean(mai.16), jun_16 = mean(jun.16),
    #jul_16 = mean(jul.16), ago_16 = mean(ago.16), set_16 = mean(set.16), out_16 = mean(out.16), nov_16 = mean(nov.16), dez_16 = mean(dez.16),
    #jan_17 = mean(jan.17), fev_17 = mean(fev.17), mar_17 = mean(mar.17), abr_17 = mean(abr.17), mai_17 = mean(mai.17), jun_17 = mean(jun.17),
    #jul_17 = mean(jul.17), ago_17 = mean(ago.17), set_17 = mean(set.17), out_17 = mean(out.17), nov_17 = mean(nov.17), dez_17 = mean(dez.17),
    jan_18 = mean(jan.18), fev_18 = mean(fev.18), mar_18 = mean(mar.18))
head(dsQualidadeAgrupPorEmpresaeUF)

ggplot() +
  geom_bar(data = dsQualidadeAgrupPorEmpresaeUF[with(dsQualidadeAgrupPorEmpresaeUF,NomeFantasia == 'Oi'),],
           aes(x = UF, y = mar_18), 
           stat = "identity",
           fill = "orange"
  ) + xlab("Estados") + ylab("Mes - Mar/2018") + ggtitle("Qualidade Serviço SMP")

# Ajustando os dados para gráfico
dsQualidadeInvertidos <- melt(dsQualidadeAgrupPorEmpresaeUF,id=c("NomeFantasia","UF"))
head(dsQualidadeInvertidos)

# Criando o gráfico
ggplot(dsQualidadeInvertidos[with(dsQualidadeInvertidos,NomeFantasia == 'Oi'),],
       aes(x=variable,y=value,colour=UF,group=UF)) + 
  geom_line() +
  theme(axis.text.x = element_text(angle=90, hjust=1))


# Ajustando as variáveis categoricas
#dsQualidade$NomeFantasia <- factor(dsQualidade$NomeFantasia, levels=c('Claro', 'Oi', 'Tim', 'Vivo'), labels=c(0,1,2,3))
#
#dsQualidade$UF <- factor(dsQualidade$UF, levels=c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG',
#                                         'MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO'),
#                                labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26))
#dsQualidade$Indicador <- factor(dsQualidade$Indicador, levels=c('SMP8','SMP10','SMP11D'), labels=c(0,1,2))


##################################################################################
# Carregando dataset de Assinantes SMP ###########################################
##################################################################################
dsAssinantes <- read.csv("Assinantes/Assinantes_SMP_Empresa_UF.csv", sep=";", dec=",")

summary(dsAssinantes)
str(dsAssinantes)

dsAssinantes <- replace(dsAssinantes, is.na(dsAssinantes), 0)

dsAssinantesAgrupPorEmpresaeUF <- dsAssinantes %>% 
  group_by(NomeFantasia, UF) %>% 
  summarise(ass_jan_2018 = sum(X01.01.2018), ass_fev_2018 = sum(X01.02.2018), ass_mar_2018 = sum(X01.03.2018))
head(dsAssinantesAgrupPorEmpresaeUF)


##################################################################################
# Carregando dataset de Reclamções SMP ###########################################
##################################################################################
dsReclamacoes <- read.csv("ReclamaçõesANATEL/Reclamacoes_SMP_Consolidado_2018.csv", sep=";", dec=",")

summary(dsReclamacoes)
str(dsReclamacoes)

dsReclamacoes <- dsReclamacoes[dsReclamacoes$Tipo == 'Reclamação',]

dsReclamacoesAgrupado <- dsReclamacoes %>% 
  group_by(Ano, Mes, NomeFantasia, UF) %>% summarise(QtdeReclamacoes = sum(QtdeSolic))
summary(dsReclamacoesAgrupado)

dsReclamacoesAgrupadoPorMeses <- distinct(dsReclamacoes, NomeFantasia, UF)

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2018 & Mes == 1), c(3,4,5)]
summary(dsFiltrado)
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_01_2018 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-3)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2018 & Mes == 2), c(3,4,5)]
summary(dsFiltrado)
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_02_2018 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-4)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2018 & Mes == 3), c(3,4,5)]
summary(dsFiltrado)
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_03_2018 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-5)]

summary(dsReclamacoesAgrupadoPorMeses)

dsReclamacoesAgrupadoPorMeses <- replace(dsReclamacoesAgrupadoPorMeses, is.na(dsReclamacoesAgrupadoPorMeses), 0)



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

# Gerando o gráfico de correlação entre as variáveis
ggcorr(dsQualidadeComReclamacoes, palette = "RdBu", label = TRUE)

