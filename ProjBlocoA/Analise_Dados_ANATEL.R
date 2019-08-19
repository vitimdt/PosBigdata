setwd("D:/Pessoal/Projects/PosBigdata/ProjBlocoA")

#install.packages('ggplot2', dependencies=TRUE)
#install.packages('GGally', dependencies=TRUE)

library(ggplot2)
library(GGally)
library(dplyr)

# Carregando dataset
ds <- read.csv("Qualidades_Dados_SMP_Filtrado.csv", sep=";", dec=",")
summary(ds)

ds <- replace(ds, ds=="NO", NA)
ds <- replace(ds, ds=="NI", NA)

ds$Media <- rowMeans(ds[,7:77], na.rm=TRUE)
ds$mai.12 <- ifelse(is.na(ds$mai.12), ds$Media, ds$mai.12)
ds$jun.12 <- ifelse(is.na(ds$jun.12), ds$Media, ds$jun.12)
ds$jul.12 <- ifelse(is.na(ds$jul.12), ds$Media, ds$jul.12)
ds$ago.12 <- ifelse(is.na(ds$ago.12), ds$Media, ds$ago.12)
ds$set.12 <- ifelse(is.na(ds$set.12), ds$Media, ds$set.12)
ds$out.12 <- ifelse(is.na(ds$out.12), ds$Media, ds$out.12)
ds$nov.12 <- ifelse(is.na(ds$nov.12), ds$Media, ds$nov.12)
ds$dez.12 <- ifelse(is.na(ds$dez.12), ds$Media, ds$dez.12)

ds$jan.13 <- ifelse(is.na(ds$jan.13), ds$Media, ds$jan.13)
ds$fev.13 <- ifelse(is.na(ds$fev.13), ds$Media, ds$fev.13)
ds$mar.13 <- ifelse(is.na(ds$mar.13), ds$Media, ds$mar.13)
ds$abr.13 <- ifelse(is.na(ds$abr.13), ds$Media, ds$abr.13)
ds$mai.13 <- ifelse(is.na(ds$mai.13), ds$Media, ds$mai.13)
ds$jun.13 <- ifelse(is.na(ds$jun.13), ds$Media, ds$jun.13)
ds$jul.13 <- ifelse(is.na(ds$jul.13), ds$Media, ds$jul.13)
ds$ago.13 <- ifelse(is.na(ds$ago.13), ds$Media, ds$ago.13)
ds$set.13 <- ifelse(is.na(ds$set.13), ds$Media, ds$set.13)
ds$out.13 <- ifelse(is.na(ds$out.13), ds$Media, ds$out.13)
ds$nov.13 <- ifelse(is.na(ds$nov.13), ds$Media, ds$nov.13)
ds$dez.13 <- ifelse(is.na(ds$dez.13), ds$Media, ds$dez.13)

ds$jan.14 <- ifelse(is.na(ds$jan.14), ds$Media, ds$jan.14)
ds$fev.14 <- ifelse(is.na(ds$fev.14), ds$Media, ds$fev.14)
ds$mar.14 <- ifelse(is.na(ds$mar.14), ds$Media, ds$mar.14)
ds$abr.14 <- ifelse(is.na(ds$abr.14), ds$Media, ds$abr.14)
ds$mai.14 <- ifelse(is.na(ds$mai.14), ds$Media, ds$mai.14)
ds$jun.14 <- ifelse(is.na(ds$jun.14), ds$Media, ds$jun.14)
ds$jul.14 <- ifelse(is.na(ds$jul.14), ds$Media, ds$jul.14)
ds$ago.14 <- ifelse(is.na(ds$ago.14), ds$Media, ds$ago.14)
ds$set.14 <- ifelse(is.na(ds$set.14), ds$Media, ds$set.14)
ds$out.14 <- ifelse(is.na(ds$out.14), ds$Media, ds$out.14)
ds$nov.14 <- ifelse(is.na(ds$nov.14), ds$Media, ds$nov.14)
ds$dez.14 <- ifelse(is.na(ds$dez.14), ds$Media, ds$dez.14)

ds$jan.15 <- ifelse(is.na(ds$jan.15), ds$Media, ds$jan.15)
ds$fev.15 <- ifelse(is.na(ds$fev.15), ds$Media, ds$fev.15)
ds$mar.15 <- ifelse(is.na(ds$mar.15), ds$Media, ds$mar.15)
ds$abr.15 <- ifelse(is.na(ds$abr.15), ds$Media, ds$abr.15)
ds$mai.15 <- ifelse(is.na(ds$mai.15), ds$Media, ds$mai.15)
ds$jun.15 <- ifelse(is.na(ds$jun.15), ds$Media, ds$jun.15)
ds$jul.15 <- ifelse(is.na(ds$jul.15), ds$Media, ds$jul.15)
ds$ago.15 <- ifelse(is.na(ds$ago.15), ds$Media, ds$ago.15)
ds$set.15 <- ifelse(is.na(ds$set.15), ds$Media, ds$set.15)
ds$out.15 <- ifelse(is.na(ds$out.15), ds$Media, ds$out.15)
ds$nov.15 <- ifelse(is.na(ds$nov.15), ds$Media, ds$nov.15)
ds$dez.15 <- ifelse(is.na(ds$dez.15), ds$Media, ds$dez.15)

ds$jan.16 <- ifelse(is.na(ds$jan.16), ds$Media, ds$jan.16)
ds$fev.16 <- ifelse(is.na(ds$fev.16), ds$Media, ds$fev.16)
ds$mar.16 <- ifelse(is.na(ds$mar.16), ds$Media, ds$mar.16)
ds$abr.16 <- ifelse(is.na(ds$abr.16), ds$Media, ds$abr.16)
ds$mai.16 <- ifelse(is.na(ds$mai.16), ds$Media, ds$mai.16)
ds$jun.16 <- ifelse(is.na(ds$jun.16), ds$Media, ds$jun.16)
ds$jul.16 <- ifelse(is.na(ds$jul.16), ds$Media, ds$jul.16)
ds$ago.16 <- ifelse(is.na(ds$ago.16), ds$Media, ds$ago.16)
ds$set.16 <- ifelse(is.na(ds$set.16), ds$Media, ds$set.16)
ds$out.16 <- ifelse(is.na(ds$out.16), ds$Media, ds$out.16)
ds$nov.16 <- ifelse(is.na(ds$nov.16), ds$Media, ds$nov.16)
ds$dez.16 <- ifelse(is.na(ds$dez.16), ds$Media, ds$dez.16)

ds$jan.17 <- ifelse(is.na(ds$jan.17), ds$Media, ds$jan.17)
ds$fev.17 <- ifelse(is.na(ds$fev.17), ds$Media, ds$fev.17)
ds$mar.17 <- ifelse(is.na(ds$mar.17), ds$Media, ds$mar.17)
ds$abr.17 <- ifelse(is.na(ds$abr.17), ds$Media, ds$abr.17)
ds$mai.17 <- ifelse(is.na(ds$mai.17), ds$Media, ds$mai.17)
ds$jun.17 <- ifelse(is.na(ds$jun.17), ds$Media, ds$jun.17)
ds$jul.17 <- ifelse(is.na(ds$jul.17), ds$Media, ds$jul.17)
ds$ago.17 <- ifelse(is.na(ds$ago.17), ds$Media, ds$ago.17)
ds$set.17 <- ifelse(is.na(ds$set.17), ds$Media, ds$set.17)
ds$out.17 <- ifelse(is.na(ds$out.17), ds$Media, ds$out.17)
ds$nov.17 <- ifelse(is.na(ds$nov.17), ds$Media, ds$nov.17)
ds$dez.17 <- ifelse(is.na(ds$dez.17), ds$Media, ds$dez.17)

ds$jan.18 <- ifelse(is.na(ds$jan.18), ds$Media, ds$jan.18)
ds$fev.18 <- ifelse(is.na(ds$fev.18), ds$Media, ds$fev.18)
ds$mar.18 <- ifelse(is.na(ds$mar.18), ds$Media, ds$mar.18)
summary(ds)

#Claro
dsClaro_PMDiario <- ds[with(ds, Nome.Fantasia == 'Claro'),]
#Oi
dsOi_PMDiario <- ds[with(ds, Nome.Fantasia == 'Oi'),]
#Tim
dsTim_PMDiario <- ds[with(ds, Nome.Fantasia == 'Tim'),]
#Vivo
dsVivo_PMDiario <- ds[with(ds, Nome.Fantasia == 'Vivo'),]

teste <- dsVivo_PMDiario %>% group_by(Unidade.Federativa, Indicador)
head(teste)

dadosOperadoraPorEstado <- aggregate(x = dsVivo_PMDiario$mar.18, by = list(dsVivo_PMDiario$Unidade.Federativa),
                                    FUN = mean)
head(dadosOperadoraPorEstado)
ggplot() +
  geom_bar(data = dadosOperadoraPorEstado,
           aes(x = Group.1, y = x), 
           stat = "identity", 
           color = "red", 
           fill = "orange"
  ) + xlab("Empresa - Estados") + ylab("Mes - Mar/2018") + ggtitle("Qualidade Serviço SMP")