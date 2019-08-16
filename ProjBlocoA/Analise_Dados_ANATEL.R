setwd("D:/Pessoal/Projects/PosBigdata/ProjBlocoA")

#install.packages('ggplot2', dependencies=TRUE)
#install.packages('GGally', dependencies=TRUE)

library(ggplot2)
library(GGally)
library(dplyr)

# Carregando dataset
ds <- read.csv("Qualidade_SMP.csv", sep=";", dec=",")
summary(ds)
str(ds)

ds <- replace(ds, ds=="NO", NA)
ds <- replace(ds, ds=="NI", NA)
ds <- replace(ds, is.na(ds), 0)
#ds <- na.omit(ds)

dsPMDiurno <- ds[with(ds, Período.de.Coleta == 'PMM1'),]
dsPMNoturno <- ds[with(ds, Período.de.Coleta == 'PMM2'),]
dsPMDiario <- ds[with(ds, Período.de.Coleta == 'PMT'),]
dsPMensal <- ds[with(ds, Período.de.Coleta == 'Mensal'),]

#Algar
#dsAlgar_PMDiario <- dsPMDiario[with(dsPMDiario, Nome.Fantasia == 'Algar'),]
#Claro
dsClaro_PMDiario <- dsPMDiario[with(dsPMDiario, Nome.Fantasia == 'Claro'),]
#Datora Mobile
#dsDatora_PMDiario <- dsPMDiario[with(dsPMDiario, Nome.Fantasia == 'Datora Mobile'),]
#Nextel
dsNextel_PMDiario <- dsPMDiario[with(dsPMDiario, Nome.Fantasia == 'Nextel'),]
#Oi
dsOi_PMDiario <- dsPMDiario[with(dsPMDiario, Nome.Fantasia == 'Oi'),]
#Porto Seguro Conecta
#dsPorto_PMDiario <- dsPMDiario[with(dsPMDiario, Nome.Fantasia == 'Porto Seguro Conecta'),]
#Sercomtel
#dsSercomtel_PMDiario <- dsPMDiario[with(dsPMDiario, Nome.Fantasia == 'Sercomtel'),]
#Tim
dsTim_PMDiario <- dsPMDiario[with(dsPMDiario, Nome.Fantasia == 'Tim'),]
#Vivo
dsVivo_PMDiario <- dsPMDiario[with(dsPMDiario, Nome.Fantasia == 'Vivo'),]

dadosOperadoraPorEstado <- aggregate(x = dsAlgar_PMDiario$mar.18, by = list(dsAlgar_PMDiario$Unidade.Federativa),
                                    FUN = mean)
head(dadosOperadoraPorEstado)
ggplot() +
  geom_bar(data = dadosOperadoraPorEstado,
           aes(x = Group.1, y = x), 
           stat = "identity", 
           color = "red", 
           fill = "orange"
  ) + xlab("Empresa - Estados") + ylab("Mes - Mar/2018") + ggtitle("Qualidade Serviço SMP")

