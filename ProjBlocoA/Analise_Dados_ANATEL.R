setwd("D:/Pessoal/Projects/PosBigdata/ProjBlocoA")

#install.packages('ggplot2', dependencies=TRUE)
#install.packages('GGally', dependencies=TRUE)
#install.packages(c('ggthemes', 'reshape2'))

library(ggplot2)
library(GGally)
library(dplyr)
library(reshape2)
library(sjlabelled)

# Carregando dataset
ds <- read.csv("Qualidades_Dados_SMP_Filtrado.csv", sep=";", dec=",")

# Removendo colunas irrelevantes
ds <- ds[,c(-1, -4, -6, -7, -8, -9, -10, -11, -12, -13, -14)]

summary(ds)
str(ds)

#ds <- replace(ds, ds=="NO", NA)
#ds <- replace(ds, ds=="NI", NA)

ds$Media <- rowMeans(ds[,4:66], na.rm=TRUE)
#ds$mai.12 <- ifelse(is.na(ds$mai.12), ds$Media, ds$mai.12)
#ds$jun.12 <- ifelse(is.na(ds$jun.12), ds$Media, ds$jun.12)
#ds$jul.12 <- ifelse(is.na(ds$jul.12), ds$Media, ds$jul.12)
#ds$ago.12 <- ifelse(is.na(ds$ago.12), ds$Media, ds$ago.12)
#ds$set.12 <- ifelse(is.na(ds$set.12), ds$Media, ds$set.12)
#ds$out.12 <- ifelse(is.na(ds$out.12), ds$Media, ds$out.12)
#ds$nov.12 <- ifelse(is.na(ds$nov.12), ds$Media, ds$nov.12)
#ds$dez.12 <- ifelse(is.na(ds$dez.12), ds$Media, ds$dez.12)

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

# Filtrando do dataset da Operadora Vivo com a média dos meses disponíveis dos tres indicadores (SMP8, SMP10, SMP11D)
dsAgrupadoPorEmpresaeUF <- ds %>% 
  group_by(Nome.Fantasia, Unidade.Federativa) %>% 
  summarise(
    #mai_12 = mean(mai.12), jun_12 = mean(jun.12),
    #jul_12 = mean(jul.12), ago_12 = mean(ago.12), set_12 = mean(set.12), out_12 = mean(out.12), nov_12 = mean(nov.12), dez_12 = mean(dez.12),
    jan_13 = mean(jan.13), fev_13 = mean(fev.13), mar_13 = mean(mar.13), abr_13 = mean(abr.13), mai_13 = mean(mai.13), jun_13 = mean(jun.13),
    jul_13 = mean(jul.13), ago_13 = mean(ago.13), set_13 = mean(set.13), out_13 = mean(out.13), nov_13 = mean(nov.13), dez_13 = mean(dez.13),
    jan_14 = mean(jan.14), fev_14 = mean(fev.14), mar_14 = mean(mar.14), abr_14 = mean(abr.14), mai_14 = mean(mai.14), jun_14 = mean(jun.14),
    jul_14 = mean(jul.14), ago_14 = mean(ago.14), set_14 = mean(set.14), out_14 = mean(out.14), nov_14 = mean(nov.14), dez_14 = mean(dez.14),
    jan_15 = mean(jan.15), fev_15 = mean(fev.15), mar_15 = mean(mar.15), abr_15 = mean(abr.15), mai_15 = mean(mai.15), jun_15 = mean(jun.15),
    jul_15 = mean(jul.15), ago_15 = mean(ago.15), set_15 = mean(set.15), out_15 = mean(out.15), nov_15 = mean(nov.15), dez_15 = mean(dez.15),
    jan_16 = mean(jan.16), fev_16 = mean(fev.16), mar_16 = mean(mar.16), abr_16 = mean(abr.16), mai_16 = mean(mai.16), jun_16 = mean(jun.16),
    jul_16 = mean(jul.16), ago_16 = mean(ago.16), set_16 = mean(set.16), out_16 = mean(out.16), nov_16 = mean(nov.16), dez_16 = mean(dez.16),
    jan_17 = mean(jan.17), fev_17 = mean(fev.17), mar_17 = mean(mar.17), abr_17 = mean(abr.17), mai_17 = mean(mai.17), jun_17 = mean(jun.17),
    jul_17 = mean(jul.17), ago_17 = mean(ago.17), set_17 = mean(set.17), out_17 = mean(out.17), nov_17 = mean(nov.17), dez_17 = mean(dez.17),
    jan_18 = mean(jan.18), fev_18 = mean(fev.18), mar_18 = mean(mar.18))
head(dsAgrupadoPorEmpresaeUF)

ggplot() +
  geom_bar(data = dsAgrupadoPorEmpresaeUF[with(dsAgrupadoPorEmpresaeUF,Nome.Fantasia == 1),],
           aes(x = Unidade.Federativa, y = mar_18), 
           stat = "identity",
           fill = "orange"
  ) + xlab("Estados") + ylab("Mes - Mar/2018") + ggtitle("Qualidade Serviço SMP")

# Ajustando os dados para gráfico
meltdados <- melt(dsAgrupadoPorEmpresaeUF,id=c("Nome.Fantasia","Unidade.Federativa"))
head(meltdados)

# Criando o gráfico
ggplot(meltdados[with(meltdados,Nome.Fantasia == 1),],
       aes(x=variable,y=value,colour=Unidade.Federativa,group=Unidade.Federativa)) + 
  geom_line() +
  theme(axis.text.x = element_text(angle=90, hjust=1))




# Ajustando as variáveis categoricas
ds$Nome.Fantasia <- factor(ds$Nome.Fantasia, levels=c('Claro', 'Oi', 'Tim', 'Vivo'), labels=c(0,1,2,3))

ds$Unidade.Federativa <- factor(ds$Unidade.Federativa, levels=c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG',
                                                                'MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO'),
                                labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26))
ds$Indicador <- factor(ds$Indicador, levels=c('SMP8','SMP10','SMP11D'), labels=c(0,1,2))