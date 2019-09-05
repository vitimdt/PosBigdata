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

dsQualidade$jan.18 <- ifelse(is.na(dsQualidade$jan.18), dsQualidade$Media, dsQualidade$jan.18)
dsQualidade$fev.18 <- ifelse(is.na(dsQualidade$fev.18), dsQualidade$Media, dsQualidade$fev.18)
dsQualidade$mar.18 <- ifelse(is.na(dsQualidade$mar.18), dsQualidade$Media, dsQualidade$mar.18)
summary(dsQualidade)

# Filtrando do dataset da Operadora Vivo com a média dos meses disponíveis dos tres indicadores (SMP8, SMP10, SMP11D)
dsQualidadeAgrupPorEmpresaeUF <- dsQualidade %>% 
  group_by(NomeFantasia, UF) %>% 
  summarise(
    jan_18 = mean(jan.18), fev_18 = mean(fev.18), mar_18 = mean(mar.18))
head(dsQualidadeAgrupPorEmpresaeUF)

ggplot() +
  geom_bar(data = dsQualidadeAgrupPorEmpresaeUF,
           aes(x = UF, y = mar_18), 
           stat = "identity",
           fill = "orange"
  ) + facet_grid(NomeFantasia ~ .) + xlab("Estados") + ylab("Mar/2018") + ggtitle("Qualidade Serviço SMP")


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

ggplot() +
  geom_bar(data = dsAssinantesAgrupPorEmpresaeUF,
           aes(x = UF, y = ass_mar_2018), 
           stat = "identity",
           fill = "orange"
  ) + facet_grid(NomeFantasia ~ .) + xlab("Estados") + ylab("Mar/2018") + ggtitle("Quantidade Assinantes SMP")

##################################################################################
# Carregando dataset de Reclamções SMP ###########################################
##################################################################################
dsReclamacoes <- read.csv("ReclamaçõesANATEL/Reclamacoes_SMP_Consolidado_2018_v3.csv", sep=";", dec=",")

summary(dsReclamacoes)
str(dsReclamacoes)

dsReclamacoes <- dsReclamacoes[dsReclamacoes$Tipo == 'Reclamação',]

dsReclamacoesAgrupado <- dsReclamacoes %>% 
  group_by(Ano, Mes, NomeFantasia, UF) %>% summarise(QtdeReclamacoes = sum(QtdeSolic))
summary(dsReclamacoesAgrupado)

dsReclamacoesAgrupadoPorMeses <- distinct(dsReclamacoes, NomeFantasia, UF)

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2018 & Mes == 1), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_01_2018 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-3)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2018 & Mes == 2), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_02_2018 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-4)]

dsFiltrado <- dsReclamacoesAgrupado[with(dsReclamacoesAgrupado, Ano == 2018 & Mes == 3), c(3,4,5)]
dsReclamacoesAgrupadoPorMeses <- 
  merge(x=dsReclamacoesAgrupadoPorMeses, y=dsFiltrado, by=c('NomeFantasia','UF'), all.x = TRUE)
dsReclamacoesAgrupadoPorMeses$Rec_03_2018 <- dsReclamacoesAgrupadoPorMeses$QtdeReclamacoes
dsReclamacoesAgrupadoPorMeses <- dsReclamacoesAgrupadoPorMeses[,c(-5)]

summary(dsReclamacoesAgrupadoPorMeses)

dsReclamacoesAgrupadoPorMeses <- replace(dsReclamacoesAgrupadoPorMeses, is.na(dsReclamacoesAgrupadoPorMeses), 0)

ggplot() +
  geom_bar(data = dsReclamacoesAgrupadoPorMeses,
           aes(x = UF, y = Rec_03_2018), 
           stat = "identity",
           fill = "orange"
  ) + facet_grid(NomeFantasia ~ .) + xlab("Estados") + ylab("Mar/2018") + ggtitle("Reclamações ANATEL - SMP")

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

# Percentual de reclamações por Operadora
ggplot() +
  geom_bar(data = dsQualidadeComReclamacoes,
           aes(x = UF, y = PercRec_jan_2018), 
           stat = "identity",
           fill = "orange"
  ) + facet_grid(NomeFantasia ~ .) + xlab("Estados") + ylab("Jan/2018") + ggtitle("Reclamações (%) ANATEL - SMP")

# Percentual de reclamações por Operadora
ggplot() +
  geom_bar(data = dsQualidadeComReclamacoes,
           aes(x = UF, y = PercRec_fev_2018), 
           stat = "identity",
           fill = "orange"
  ) + facet_grid(NomeFantasia ~ .) + xlab("Estados") + ylab("Fev/2018") + ggtitle("Reclamações (%) ANATEL - SMP")

# Percentual de reclamações por Operadora
ggplot() +
  geom_bar(data = dsQualidadeComReclamacoes,
           aes(x = UF, y = PercRec_mar_2018), 
           stat = "identity",
           fill = "orange"
  ) + facet_grid(NomeFantasia ~ .) + xlab("Estados") + ylab("Mar/2018") + ggtitle("Reclamações (%) ANATEL - SMP")

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
# Setando a semente
set.seed(18140)

# Reduzindo as variáveis em duas
tsne_reduction <- tsne(dsQualidadeComReclamacoes[,3:14], k = 2, perplexity = 30, epoch = 100)
dfReduction <- as.data.frame(tsne_reduction)
dfReduction$Empresa <- dsQualidadeComReclamacoes$NomeFantasia
str(dfReduction)
ggplot(data = dfReduction, aes(x = V1, y = V2)) +
  geom_point(aes(color = Empresa)) + 
  theme_bw()

# Executando o método do cotovelo com todas as variáveis para verificar um bom número de clusters
kmean_withinss <- function(k) {
  cluster <- kmeans(dfReduction[,-3], k)
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
km <- kmeans(dfReduction[,-3], 4, nstart=100)
# Visualizando.
plot(dfReduction[,-3], col=(km$cluster+1) ,
     main="Resultado do K-médias com 4 agrupamentos", pch=20, cex=2)

km$cluster <- as.factor(km$cluster)
ggplot(dfReduction, aes(x=V1, y=V2, color=km$cluster)) +
  geom_point()
