# Tarefa 1 (Avalia��o Algoritmos) - An�lise de uma base de ag�ncia de viagens aplicando algoritmos de 
# Agrupamentos (Clustering)

# -------------------------
# Instala��o de Pacotes Externos
# -------------------------
#install.packages('ggplot2', dependencies=TRUE)

# -------------------------
# Carregando as LIBs
# -------------------------
library('ggplot2')

# Setando diret�rio onde est�o os arquivos
setwd("D:\\Pessoal\\Projects\\PosBigdata\\Analytics_R\\Tarefa_1-AAlgoritmos")
# Carregando dataset
dsTrip <- read.csv("tripadvisor_review.csv", sep=",", dec=".")

summary(dsTrip)
dim(dsTrip)
sapply(dsTrip, class)

# Histograma das tr�s categorias com maior m�dia pontua��o
hist(dsTrip$Category.7,
     main="Histograma da Categoria 7 (Piqueniques)",
     xlab="Galerias de arte",
     ylab="Frequ�ncia",
     col="orange",
     border="brown")

# Histograma das tr�s categorias com maior m�dia pontua��o
hist(dsTrip$Category.8,
     main="Histograma da Categoria 8 (Praias)",
     xlab="Galerias de arte",
     ylab="Frequ�ncia",
     col="orange",
     border="brown")

# Histograma das tr�s categorias com maior m�dia pontua��o
hist(dsTrip$Category.10,
     main="Histograma da Categoria 10 (Espa�os religiosos)",
     xlab="Galerias de arte",
     ylab="Frequ�ncia",
     col="orange",
     border="brown")

# Segundo estes clientes, quais s�o os maiores atrativos do continente?
# Resp.: Pelas melhores m�dias das notas � poss�vel identificar que os maiores atrativos s�o:
# Category 7 (Piqueniques), Category 8 (Praias) e Category 10 (Espa�os Religiosos)

# Quais grupos de itens agradam mais?
# Resp.: As categorias 6 e 10 (Resorts e Espa�os Religiosos) tiveram as maiores notas, mas as categorias
# melhores avaliadas s�o as que tiveram as maiores m�dias.

# Definindo o seed para reprodu��o posterior.
set.seed(1500)

# Executando o m�todo do cotovelo com todas as vari�veis para verificar um bom n�mero de clusters
kmean_withinss <- function(k) {
  cluster <- kmeans(dsTrip[2:11], k)
  return (cluster$tot.withinss)
}
# Set maximum cluster 
max_k <-15 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

elbow <- data.frame(2:max_k, wss)

# Plot the graph with gglop
ggplot(elbow, aes(x=X2.max_k, y=wss)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1, 15, by = 1),labs(x='N�mero de Agrupamentos'))

# Executar o Kmeans de todas as vari�veis com 5 clusters
km <- kmeans(dsTrip[2:11], 5, nstart=100)
# Visualizando.
plot(dsTrip[2:11], col=(km$cluster+1) , main="Resultado do K-m�dias com 5 agrupamentos", pch=20, cex=2)


# Agrupando as notas de cada categoria em 3 grupos tur�sticos e tirando a m�dia por usu�rio
# Coluna Entretenimento (Categoria 2 + Categoria 7 + Categoria 8 + Categoria 9)
# Coluna Hotelaria e Gastronomia (Categoria 3 + Categoria 4 + Categoria 6)
# Coluna Cultural e Historico (Categoria 1 + Categoria 5 + Categoria 10)
dsTripClassAgrupado <- dsTrip[1]
dsTripClassAgrupado$ColEntretenimento <- with(dsTrip, (Category.2 + Category.7 + 
                                                      Category.8 + Category.9)/4)
dsTripClassAgrupado$ColHotelGastro <- with(dsTrip, (Category.3 + Category.4 + Category.6)/3)
dsTripClassAgrupado$ColCulturalHist <- with(dsTrip, (Category.1 + Category.5 + Category.10)/3)

summary(dsTripClassAgrupado)

# Executando o m�todo do cotovelo com todas as vari�veis para verificar um bom n�mero de clusters
kmean_withinss <- function(k) {
  cluster <- kmeans(dsTripClassAgrupado[2:4], k)
  return (cluster$tot.withinss)
}
# Set maximum cluster 
max_k <-10 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

elbow <- data.frame(2:max_k, wss)

# Plot the graph with gglop
ggplot(elbow, aes(x=X2.max_k, y=wss)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1, 10, by = 1),labs(x='N�mero de Agrupamentos'))

# Executar o Kmeans das tr�s novas vari�veis com 5 clusters
km2 <- kmeans(dsTripClassAgrupado[2:4], 5, nstart=100)
# Visualizando.
plot(dsTripClassAgrupado[2:4], col=(km2$cluster+1) , main="Resultado do K-m�dias com 5 agrupamentos", pch=20, cex=2)



# Fazendo Agrupamento de duas categorias que tiveram melhor resultado com o K-Means
kmean_withinss <- function(k) {
  cluster <- kmeans(dsTrip[,c(4,7)], k)
  return (cluster$tot.withinss)
}
# Set maximum cluster 
max_k <- 15
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

elbow <- data.frame(2:max_k, wss)

# Plot the graph with gglop
ggplot(elbow, aes(x=X2.max_k, y=wss)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1, 15, by = 1),labs(x='N�mero de Agrupamentos'))

# Executar o Kmeans das tr�s novas vari�veis com 5 clusters
km2 <- kmeans(dsTrip[,c(4,7)], 5, nstart=100)
# Visualizando.
plot(dsTrip[,c(4,7)], col=(km2$cluster+1),
      xlab="Bares",
      ylab="Resorts",
      main="Resultado do K-m�dias com 5 agrupamentos", 
      pch=20, cex=2)

# Poder�amos direcionar uma campanha promovendo o continente para grupos de clientes que se interessem 
# por determinados tipos de atra��es do local?
# Resp.: Analisando os resultados dos agrupamentos, podemos avaliar os grupos de usu�rios baseado nas notas
# entre Resorts e Bares, que tiveram resultados mais claros. Atrav�s do gr�fico, podemos utilizar como:
# 1 - Usu�rios com caracter�sticas dos grupos Azul Claro e Vermelho podem receber propagandas de outras 
#     categorias para viagens;
# 2 - Usu�rios com caracter�sticas dos grupos Verde e Azul Escuro podem receber propagandas de Resorts da �sia;
# 3 - Usu�rios com caracter�sticas do grupo Rosa podem receber propagandas de Resorts e Bares da �sia.
