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


dsTripClass <- dsTrip[2:11]
summary(dsTripClass)

# Definindo o seed para reprodu��o posterior.
set.seed(1500)

# Executando o m�todo do cotovelo com todas as vari�veis para verificar um bom n�mero de clusters
kmean_withinss <- function(k) {
  cluster <- kmeans(dsTripClass, k)
  return (cluster$tot.withinss)
}
# Set maximum cluster 
max_k <-20 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

elbow <- data.frame(2:max_k, wss)

# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

# Executar o Kmeans de todas as vari�veis com 3 clusters
km <- kmeans(dsTripClass, 3, nstart=1000)
# Visualizando.
plot(dsTripClass, col=(km$cluster+1) , main="Resultado do K-m�dias com 3 agrupamentos", pch=20, cex=3)