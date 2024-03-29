# Tarefa 2 (Avalia��o Algoritmos) - An�lise de um dataset de sess�es de compras pela internet

# -------------------------
# Instala��o de Pacotes Externos
# -------------------------
#install.packages('ggplot2', dependencies=TRUE)
#install.packages('GGally', dependencies=TRUE)
#install.packages('party', dependencies=TRUE)
#install.packages('rpart', dependencies=TRUE)
#install.packages('rpart.plot', dependencies=TRUE)
#install.packages('rattle', dependencies=TRUE)
#install.packages("caTools")
#install.packages('e1071')
#install.packages('caret')
#install.packages("randomForest",dependencies = TRUE)

# -------------------------
# Carregando as LIBs
# -------------------------
library(ggplot2)
library(GGally)
library(party)
library(rpart)
library(rpart.plot)
library(rattle)
library(caTools)
library(e1071)
library(caret)
library(randomForest)

# Setando diret�rio onde est�o os arquivos
setwd("D:\\Pessoal\\Projects\\PosBigdata\\Analytics_R\\Tarefa_2-AAlgoritmos")
# Carregando dataset
dsIntencaoCompras <- read.csv("online_shoppers_intention.csv", sep=",", dec=".")

summary(dsIntencaoCompras)

# Transformando as vari�veis n�o num�ricas e num�ricas
# Vari�veis booleanas ser�o transformadas em 0 (FALSE) e 1 (TRUE)
dsIntencaoCompras$Revenue <- factor(dsIntencaoCompras$Revenue, levels = c(FALSE,TRUE), labels = c(0,1))

dsIntencaoCompras$Weekend <- factor(dsIntencaoCompras$Weekend, levels = c(FALSE,TRUE), labels = c(0,1))

# Transformando a vari�vel "VisitorType" com os seguintes valores:
# New_Visitor = 0
# Returning_Visitor = 1
# Other = 2
dsIntencaoCompras$VisitorType <- factor(dsIntencaoCompras$VisitorType, levels = c('New_Visitor','Returning_Visitor','Other'), labels = c(0,1,2))

# Transformando a vari�vel "Month" com os valores num�ricos:
dsIntencaoCompras$Month <- factor(dsIntencaoCompras$Month, 
                                     levels = c('Feb', 'Mar', 'May', 'June', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), 
                                     labels = c(2, 3, 5, 6, 7,8,9,10,11,12))

# Histograma de Revenue pela vari�vel Weekend
ggplot(dsIntencaoCompras, aes(x = Weekend, fill = Revenue)) +
  geom_histogram(stat="count", color ='black')  + 
  xlab("Weekend") + ylab("Frequ�ncia") + ggtitle("Histograma de Revenue por Weekend") +
  theme_grey()

# Histograma de Revenue pela vari�vel VisitorType
ggplot(dsIntencaoCompras, aes(x = VisitorType, fill = Revenue)) +
  geom_histogram(stat="count", color ='black')  + 
  xlab("VisitorType") + ylab("Frequ�ncia") + ggtitle("Histograma de Revenue por VisitorType") +
  theme_grey()

# Histograma de Revenue por SpecialDay
ggplot(dsIntencaoCompras, aes(x = SpecialDay, fill = Revenue)) +
  geom_histogram(stat="count", color ='black')  + 
  xlab("SpecialDay") + ylab("Frequ�ncia") + ggtitle("Histograma de Revenue por SpecialDay") +
  theme_grey()

# Histograma de Revenue por Month
ggplot(dsIntencaoCompras, aes(x = Month, fill = Revenue)) +
  geom_histogram(stat="count", color ='black')  + 
  xlab("Month") + ylab("Frequ�ncia") + ggtitle("Histograma de Revenue por Month") +
  theme_grey()

# PairsPlot das outras vari�veis num�ricas, para verificar se existe correla��o entre elas
ggpairs(dsIntencaoCompras, columns = 1:9, 
        ggplot2::aes(colour=Revenue), title = "Inten��o de Campras Online")


head(dsIntencaoCompras)
set.seed(19000)
# Misturando o DataSet
dsNewIntencaoCompras <- dsIntencaoCompras[sample(1:nrow(dsIntencaoCompras), nrow(dsIntencaoCompras), replace=FALSE),]
head(dsNewIntencaoCompras)

## ----------------------- ##

### Naive Bayes - com tratamento ###

df_naivebayes <- dsNewIntencaoCompras

# Define o valor da semente
#set.seed(18072019)

# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'Revenue'
splitting = sample.split(df_naivebayes$Revenue, SplitRatio = 0.7)
df_naivebayes_treinamento = subset(df_naivebayes, splitting == TRUE)
df_naivebayes_teste = subset(df_naivebayes, splitting == FALSE)

str(df_naivebayes_treinamento)
str(df_naivebayes_teste)

# Classifica��o (x: dataframe de treinamento sem a coluna 'Revenue' / y: coluna 'Revenue' do dataframe de treinamento)
classifier <- naiveBayes(x = df_naivebayes_treinamento[,-18], y = df_naivebayes_treinamento$Revenue )
print(classifier)

# Previs�o
prediction <- predict(classifier, newdata = df_naivebayes_teste[-18])

# Tabela de previs�es (coluna 'Revenue' do dataframe de teste)
table_prediction <- table(df_naivebayes_teste[,18],prediction)
print(table_prediction)

# Matriz de confus�o
confusionMatrix(table_prediction)

# >>> Acur�cia: 80%

### ---------------------------- ###

### �rvore - com tratamento ###

df_arvore <- dsNewIntencaoCompras

str(df_arvore)

# Semente
#set.seed(18072019)

# Treinamento e teste
splitting = sample.split(df_arvore$Revenue, SplitRatio = 0.7)
df_arvore_treinamento= subset(df_arvore, splitting == TRUE)
df_arvore_teste = subset(df_arvore, splitting == FALSE)

# Classifica��o
classifier <- rpart(formula = Revenue ~ ., data = df_arvore_treinamento)
print(classifier)
plot(classifier)

rpart.plot(classifier)
# Determinantes: PageValue, BounceRates e Month

# Previs�o
prediction <- predict(classifier, newdata = df_arvore_teste[,-18], type = 'class')
print(prediction)


# Matriz de confus�o
table_prediction <- table(df_arvore_teste[,18],prediction)
print(table_prediction)
confusionMatrix(table_prediction)

# >>> Acur�cia: 89.24%

### ----------------------- ###

### Random Forest - com tratamento ###

df_rf <- dsNewIntencaoCompras

str(df_rf)

# Semente
#set.seed(18072019)

# Treinamento e teste
splitting = sample.split(df_rf$Revenue, SplitRatio = 0.7)
df_rf_treinamento= subset(df_rf, splitting == TRUE)
df_rf_teste = subset(df_rf, splitting == FALSE)

# Realiza classifica��o (30 �rvores)
classifier <- randomForest(x = df_rf_treinamento[,-18],y = df_rf_treinamento$Revenue,ntree = 30)
print(classifier)
# Number of trees: 30
# No. of variables tried at each split: 4
# OOB estimate of  error rate: 10.17%

# Gr�fico
plot(classifier)


# Previs�o
prediction <- predict(classifier, newdata = df_rf_teste[,-18])
print(prediction)

# Matriz de confus�o
table_prediction <- table(df_rf_teste[,18],prediction)
print(table_prediction)
confusionMatrix(table_prediction)

# R: Acur�cia: 89.54%

### ------------------------------ ###




# Criando um Modelo de Regress�o Log�stica
# Treinamento 70% e Teste 30%
#set.seed(12345)
create_train_test <- function(data, size = 0.7, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(dsNewIntencaoCompras, 0.7, train = TRUE)
data_test <- create_train_test(dsNewIntencaoCompras, 0.7, train = FALSE)
dim(data_train)
dim(data_test)

formula <- Revenue~.
logit <- glm(formula, data = data_train, family = 'binomial')
summary(logit)

predict <- predict(logit, data_test, type = 'response')
# confusion matrix
table_mat <- table(data_test$Revenue, predict > 0.1)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test



# Criando RPart e modelo em �rvores de decis�o
fit <- rpart(Revenue ~ ., data=data_train, method = "class", minsplit = 2, minbucket = 1, cp = -1)
prp(fit)

pred <- predict(fit, data_test[,-18], type="class")
table_mat <- table(pred, data_test$Revenue)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
