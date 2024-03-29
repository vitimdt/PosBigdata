#install.packages('ggplot2', dependencies = TRUE)
#install.packages('GGally', dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)
#install.packages('party', dependencies=TRUE)
#install.packages('rpart', dependencies=TRUE)
#install.packages('rpart.plot', dependencies=TRUE)
#install.packages('neuralnet', dependencies=TRUE)

library('ggplot2')
library('GGally')
library('caret')
library('caTools')
library('e1071')
library('caret')
library('party')
library('rpart')
library('rpart.plot')
library('neuralnet')

convert_types <- function(obj,types){
  out <- lapply(1:length(obj),FUN = function(i){FUN1 <- 
    switch(types[i],
           character = as.character,
           numeric = as.numeric,
           factor = as.factor); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  as.data.frame(out)
}

# Carregando o dataset de Cleveland
df <- read.csv(file='processed.cleveland.data', sep=',', header= FALSE, dec='.', na.strings = '?')

# Definindo cabe�alho para as vari�veis relevantes do arquivo processado
names(df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
summary(df)

# Removendo os registros onde alguma v�riavel possui valor NA
df <- na.omit(df)

# Transformando os valores que definem os niveis de doen�a card�aca para o valor 1, informando a presen�a de alguma
# doen�a ou n�o (valor 0)
df[df$num > 0,]$num <- 1

# Gerando o gr�fico de correla��o entre as vari�veis
ggcorr(df, palette = "RdBu", label = TRUE)

# Definindo os tipos de vari�veis categ�ricas como factor
types <- c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor",
           "factor","factor","factor")
df_Factors <- convert_types(df, types)
str(df_Factors)

# Boxplot do diagn�stico pela vari�vel Fluoroscopia (Ca)
boxplot(df$ca ~ df$num,
        main="Diagn�stico por Fluoroscopia",
        ylab="Fluoroscopia (Ca)",xlab="Presen�a de doen�a card�aca")

# Boxplot do diagn�stico pela vari�vel Idade (Ca)
boxplot(df$age ~ df$num,
        main="Diagn�stico por Idade",
        ylab="Idade (Age)",xlab="Presen�a de doen�a card�aca")

# Histograma de pessoas pelo sexo e classificando pelo diagn�stico
df_Factors$sexFactor <- factor(df_Factors$sex, labels=c('Mulher', 'Homem'), levels = c(0,1))
ggplot(df_Factors, aes(x = sexFactor, fill = num)) +
  geom_histogram(stat="count", color ='black')  + 
  xlab("Sexo") + ylab("Frequ�ncia") + 
  ggtitle("Histograma da vari�vel sexo classificando pelo diagn�stico") +
  theme_classic()


### Testando modelos para fazer a predi�ao do diagn�stico ###
# Semente
set.seed(22082019)

### Naive Bayes ###
df_Factors <- df_Factors[,-15]
# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'num'
splitting = sample.split(df_Factors$num, SplitRatio = 0.7)
df_naivebayes_treinamento = subset(df_Factors, splitting == TRUE)
df_naivebayes_teste = subset(df_Factors, splitting == FALSE)

# Classifica��o (x: dataframe de treinamento sem a coluna 'num' / y: coluna 'num' do dataframe de treinamento)
classifier <- naiveBayes(x = df_naivebayes_treinamento[,-14], y = df_naivebayes_treinamento$num )

# Previs�o
prediction <- predict(classifier, newdata = df_naivebayes_teste[-14])

# Tabela de previs�es (coluna 'num' do dataframe de teste)
table_prediction <- table(df_naivebayes_teste[,14],prediction)
print(table_prediction)

# Matriz de confus�o
confusionMatrix(table_prediction)
# Acur�cia: 78%

### ---------------------------- ###


### �rvore de Decis�o ###

# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'num'
splitting = sample.split(df_Factors$num, SplitRatio = 0.7)
df_arvore_treinamento= subset(df_Factors, splitting == TRUE)
df_arvore_teste = subset(df_Factors, splitting == FALSE)

# Classifica��o
classifier <- rpart(formula = num ~ ., data = df_arvore_treinamento)
print(classifier)
rpart.plot(classifier)

# Previs�o
prediction <- predict(classifier, newdata = df_arvore_teste[,-14], type = 'class')
print(prediction)

# Matriz de confus�o
table_prediction <- table(df_arvore_teste[,14],prediction)
print(table_prediction)
confusionMatrix(table_prediction)
# Acur�cia: 69%

### ---------------------------- ###


### Regress�o Log�stica ###

# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'num'
splitting = sample.split(df_Factors$num, SplitRatio = 0.7)
df_logit_treinamento = subset(df_Factors, splitting == TRUE)
df_logit_teste = subset(df_Factors, splitting == FALSE)

formula <- num~.
logit <- glm(formula, data = df_logit_treinamento, family = 'binomial')
#summary(logit)

predict <- predict(logit, df_logit_teste, type = 'response')
# confusion matrix
table_mat <- table(df_logit_teste$num, predict > 0.5)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
# Acur�cia: 87%

### ---------------------------- ###


### Redes Neurais ###

# Criando um outro dataset sem vari�veis categ�ricas para testar um modelo de redes neurais
# Selecionando 4 vari�veis relevantes ("age","sex","ca","thal")
dfReduzido <- df[,c(1,2,12,13,14)]
summary(dfReduzido)
# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'num'
splitting = sample.split(dfReduzido$num, SplitRatio = 0.7)
df_neuralnet_treinamento = subset(dfReduzido, splitting == TRUE)
df_neuralnet_teste = subset(dfReduzido, splitting == FALSE)

# Definindo a rede neural
nn <- neuralnet(formula = num ~ ., data = df_neuralnet_treinamento, hidden = 10, lifesign = "minimal", 
                linear.output = FALSE, threshold = 0.1)
plot(nn, rep = "best")

# Previs�o
prediction <- predict(nn, newdata = df_neuralnet_teste[,-5])
print(prediction)

# Matriz de confus�o
table_prediction <- table(df_neuralnet_teste[,5], prediction > 0.5)
print(table_prediction)

accuracy_Test <- sum(diag(table_prediction)) / sum(table_prediction)
accuracy_Test
# Acur�cia: 83%




### -------------  Carregando o dataset da Su��a   --------------- ###

# Carregando o dataset da Su��a
dfSuica <- read.csv(file='processed.switzerland.data', sep=',', header= FALSE, dec='.', na.strings = '?')

# Definindo cabe�alho para as vari�veis relevantes do arquivo processado
names(dfSuica) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach","exang", "oldpeak",
                     "slope", "ca", "thal", "num")
summary(dfSuica)

# Deixando apenas as 3 primeiras vari�veis que n�o tem valores NAs e o valor de diagn�stico
dfSuica <- dfSuica[,c(1,2,3,14)]

# Transformando os valores que definem os niveis de doen�a card�aca para o valor 1, informando a presen�a de alguma
# doen�a ou n�o (valor 0)
dfSuica[dfSuica$num > 0,]$num <- 1


### Redes Neurais ###

# Criando um outro dataset sem vari�veis categ�ricas para testar um modelo de redes neurais
# Selecionando 3 vari�veis ("age","sex","cp")
dfReduzido <- df[,c(1,2,3,14)]
summary(dfReduzido)
# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'num'
splitting = sample.split(dfReduzido$num, SplitRatio = 0.9)
df_neuralnet_treinamento = subset(dfReduzido, splitting == TRUE)
df_neuralnet_teste = subset(dfReduzido, splitting == FALSE)

# Definindo a rede neural
nn <- neuralnet(formula = num ~ ., data = df_neuralnet_treinamento, hidden = 7, lifesign = "minimal", 
                linear.output = FALSE, threshold = 0.1)
plot(nn, rep = "best")

# Previs�o
prediction <- predict(nn, newdata = df_neuralnet_teste[,-4])
print(prediction)

# Matriz de confus�o
table_prediction <- table(df_neuralnet_teste[,4], prediction > 0.5)
print(table_prediction)

accuracy_Test <- sum(diag(table_prediction)) / sum(table_prediction)
accuracy_Test
# Acur�cia: 73%


# Aplicando o dataset completo da Sui�a no modelo de Redes Neurais para verificar o resultado
prediction <- predict(nn, newdata = dfSuica[,-4])
print(prediction)

# Matriz de confus�o
table_prediction <- table(dfSuica[,4], prediction > 0.5)
print(table_prediction)

accuracy_Test <- sum(diag(table_prediction)) / sum(table_prediction)
accuracy_Test
# Acur�cia: 77%
