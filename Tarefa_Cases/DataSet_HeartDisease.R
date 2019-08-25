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

# Definindo cabeçalho para as variáveis relevantes do arquivo processado
names(df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
summary(df)

# Removendo os registros onde alguma váriavel possui valor NA
df <- na.omit(df)

# Transformando os valores que definem os niveis de doença cardíaca para o valor 1, informando a presença de alguma
# doença ou não (valor 0)
df[df$num > 0,]$num <- 1

# Gerando o gráfico de correlação entre as variáveis
ggcorr(df, palette = "RdBu", label = TRUE)

# Definindo os tipos de variáveis categóricas como factor
types <- c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor",
           "factor","factor","factor")
df_Factors <- convert_types(df, types)
str(df_Factors)

# Boxplot do diagnóstico pela variável Fluoroscopia (Ca)
boxplot(df$ca ~ df$num,
        main="Diagnóstico por Fluoroscopia",
        ylab="Fluoroscopia (Ca)",xlab="Presença de doença cardíaca")

# Boxplot do diagnóstico pela variável Idade (Ca)
boxplot(df$age ~ df$num,
        main="Diagnóstico por Idade",
        ylab="Idade (Age)",xlab="Presença de doença cardíaca")

# Histograma de pessoas pelo sexo e classificando pelo diagnóstico
df_Factors$sexFactor <- factor(df_Factors$sex, labels=c('Mulher', 'Homem'), levels = c(0,1))
ggplot(df_Factors, aes(x = sexFactor, fill = num)) +
  geom_histogram(stat="count", color ='black')  + 
  xlab("Sexo") + ylab("Frequência") + 
  ggtitle("Histograma da variável sexo classificando pelo diagnóstico") +
  theme_classic()


### Testando modelos para fazer a prediçao do diagnóstico ###
# Semente
set.seed(22082019)

### Naive Bayes ###
df_Factors <- df_Factors[,-15]
# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'num'
splitting = sample.split(df_Factors$num, SplitRatio = 0.7)
df_naivebayes_treinamento = subset(df_Factors, splitting == TRUE)
df_naivebayes_teste = subset(df_Factors, splitting == FALSE)

# Classificação (x: dataframe de treinamento sem a coluna 'num' / y: coluna 'num' do dataframe de treinamento)
classifier <- naiveBayes(x = df_naivebayes_treinamento[,-14], y = df_naivebayes_treinamento$num )

# Previsão
prediction <- predict(classifier, newdata = df_naivebayes_teste[-14])

# Tabela de previsões (coluna 'num' do dataframe de teste)
table_prediction <- table(df_naivebayes_teste[,14],prediction)
print(table_prediction)

# Matriz de confusão
confusionMatrix(table_prediction)
# Acurácia: 78%

### ---------------------------- ###


### Árvore de Decisão ###

# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'num'
splitting = sample.split(df_Factors$num, SplitRatio = 0.7)
df_arvore_treinamento= subset(df_Factors, splitting == TRUE)
df_arvore_teste = subset(df_Factors, splitting == FALSE)

# Classificação
classifier <- rpart(formula = num ~ ., data = df_arvore_treinamento)
print(classifier)
rpart.plot(classifier)

# Previsão
prediction <- predict(classifier, newdata = df_arvore_teste[,-14], type = 'class')
print(prediction)

# Matriz de confusão
table_prediction <- table(df_arvore_teste[,14],prediction)
print(table_prediction)
confusionMatrix(table_prediction)
# Acurácia: 69%

### ---------------------------- ###


### Regressão Logística ###

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
# Acurácia: 87%

### ---------------------------- ###


### Redes Neurais ###

# Criando um outro dataset sem variáveis categóricas para testar um modelo de redes neurais
# Selecionando 4 variáveis relevantes ("age","sex","ca","thal")
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

# Previsão
prediction <- predict(nn, newdata = df_neuralnet_teste[,-5])
print(prediction)

# Matriz de confusão
table_prediction <- table(df_neuralnet_teste[,5], prediction > 0.5)
print(table_prediction)

accuracy_Test <- sum(diag(table_prediction)) / sum(table_prediction)
accuracy_Test
# Acurácia: 83%




### -------------  Carregando o dataset da Suíça   --------------- ###

# Carregando o dataset da Suíça
dfSuica <- read.csv(file='processed.switzerland.data', sep=',', header= FALSE, dec='.', na.strings = '?')

# Definindo cabeçalho para as variáveis relevantes do arquivo processado
names(dfSuica) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach","exang", "oldpeak",
                     "slope", "ca", "thal", "num")
summary(dfSuica)

# Deixando apenas as 3 primeiras variáveis que não tem valores NAs e o valor de diagnóstico
dfSuica <- dfSuica[,c(1,2,3,14)]

# Transformando os valores que definem os niveis de doença cardíaca para o valor 1, informando a presença de alguma
# doença ou não (valor 0)
dfSuica[dfSuica$num > 0,]$num <- 1


### Redes Neurais ###

# Criando um outro dataset sem variáveis categóricas para testar um modelo de redes neurais
# Selecionando 3 variáveis ("age","sex","cp")
dfReduzido <- df[,c(1,2,3,14)]
summary(dfReduzido)
# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'num'
splitting = sample.split(dfReduzido$num, SplitRatio = 0.7)
df_neuralnet_treinamento = subset(dfReduzido, splitting == TRUE)
df_neuralnet_teste = subset(dfReduzido, splitting == FALSE)

# Definindo a rede neural
nn <- neuralnet(formula = num ~ ., data = df_neuralnet_treinamento, hidden = 7, lifesign = "minimal", 
                linear.output = FALSE, threshold = 0.1)
plot(nn, rep = "best")

# Previsão
prediction <- predict(nn, newdata = df_neuralnet_teste[,-4])
print(prediction)

# Matriz de confusão
table_prediction <- table(df_neuralnet_teste[,4], prediction > 0.5)
print(table_prediction)

accuracy_Test <- sum(diag(table_prediction)) / sum(table_prediction)
accuracy_Test
# Acurácia: 70%


# Aplicando o dataset completo da Suiça no modelo de Redes Neurais para verificar o resultado
prediction <- predict(nn, newdata = dfSuica[,-4])
print(prediction)

# Matriz de confusão
table_prediction <- table(dfSuica[,4], prediction > 0.5)
print(table_prediction)

accuracy_Test <- sum(diag(table_prediction)) / sum(table_prediction)
accuracy_Test
# Acurácia: 82%