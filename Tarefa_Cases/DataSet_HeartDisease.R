#install.packages('ggplot2', dependencies = TRUE)
#install.packages('GGally', dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)

library('ggplot2')
library('GGally')
library('caret')
library('caTools')
library('e1071')
library('caret')

convert_types <- function(obj,types){
  out <- lapply(1:length(obj),FUN = function(i){FUN1 <- 
    switch(types[i],
           character = as.character,
           numeric = as.numeric,
           factor = as.factor); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  as.data.frame(out)
}

# Carrgando o dataset
df <- read.csv(file='processed.cleveland.data', sep=',', header= FALSE, dec='.', na.strings = '?')

# Definindo cabeçalho para as variáveis relevantes do arquivo processado
names(df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
summary(df)

# Removendo os registros onde alguma váriavel possui valores NAs
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


# Testando um modelo com algoritmo de Naive Bayes para fazer a prediçao
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


