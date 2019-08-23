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

df <- read.csv(file='processed.cleveland.data', sep=',', header= FALSE, dec='.', na.strings = '?')
#View(df)
names(df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
summary(df)

df <- na.omit(df)

df[df$num > 0,]$num <- 1

ggcorr(df, palette = "RdBu", label = TRUE)

types <- c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","factor")

df_Factors <- convert_types(df, types)
str(df_Factors)


boxplot(df$ca ~ df$num,
        main="Predição por Fluoroscopia",
        ylab="Ca",xlab="Heart disease")

boxplot(df$age ~ df$num,
        main="Predição por Idade",
        ylab="Age",xlab="Heart disease")

df_Factors$sexNew <- factor(df_Factors$sex, labels=c('Mulher', 'Homem'), levels = c(0,1))
ggplot(df_Factors, aes(x = sexNew, fill = num)) +
  geom_histogram(stat="count", color ='black')  + 
  xlab("Sexo") + ylab("Frequência") + ggtitle("Histograma de Predição por Sexo") +
  theme_classic()


df_Factors <- df_Factors[,-15]
# Divide o dataframe em treinamento e teste, com taxa de 70% na coluna 'num'
splitting = sample.split(df_Factors$num, SplitRatio = 0.7)
df_naivebayes_treinamento = subset(df_Factors, splitting == TRUE)
df_naivebayes_teste = subset(df_Factors, splitting == FALSE)

# Classificação (x: dataframe de treinamento sem a coluna 'num' / y: coluna 'num' do dataframe de treinamento)
classifier <- naiveBayes(x = df_naivebayes_treinamento[,-14], y = df_naivebayes_treinamento$num )
print(classifier)

# Previsão
prediction <- predict(classifier, newdata = df_naivebayes_teste[-14])

# Tabela de previsões (coluna 'num' do dataframe de teste)
table_prediction <- table(df_naivebayes_teste[,14],prediction)
print(table_prediction)

# Matriz de confusão
confusionMatrix(table_prediction)
