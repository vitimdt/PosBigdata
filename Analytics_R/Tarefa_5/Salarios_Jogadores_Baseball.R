# Tarefa 5 - Estudo sobre salários de jogadores de baseball

# -------------------------
# Instalação de Pacotes Externos
# -------------------------
#install.packages('ISLR')
#install.packages('hablar')
#install.packages('ggplot2', dependencies=TRUE)

# -------------------------
# Carregando as LIBs
# -------------------------
library('ISLR')
library('dplyr')
library('hablar')

# Verificando dataset, analisando as variáveis disponíveis
summary(Hitters)

str(Hitters)

nrow(na.omit(Hitters))

# Gerando um novo DataSet, retirando as linhas onde o salário está como NA
dsHitters <- na.omit(Hitters)

summary(dsHitters)


# --------------------------------------------------------------------
# Gerando gráficos para analisar a correlação entre as variáveis
# --------------------------------------------------------------------
# Histograma sobre o Salário anual dos jogadores
hist(dsHitters$Salary,
     main="Histograma dos Salários",
     xlab="Salário Anual",
     ylab="Frequência",
     col="orange",
     border="brown")


# Função para converter as variáveis Factor em valores Char
unfactorize <- function(df){
  for(i in which(sapply(df, class) == "factor")) df[[i]] = as.character(df[[i]])
  return(df)
}
# Aplicando no Dataframe
dsHittersSemFactor <- unfactorize(dsHitters)
str(dsHittersSemFactor)

# Criando variável númerica para Division (Divisão 'W' = 1 e Divisão 'E' = 2)
dsHittersSemFactor$NumDivision <- 0
dsHittersSemFactor[dsHittersSemFactor$Division == 'W',]$NumDivision <- 1
dsHittersSemFactor[dsHittersSemFactor$Division == 'E',]$NumDivision <- 2
str(dsHittersSemFactor)

# Gerando o gráfico BoxPlot para analisar a relação de Divisão x Salários dos Jogadores em 1986
boxplot(Salary~NumDivision,
        data=dsHittersSemFactor,
        main="Salários dos jogadores pela Divisão no final 1986",
        xlab="Divisão (1 = W e 2 = E)",
        ylab="Salário",
        col="orange",
        border="brown"
)

# Criando variável númerica para League (Liga 'N' = 1 e Liga 'A' = 2)
dsHittersSemFactor$NumLeague <- 0
dsHittersSemFactor[dsHittersSemFactor$League == 'N',]$NumLeague <- 1
dsHittersSemFactor[dsHittersSemFactor$League == 'A',]$NumLeague <- 2
str(dsHittersSemFactor)

# Gerando o gráfico BoxPlot para analisar a relação da Liga x Salários dos Jogadores em 1986
boxplot(Salary~NumLeague,
        data=dsHittersSemFactor,
        main="Salários dos jogadores pela Liga no final 1986",
        xlab="Liga (1 = N e 2 = A)",
        ylab="Salário",
        col="orange",
        border="brown"
)



# Fazendo mais uma matriz de scatterplots,
# incluindo a correlação no painel superior mas agora com proporcionalidade.
panel.corprop <- function(x, y, ...)
{
        par(usr = c(0, 1, 0, 1))
        txt <- as.character(format(cor(x, y), digits=2))
        text(0.5, 0.5, txt, cex = 6* abs(cor(x, y)))
}
pairs(iris[1:4], main="Iris Dataset com Correlações de Tamanho Proporcional", pch=21, 
      bg=c("red","green3","blue")[unclass(iris$Species)], upper.panel=panel.corprop)
