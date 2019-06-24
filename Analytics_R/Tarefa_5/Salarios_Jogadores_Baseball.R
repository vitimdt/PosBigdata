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


# Matriz de scatterplots para verificar a correlação do Salário com as outras variáveis
# Função para informar o indice de correlação no painel superior
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
}
# Correlação das variáveis (Salary, AtBat, Hits, HmRun, Runs, RBI)
pairs(~ Salary + AtBat + Hits + HmRun + Runs + RBI, 
      data=dsHittersSemFactor, 
      main="Correlação do Salário com outras variáveis (Salary, AtBat, Hits, HmRun, Runs, RBI)", 
      pch=21, bg=c("green3"), upper.panel=panel.pearson)

# Correlação das variáveis (Salary, Walks, Years, CAtBat, CHits, CHmRun)
pairs(~ Salary + Walks + Years + CAtBat + CHits + CHmRun, 
      data=dsHittersSemFactor, 
      main="Correlação do Salário com outras variáveis (Salary, Walks, Years, CAtBat, CHits, CHmRun)", 
      pch=21, bg=c("green3"), upper.panel=panel.pearson)

# Correlação das variáveis (Salary, CRuns, CRBI, CWalks, PutOuts, Assists, Errors)
pairs(~ Salary + CRuns + CRBI + CWalks + PutOuts + Assists + Errors, 
      data=dsHittersSemFactor, 
      main="Correlação do Salário com outras variáveis (Salary, CRuns, CRBI, CWalks, PutOuts, Assists, Errors)", 
      pch=21, bg=c("green3"), upper.panel=panel.pearson)


# GPlot das váriaveis com CRuns e CRBI pelo Salário, separando por Divisão
library(ggplot2)
qplot(Salary, CRuns, data=dsHittersSemFactor, facets=Division ~.)
qplot(Salary, CRBI, data=dsHittersSemFactor, facets=Division ~.)
