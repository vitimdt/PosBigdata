# Tarefa 5 - Estudo sobre sal�rios de jogadores de baseball (DataSet Hitters)

# -------------------------
# Instala��o de Pacotes Externos
# -------------------------
#install.packages('ISLR')
#install.packages('ggplot2', dependencies=TRUE)

# -------------------------
# Carregando as LIBs
# -------------------------
library('ISLR')
library(ggplot2)

# Verificando dataset, analisando as vari�veis dispon�veis
summary(Hitters)

str(Hitters)

nrow(na.omit(Hitters))

# Gerando um novo DataSet, retirando as linhas onde o sal�rio est� como NA
dsHitters <- na.omit(Hitters)

summary(dsHitters)
# Achamos melhor retirar os registros com Sal�rios faltantes, pois n�o encontramos nenhuma rela��o clara
# que justifique realizar algum preenchimento aplicando alguma regra gen�rica.


# --------------------------------------------------------------------
# Gerando gr�ficos para analisar a correla��o entre as vari�veis
# --------------------------------------------------------------------
# Histograma sobre o Sal�rio anual dos jogadores
hist(dsHitters$Salary,
     main="Histograma dos Sal�rios",
     xlab="Sal�rio Anual",
     ylab="Frequ�ncia",
     col="orange",
     border="brown")


# Fun��o para converter as vari�veis Factor em valores Char
unfactorize <- function(df){
  for(i in which(sapply(df, class) == "factor")) df[[i]] = as.character(df[[i]])
  return(df)
}
# Aplicando no Dataframe
dsHittersSemFactor <- unfactorize(dsHitters)
str(dsHittersSemFactor)

# Criando vari�vel n�merica para Division (Divis�o 'W' = 1 e Divis�o 'E' = 2)
dsHittersSemFactor$NumDivision <- 0
dsHittersSemFactor[dsHittersSemFactor$Division == 'W',]$NumDivision <- 1
dsHittersSemFactor[dsHittersSemFactor$Division == 'E',]$NumDivision <- 2
str(dsHittersSemFactor)

# Gerando o gr�fico BoxPlot para analisar a rela��o de Divis�o x Sal�rios dos Jogadores em 1986
boxplot(Salary~NumDivision,
        data=dsHittersSemFactor,
        main="Sal�rios dos jogadores pela Divis�o no final 1986",
        xlab="Divis�o (1 = W e 2 = E)",
        ylab="Sal�rio",
        col="orange",
        border="brown"
)
# Qual divis�o apresenta os maiores sal�rios? Resposta: Divis�o E (Leste)

# Somando os sal�rios por Divis�o
filtro <- with(dsHittersSemFactor, Division == 'W')
dsDivisaoW <- dsHittersSemFactor[filtro,]
filtro <- with(dsHittersSemFactor, Division == 'E')
dsDivisaoE <- dsHittersSemFactor[filtro,]
# Soma dos sal�rios de jogadores da Divis�o W.
sum(dsDivisaoW$Salary)
# Resultado = 60417.5
# Soma dos sal�rios de jogadores da Divis�o E.
sum(dsDivisaoE$Salary)
# Resultado = 80531.01


# Criando vari�vel n�merica para League (Liga 'N' = 1 e Liga 'A' = 2)
dsHittersSemFactor$NumLeague <- 0
dsHittersSemFactor[dsHittersSemFactor$League == 'N',]$NumLeague <- 1
dsHittersSemFactor[dsHittersSemFactor$League == 'A',]$NumLeague <- 2
str(dsHittersSemFactor)

# Gerando o gr�fico BoxPlot para analisar a rela��o da Liga x Sal�rios dos Jogadores em 1986
boxplot(Salary~NumLeague,
        data=dsHittersSemFactor,
        main="Sal�rios dos jogadores pela Liga no final 1986",
        xlab="Liga (1 = N e 2 = A)",
        ylab="Sal�rio",
        col="orange",
        border="brown"
)
# Qual Liga apresenta os maiores sal�rios? Resposta: Liga A (Americana)

# Somando os sal�rios por Liga
filtro <- with(dsHittersSemFactor, League == 'A')
dsLigaA <- dsHittersSemFactor[filtro,]
filtro <- with(dsHittersSemFactor, League == 'N')
dsLigaN <- dsHittersSemFactor[filtro,]
# Soma dos sal�rios de jogadores da Liga A.
sum(dsLigaA$Salary)
# Resultado = 75337.94
# Soma dos sal�rios de jogadores da Liga N.
sum(dsLigaN$Salary)
# Resultado = 65610.57


# Matriz de scatterplots para verificar a correla��o do Sal�rio com as outras vari�veis
# Fun��o para informar o indice de correla��o no painel superior
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
}
# Correla��o das vari�veis (Salary, AtBat, Hits, HmRun, Runs, RBI)
pairs(~ Salary + AtBat + Hits + HmRun + Runs + RBI, 
      data=dsHittersSemFactor, 
      main="Correla��o do Sal�rio com outras vari�veis (Salary, AtBat, Hits, HmRun, Runs, RBI)", 
      pch=21, bg=c("green3"), upper.panel=panel.pearson)

# Correla��o das vari�veis (Salary, Walks, Years, CAtBat, CHits, CHmRun)
pairs(~ Salary + Walks + Years + CAtBat + CHits + CHmRun, 
      data=dsHittersSemFactor, 
      main="Correla��o do Sal�rio com outras vari�veis (Salary, Walks, Years, CAtBat, CHits, CHmRun)", 
      pch=21, bg=c("green3"), upper.panel=panel.pearson)

# Correla��o das vari�veis (Salary, CRuns, CRBI, CWalks, PutOuts, Assists, Errors)
pairs(~ Salary + CRuns + CRBI + CWalks + PutOuts + Assists + Errors, 
      data=dsHittersSemFactor, 
      main="Correla��o do Sal�rio com outras vari�veis (Salary, CRuns, CRBI, CWalks, PutOuts, Assists, Errors)", 
      pch=21, bg=c("green3"), upper.panel=panel.pearson)

# Quais vari�veis quantitativas apresentam maior correla��o com o sal�rio? 
# Resp.: As vari�veis relacionadas a carreira dos jogadores: CAtBat, CHits, CHmRun, CRuns, CRBI


# GPlot das v�riaveis com CRuns e CRBI pelo Sal�rio, separando por Divis�o
qplot(Salary, CRuns, data=dsHittersSemFactor, facets=Division ~.)
qplot(Salary, CRBI, data=dsHittersSemFactor, facets=Division ~.)

# Criando uma nova coluna somando todas os indicadores positivos - os indicadores negativos do ano de 1986
dsHittersSemFactor$totalInd <- with(dsHittersSemFactor, 
                                    (AtBat+Hits+HmRun+Runs+RBI+Walks+Assists)-(PutOuts+Errors))
# Criando uma nova coluna somando todas os indicadores positivos da carreira dos jogadores
dsHittersSemFactor$totalIndCareer <- with(dsHittersSemFactor, (CAtBat+CHits+CHmRun+CRuns+CRBI+CWalks))

# GPlot das vari�veis calculadas com indicadores pelo Sal�rio
qplot(Salary, totalInd, data=dsHittersSemFactor, facets=Division ~.)
qplot(Salary, totalIndCareer, data=dsHittersSemFactor, facets=Division ~.)
qplot(Salary, totalInd, data=dsHittersSemFactor, facets=League ~.)
qplot(Salary, totalIndCareer, data=dsHittersSemFactor, facets=League ~.)

# Criando uma coluna com a marca��o dos jogadores que mudaram de liga para temporada de 1987
dsHittersSemFactor$mudouLiga <- ifelse(dsHittersSemFactor$League == dsHittersSemFactor$NewLeague, 0, 1)
str(dsHittersSemFactor)

# Gerando o gr�fico BoxPlot para analisar a rela��o Sal�rios x Jogadores que mudaram de Liga
boxplot(Salary~mudouLiga,
        data=dsHittersSemFactor,
        main="Sal�rios dos jogadores X Mudaram de Liga?",
        xlab="Mudou de Liga?",
        ylab="Sal�rio",
        col="orange",
        border="brown"
)

# Apresente suas conclus�es, relacionadas � quest�o da influ�ncia dos fatores apresentados no sal�rio.
# Resp.: Apesar de n�o ter nenhuma vari�vel com uma forte correla��o com os Sal�rios dos jogadores,
# as vari�veis com indicadores da carreira dos jogadores s�o os que mais influenciam os Sal�rios. Outro
# ponto que verificamos � que apenas jogadores com sal�rios abaixo de 1000 milhares anuais que trocaram de
# Liga para o ano 1987.