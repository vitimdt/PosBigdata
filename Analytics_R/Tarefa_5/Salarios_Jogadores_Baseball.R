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
     xlab="Salário Anual",
     ylab="Frequência",
     col="orange",
     border="brown")


# Convertendo as variáveis Factor
#unfactorize <- function(df){
#for(i in which(sapply(df, class) == "factor")) df[[i]] = as.character(df[[i]])
#return(df)
#}

#library(dplyr)
#library(hablar)

#df <- tibble(a = factor(c(1, 2, 3, 4)),
#             b = factor(c(5, 6, 7, 8)))

#df %>% convert(chr(a:b))
#  a     b    
#<chr> <chr>
#1 1     5    
#2 2     6    
#3 3     7    
#4 4     8  
boxplot(Hits~Salary,
        data=dsHitters,
        main="Rebatidas em 1986 por Salário Anual",
        xlab="Salário",
        ylab="Rebatidas em 1986",
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
