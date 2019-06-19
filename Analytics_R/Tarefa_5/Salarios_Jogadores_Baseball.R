# Tarefa 5 - Estudo sobre salários de jogadores de baseball

# -------------------------
# Instalação de Pacotes Externos
# -------------------------
#install.packages('ISLR')
#install.packages('ggplot2', dependencies=TRUE)

# -------------------------
# Carregando as LIBs
# -------------------------
library('ISLR')

# Verificando dataset, analisando as variáveis disponíveis
summary(Hitters)

str(Hitters)

boxplot(Hits~Salary,
        data=Hitters,
        main="Rebatidas em 1986 por Salário Anual",
        xlab="Salário",
        ylab="Rebatidas em 1986",
        col="orange",
        border="brown"
)

hist(Hitters$Salary)
