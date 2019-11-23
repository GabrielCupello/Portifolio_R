
# Exercício de amostragem
install.packages("ggplot2")
library(ggplot2)
d <- diamonds

# Geração de amosrtras
a1 <- d[1:3000,] # o espaço em branco após a virgula indica que todos os itens serão pegos

# Cálculo da média da var price da população e da amostra
mean(d$price) # para pegar uma variável dentro do dataframe utiliza-se $
mean(a1$price)

# Cálculo da mediana da var price da população e da amostra
median(d$price)
median(a1$price)

# Cálculo do descio padrão da var price da população e da amostra
sd(d$price)
sd(a1$price)

# Pegar nova amostra
a2 <- d[3001:6000,]
mean(d$price) 
mean(a1$price)
mean(a2$price)
median(d$price)
median(a1$price)
median(a2$price)
sd(d$price)
sd(a1$price)
sd(a2$price)

# Geração de amostra aleatória
sample(3) #gerar números aleatórios
# definição da semente de elatoriedade (manter o mesmo conjunto de dados)
set.seed(33)
sample(3)

set.seed(33)
# geração de vetor de números aleatórios
vn <- sample(53940)
#geração de amostra aleatória
a3 <- d[vn[1:3000],]
# análise dos indicadores
mean(a3$price)
median(a3$price)
sd(a3$price)

#Histograma
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Boxplot
# simbolo ~ representa "em relação a"
# gráfico de preços em relação ao corte do diamante
boxplot(price~cut, data=d)

# comando summary apresenta os elementos principais do dataset
summary(d$price)
summary(a3$price)

# comando para criar duas colunas no frame de visualização
# ideal para colocar gráficos lado a lado de datasets diferentes
par(mfrow=(c(1,2))) # para voltar ao normal par(mfrow=(c(1,1)))
boxplot(d$price)
boxplot(a3$price)

# Gráfico de dispersão
m <- mtcars
plot(m$mpg~m$wt)
# cálculo do coeficiente de correlação linear
# positivo = relação direta
# negativo = relação inversa
# quanto mais perto de 1 (e -1) mais se parece com uma reta
cor(m$mpg, m$wt)

plot(m$mpg~m$hp)
cor(m$mpg, m$hp)

# relação de todas as variáveis em relação a todas a variaveis
cor(m)
