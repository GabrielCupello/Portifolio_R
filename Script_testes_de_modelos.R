# carregando os datasets
train <- read.csv(file="/bb_kaggle/train.csv")
test <- read.csv(file="/bb_kaggle/test.csv")
# criar a feature venda para poder juntar os datasets
test$venda <- 0
b <- rbind(train, test)
tail(b)

# criar as novas variáveis
b$fds <- ifelse(b$weekday=='sexta-feira'|b$weekday=='s?bado'|b$weekday=='domingo', 1,0)
b$fds2 <- ifelse(b$weekday=='s?bado'|b$weekday=='domingo', 1,0)
head(b)
b$margem_inter <- cut(b$margem, seq(0,0.60,length.out = 11), labels=c(1:10))
head(b)
install.packages("dummies")
library(dummies)
b <- cbind(b, dummy(b$mes))

b$fds <- as.factor(b$fds)
b$outdesc <- as.factor(b$outdesc)
b$outmg <- as.factor(b$outmg)
b$babril <- as.factor(b$babril)
b$bagosto <- as.factor(b$bagosto)
b$bdezembro <- as.factor(b$bdezembro)
b$bfevereiro <- as.factor(b$bfevereiro)
b$bjaneiro <- as.factor(b$bjaneiro)
b$bjulho <- as.factor(b$bjulho)
b$bjunho <- as.factor(b$bjunho)
b$bmaio <- as.factor(b$bmaio)
b$`bmar?o` <- as.factor(b$`bmar?o`)
b$bnovembro <- as.factor(b$bnovembro)
b$boutubro <- as.factor(b$boutubro)
b$bsetembro <- as.factor(b$bsetembro)

# dividindo novamente os datasets
treino_2 <- b[1:365,]
head(treino_2)

teste_2 <- b[366:396,]
head(teste_2)
# removendo a variavel venda
teste_2 <- subset(teste_2, select = -c(venda))


# testes de modelos
mod <- lm(venda~desconto+fds+outdesc+bagosto+bjulho+boutubro+bsetembro+bjunho+date, data = treino_2)
summary(mod)
p <- predict(mod, newdata=teste_2)
p <- as.data.frame(cbind(1:31,p))
names(p)<-c("id","venda")
write.csv(p, file="predict.csv", row.names=FALSE)

install.packages('e1071')
library(e1071)
mod <- svm(venda~desconto+fds+outdesc+bagosto+bjulho+boutubro+bsetembro+bjunho+date, data = treino_2)
summary(mod)
p <- predict(mod, newdata=teste_2)
p <- as.data.frame(cbind(1:31,p))
names(p)<-c("id","venda")
write.csv(p, file="predict.csv", row.names=FALSE)


b <- cbind(b, dummy(b$margem_inter))
mod <- lm(venda~desconto+fds+outdesc+bjulho+bsetembro+bjunho+bnovembro+bjulho+date+b13+b12+b16+b17, data = treino_2)
summary(mod)
p <- predict(mod, newdata=teste_2)
p <- as.data.frame(cbind(1:31,p))
names(p)<-c("id","venda")
write.csv(p, file="predict.csv", row.names=FALSE)

mod <- svm(venda~desconto+fds+outdesc+bjulho+bsetembro+bjunho+bnovembro+bjulho+date+b13+b12+b16+b17, data = treino_2)
summary(mod)
p <- predict(mod, newdata=teste_2)
p <- as.data.frame(cbind(1:31,p))
names(p)<-c("id","venda")
write.csv(p, file="predict.csv", row.names=FALSE)


tail(treino_2)
cor(treino_2$venda, treino_2$margem)
treino_2[treino_2$desconto == max(treino_2$desconto),]$desconto <- mean(treino_2[treino_2$ano == 2014 & treino_2$mes == 'novembro',]$desconto)
max(treino_2$desconto)
treino_2$ano <- substr(treino_2$date,1,4)
treino_2[treino_2$ano == 2014 & treino_2$mes == 'novembro',]


mod <- lm(venda~desconto+fds+fds2+outdesc+bjulho+bsetembro+bjunho+bnovembro+bjulho+date+b13+b12+b16+b17, data = treino_2)
summary(mod)
p <- predict(mod, newdata=teste_2)
p <- as.data.frame(cbind(1:31,p))
names(p)<-c("id","venda")
write.csv(p, file="predict.csv", row.names=FALSE)

plot(treino_2$venda~treino_2$desconto)
mod <- svm(venda~desconto+fds+outdesc+bjulho+bsetembro+bjunho+bnovembro+bjulho+date+b6+b7, data = treino_2)
summary(mod)
p <- predict(mod, newdata=teste_2)
p <- as.data.frame(cbind(1:31,p))
names(p)<-c("id","venda")
write.csv(p, file="predict.csv", row.names=FALSE)
head(treino_2)
