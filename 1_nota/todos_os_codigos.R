



############ Exemplo- Correlacao ##################

####################################
## exemplo da octanagem da gasolina (Y) e da adicao de um novo aditivo (X)
####################################

octanagem<-c(80.5, 81.6, 82.1, 83.7, 83.9, 85.0)
aditivo<-c(1, 2, 3, 4, 5, 6)

# grafico de dispersao
plot(octanagem~aditivo) #octanagem em função do aditivo 

#calculo da correlacao
cor(octanagem,aditivo)

#teste  da correlacao
cor.test(octanagem,aditivo, method = "pearson", alternative="two.sided") #há correlação
cor.test(octanagem,aditivo, method = "pearson", alternative="less") # há correlação negativa
cor.test(octanagem,aditivo, method = "pearson", alternative="great") # há correlação positiva


############ Exemplo - Ajustando os parametros  ##################

####################################
## exemplo da octanagem da gasolina (Y) e da adicao de um novo aditivo (X)
####################################

octanagem<-c(80.5, 81.6, 82.1, 83.7, 83.9, 85.0)
aditivo<-c(1, 2, 3, 4, 5, 6)

# grafico de dispersao
plot(octanagem~aditivo) #aditivo explica octanagem

#ajuste do modelo
fit<-lm(octanagem~aditivo) 
abline(fit) # coloca a reta no gráfico

#"Resumo" do ajuste
summary(fit)
names(fit)
names(summary(fit))
fit


#parametros \alpha e \beta
coef(fit)
fit$coefficients
fit

## Estimador de sigma 2 
fit$residuals
sum(fit$residuals^2)/(length(fit$residuals)-2)
sqrt(sum(fit$residuals^2)/(length(fit$residuals)-2))

summary(fit)$sigma # estimativa do erro padrão do modelo ajustado
summary(fit)$sigma^2

############ Aula 4 - Inferência sob os parâmetros ##################

####################################
## exemplo da octanagem da gasolina (Y) e da adicao de um novo aditivo (X)
####################################

octanagem<-c(80.5, 81.6, 82.1, 83.7, 83.9, 85.0)
aditivo<-c(1, 2, 3, 4, 5, 6)

# grafico de dispersao
plot(octanagem~aditivo)
#ajuste do modelo
fit<-lm(octanagem~aditivo)
abline(fit)

## intervalos de confianca
confint(fit)
confint(fit, level = 0.90)
confint(fit, level = 0.99)

## teste de hipoteses
summary(fit)

## Intervalos de predicao
predict(fit,newdata=data.frame(aditivo=5.5), level = 0.90, interval="confidence") ## valor esperado
predict(fit,newdata=data.frame(aditivo=5.5), level = 0.90, interval="prediction") ## valor individual

## grafico do IC para o valor esperado
newx<-seq(1,6)
prd<-predict(fit,newdata=data.frame(aditivo=newx), level = 0.90, interval = "confidence")
lines(newx,prd[,2],col="green",lty=2,lwd=2)
lines(newx,prd[,3],col="green",lty=2,lwd=2)

## grafico do IC para o valor individual
newx<-seq(1,6)
prd<-predict(fit,newdata=data.frame(aditivo=newx), level = 0.90, interval = "prediction")
lines(newx,prd[,2],col="blue",lty=2,lwd=2)
lines(newx,prd[,3],col="blue",lty=2,lwd=2)
prd


############ Aula 5 - ANOVA e adequação do modelo ##################

####################################
## exemplo da octanagem da gasolina (Y) e da adicao de um novo aditivo (X)
####################################

# se os dados tiver em paconte no R
#MPV é o pacote que tem o banco
dados <- MPV::table.b3 # table é banco deDados

octanagem<-c(80.5, 81.6, 82.1, 83.7, 83.9, 85.0)
aditivo<-c(1, 2, 3, 4, 5, 6)

# grafico de dispersao
plot(octanagem~aditivo) # octanagem em função de aditivo
#ajuste do modelo
fit<-lm(octanagem~aditivo)
abline(fit)

## Analise de Variancia -- ANOVA
anova(fit) # Teste F
summary(fit)  # Teste t


time<-c(10.15,2.96,3.00,6.88,0.28,5.06,9.14,11.86,11.69,6.04,7.57,1.74,9.38,0.16,1.84)
casesStocked<-c(25,6,8,17,2,13,23,30,28,14,19,4,24,1,5)

df<-data.frame(time,casesStocked)

#ajuste do modelo
fit<-lm(time~casesStocked)
fit.o<-lm(time~-1+casesStocked)

summary(fit)
summary(fit.o)

# grafico de dispersao
plot(time~casesStocked)
abline(fit, col="red")
abline(fit.o,col="blue")


#Obtendo as quantidades para o gráfico usando o ggplot
coefs<- fit$coefficients
coefs <- as.data.frame(coefs)

coefs.o<- fit.o$coefficients
coefs.o <- as.data.frame(coefs.o)

library(ggplot2)
#Obtendo o gráfico com ggplot
ggplot(data = df, mapping = aes(x = casesStocked, y = time))+
  geom_point()+
  geom_abline(aes(intercept = coefs[1], slope = coefs[2]), data = coefs, color="red")+
  geom_abline(aes(intercept = 0, slope = coefs.o[1]),data = coefs.o, color="red")

