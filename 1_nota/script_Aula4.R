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

