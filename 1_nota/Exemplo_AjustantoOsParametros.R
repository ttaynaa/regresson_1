############ Exemplo - Ajustando os parametros  ##################

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

#"Resumo" do ajuste
summary(fit)
names(fit)
names(summary(fit))

#parametros \alpha e \beta
coef(fit)
fit$coefficients
fit

## Estimador de sigma 2 
fit$residuals
sum(fit$residuals^2)/(length(fit$residuals)-2)
sqrt(sum(fit$residuals^2)/(length(fit$residuals)-2))

summary(fit)$sigma
summary(fit)$sigma^2

