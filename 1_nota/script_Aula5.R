############ Aula 5 - ANOVA e adequação do modelo ##################

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

## Analise de Variancia -- ANOVA
anova(fit) # Teste F
summary(fit)  # Teste t
