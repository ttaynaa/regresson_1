############ Exemplo- Correlacao ##################

####################################
## exemplo da octanagem da gasolina (Y) e da adicao de um novo aditivo (X)
####################################

octanagem<-c(80.5, 81.6, 82.1, 83.7, 83.9, 85.0)
aditivo<-c(1, 2, 3, 4, 5, 6)

# grafico de dispersao
plot(octanagem~aditivo)

#calculo da correlacao
cor(octanagem,aditivo)

#teste  da correlacao
cor.test(octanagem,aditivo, method = "pearson", alternative="two.sided")
cor.test(octanagem,aditivo, method = "pearson", alternative="less")
cor.test(octanagem,aditivo, method = "pearson", alternative="great")
