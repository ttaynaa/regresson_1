#Matriz de Desvios-padroes
D=matrix(c(sqrt(0.49),0,0,sqrt(1.64)),2,2)
D
#Matriz Inversa dos Desvios-padroes
(InvD =solve(D))

# Matriz de Covariancia Sigma
(MCov=matrix(c(0.49,-0.06,-0.06,1.64),2,2))

# Matriz de Correlacao

# lista 4 -------------------------------------------------------------------

# 2° Questão OOOOOOOO000000OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# Ler dados
library(datasets)
data(airquality)
str(airquality)

# Remover observações faltando = NA
# Selecionar só o mês de maio (airquality$Month==5)
# Selecionar só as 4 primeiras colunas
X=as.matrix(na.exclude(airquality[airquality$Month==5,1:4]))

#(a)
(xbar <- colMeans(X)) # Vetor de médias de x


# (b)
(Sx <- cov(X)) # Matriz de covariância de x


# (c)
cor(X) # Matriz de correlação


# (d)
a <- c(3,2,0,1)
(ybar <- t(a) %*% xbar) # médias de y
(Sy <- t(a) %*% Sx %*% a) # covariância de y


# (e)
A <- matrix(c(1,1,-1,1,1,0,1,1),2,4)
(wbar <- A %*% xbar) # vetor de médias de w
(Sw <- A %*% Sx %*% t(A)) # Matriz de covariância de w




# Questão 3°OO000000000OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

#a) matriz x
x = c(1,3,5,8,4,0)
x = matrix(data=x, nrow = 3) # nrow é linhas 
x

X <- matrix(c(1,3,5,8,4,0),3,2) 
X

#b) Calcule o vetor de médias x¯

xbar=colMeans(X) #(se der como matriz, se estiver como conjunta é de outro jeito)
xbar

#c)Calcule a matriz de covariância de x
cov_x= cov(X)
cov_x
#D) Calcule a matriz de correlação
cor_x = cor(X)


# E) Calcule a média e a variância y
# média:
#y = a^t x
a1 <- c(-2,1)
(ybar <- t(a1) %*% xbar) # médias de y
(Sy <- t(a1) %*% cov_x %*% a1) # covariância/variancia de y


# F) Calcule o vetor de médias e a matriz de covariância do vetor w
#Wbar = A*Xbar
xbar <- colMeans(X) # Vetor de medias de x
A = matrix(c(-2,0,1,1),2,2)
wbar= A %*% xbar # Vetor de medias de W
wbar


# Questão 4 0000000000000000000OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# Ler dados
library(car)
data(Anscombe)
str(Anscombe)
X=as.matrix(Anscombe)


#(a)
(xbar <- colMeans(X)) # Vetor de médias de x


# (b)
(Sx <- cov(X)) # Matriz de covariância de x


# (c)
cor(X) # Matriz de correlação


# (d)
a <- c(3,2,0,1)
(ybar <- t(a) %*% xbar) # médias de y
(Sy <- t(a) %*% Sx %*% a) # covariância de y


# (e)
A <- matrix(c(1,1,-1,1,1,0,1,1),2,4)
(wbar <- A %*% xbar) # vetor de médias de w
(Sw <- A %*% Sx %*% t(A)) # Matriz de covariância de w
det(Sw) # variância generalizada de w






# lista 5 ---------------------------------------------------------------------

# 1° Questão 00000000000000000000000000000000000000000000000000000000000000000 
#Considere os dados airquality do pacote datasets do R, sobre a qualidade do ar 
#em Nova Iorque, em maio de 1973. As seguintes variáveis serão consideradas: 
#Ozone (x1): ozônio (ppb); 
#Solar.R (x2): Radiação Solar (lang); 
#Wind (x3): velocidade do vento (mph);
#temp (x4): temperatura (graus F).

# Ler dados
library(datasets)
data(airquality)
str(airquality)

# Remover observações faltando = NA
# Selecionar só o mês de maio (airquality$Month==5)
# Selecionar só as 4 primeiras colunas
X=as.matrix(na.exclude(airquality[airquality$Month==5,1:4]))

# a) 
(Sx <- cov(X)) # Matriz de covariância de x

(det(Sx)) # variância generalizada


#b) # matriz de distância euclidiana de x
(D <- dist(X)) 
D

#c)
X = matrix(c(-1,0,1,1,5,3),3,2) # outroa matriz para teste 
(Sx <- cov(X)) 
Sx
(D <- dist(X))
D
# n=3 primeiras amostras
dm12 <- sqrt(t(X[1,]-X[2,]) %*% solve(Sx) %*% (X[1,]-X[2,]))
dm13 <- sqrt(t(X[1,]-X[3,]) %*% solve(Sx) %*% (X[1,]-X[3,]))
dm23 <- sqrt(t(X[2,]-X[3,]) %*% solve(Sx) %*% (X[2,]-X[3,]))
(m.D <- matrix(c(0,dm12,dm13,dm12,0,dm23,dm13,dm23,0),3,3))

# lista 7 -----------------------------------------------

#° Questão 1
install.packages("DescTools")
library(DescTools)
install.packages("HSAUR")
library(HSAUR)
agefat
X=as.matrix(agefat[,1:2]) #Cria a matriz de dados

#a) 
xbar <- colMeans(X) # Vetor de medias de x

#b) 
(Sx <- cov(X)) # Matriz de covariancia de x

#c)
cor(X) # Matriz de correlacao

#d)
a <- c(0.5,0.5)
(ybar <- t(a) %*% xbar) # medias de y
(Sy <- t(a) %*% Sx %*% a) # covariancia de y

# (e)
A <- matrix(c(1,-1,-1,1),2,2)
(wbar <- A %*% xbar) # vetor de medias de w
(Sw <- A %*% Sx %*% t(A)) # Matriz de covariancia de w

#f)
(Cov.G <- det(Sx)) # variância generalizada

# g)
(D <- dist(X)) # matriz de distância euclidiana

# h)
(invSx <- solve(Sx)) #inversa da matriz de covariância

# i) # distância de Mahalanobis (OUTRA FORMA DE FAZER para n= n (tudo))
Dm <- function(X){
  n <- nrow(X)
  S <- cov(X)
  Dmit <- matrix(0,n,n)
  for(i in 1:n){
    for(t in 1:n){
      Dmit[i,t] <- sqrt(t(X[i,]-X[t,]) %*% solve(S) %*% (X[i,]-X[t,]))
    }
  }
  return(Dmit)
}
Dm(X)

# j) TESTE DE HIPOTESE

# H0: mi = (25,180,12,70)^⊤  versus  mi != (25,180,12,70)^⊤ 
# ao nível de 5% de significância, supondo que x=(X1,X2,X3,X4)^⊤
# tem distribuição normal multivariada.

mu0 <- c(40,30)# H0: mu = mu0
library(DescTools)
HotellingsT2Test(X,mu=mu0,test = "f")
qf(1-0.01,df1=2,df2=23)
pf(8.1492,df1=2,df2=23,lower.tail=F)

#Resposta: 
# Hipóteses: H0:μ=(40,30)^⊤ versus H1:μ≠(40,30)^⊤
# Estatística: T21=8,1492
# Nível de significância: α=0,01, gl1=p=2, gl2=n−p=25−2=23 e 
# Fp,n−p(α)=F2,23(0,01)=qf(1-0.01,df1=2,df2=23)=5,664
# Região Crítica: Rejeita H0 se T1^2>F2,23(0,01)= 5,664
# Valor-p: P(Fp,n−p>T21)=P(F2,23>8,1492)=pf(8.1492,df1=2,df2=23,lower.tail=F)=0,0021
# Decisão: Como T21=8,1492>5,664, rejeitamos H0:μ=(40,30)^⊤



# (k)
mu0 <- c(40,30)# H0: mu = mu0
HotellingsT2Test(X,mu=mu0,test = "chi")

qchisq(1-0.01,df=2)
pchisq(17.007,df=2,lower.tail=F)



# Hipóteses: H0:μ=(40,30)^⊤ versus H1:μ≠(40,30)⊤
# Estatística: T2=17,007
# Nível de significância: α=0,01, gl=p=2 e χ2p(α)=χ22(0,01)=qchisq(1-0.01,df=2)= 9,21034.
# Região Crítica: Rejeita H0 se T2>χ22(0,01)=9,21034.
# Valor-p: P(χ2p>T2)=P(χ22>17,007)=pchisq(17.007,df=2,lower.tail=F)=0,0002.
# Decisão: Como T2=17,007>9,21034, rejeitamos H0:μ=(40,30)^⊤




#EXTRA ---------------------------------------
# Exemplo dos Graficos de dispersao
set.seed(123)
x <- rnorm(100)
e <- rnorm(100)
y1=2*x+e #(a) direta
y2=e #(b) nula
y3=-2*x+e #(c) indireta
y4=x^2+e #(d) não linear
install.packages("latex2exp")
library(latex2exp)
par(mfrow=c(1,4),col="#145D90",col.axis="#145D90",col.lab="#145D90",col.main="#145D90",fg="#145D90")
plot(x,y1,xlim=c(-3,3),ylim=c(-6,6),xlab=TeX('$x_1$'),ylab=TeX('$x_2$'),main = "(a) direta")
plot(x,y2,xlim=c(-3,3),ylim=c(-3,3),xlab=TeX('$x_1$'),ylab=TeX('$x_2$'),main = "(b) nula")
plot(x,y3,xlim=c(-3,3),ylim=c(-6,6),xlab=TeX('$x_1$'),ylab=TeX('$x_2$'),main = "(c) indireta")
plot(x,y4,xlim=c(-3,3),ylim=c(-3,6),xlab=TeX('$x_1$'),ylab=TeX('$x_2$'),main = "(d) não linear")

#gráfico utilizando o pacote scatterplot3d.

# Ler dados
library(car)
data(Anscombe)
str(Anscombe)
X=as.data.frame(Anscombe)

library(scatterplot3d)
#X1=education, X2=income, X3=young, X4=urban
x=X$education; y=X$income; z=X$urban
scatterplot3d(x, y, z, pch = 19, color ="#f06f00",
              xlab="Gasto com Educação",
              ylab="Renda",
              zlab ="Populacao")

# gráfico utilizando o pacote ggplot2.
library(ggplot2)
library(dplyr)
X %>%
  arrange(desc(urban)) %>% #ordenar por urban
  ggplot(aes(x=education, y=income, size=urban))+
  theme_bw()+
  geom_point(alpha=0.5)+
  scale_size(range = c(0, 10), name="População urbana")+
  ylab("Renda")+
  xlab("Gasto com Educação")

#gráfico utilizando o pacote GGally.
library(GGally)
ggpairs(X,upper = list(continuous = "density",combo = "box_no_facet",discrete = "facetbar", na = "na"),
        lower = list(continuous = "points",combo = "facetdensity",discrete = "facetbar", na ="na"),
        diag = list(continuous = "barDiag",discrete = "barDiag",na = "naDiag"),
        columnLabels = c("Educação","Renda","População Jovem","População Urbana"))+theme_bw()
