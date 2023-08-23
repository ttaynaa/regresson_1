


# 1° prova 

# 1° questão 

X = matrix(c(0,1,0,1),2,2)
# a) vetor de médias
(xbar <- colMeans(X))

(cov(X))


a1 <- c(1,1)
(xbar = matrix(c(1,1),2,1))
(cov_x = matrix(c(4,1,1,4),2,2))
(ybar <- t(a1) %*% xbar) # médias de y
(Sy <- t(a1) %*% cov_x %*% a1) # covariância/variancia de y








# 3° Questão

#teste de hipoteses

n <- 25
xbarra <- matrix(c(1, 1), 2, 1)
mu <- matrix(c(0, 0), 2, 1)
S <- matrix(c(4,3, 3, 6), 2, 2)

T2 <- n * t(xbarra - mu) %*% solve(S) %*% (xbarra - mu)

p <- 2
alpha <- 0.01
F <- ((n - 1) * p) / (n - p) * qf(1 - alpha, p, n - p)
p_value <- 1 - pt(T2, p, n - p)
p_value

if (T2 > F) {
  decision <- "Rejeitar H0"
} else {
  decision <- "Não rejeitar H0"
}

print(paste("Valor da estatística de teste T²:", T2))
print(paste("Valor crítico F:", F))
print(paste("Decisão:", decision))


#intervalo de confiança

n <- 25
xbarra <- matrix(c(1, 1), 2, 1)
mu <- matrix(c(0, 0), 2, 1)
S <- matrix(c(4,3, 3, 6), 2, 2)

p <- 2
alpha <- 0.01
t_value <- qt(1 - alpha/2, n - p - 1)

interval_lower <- xbarra - t_value * sqrt((p * (n - 1)) / (n * (n - p))) * sqrt(diag(S) / n)
interval_upper <- xbarra + t_value * sqrt((p * (n - 1)) / (n * (n - p))) * sqrt(diag(S) / n)

print(paste("Intervalo de confiança (99%): [", interval_lower[1], ",", interval_upper[1], "]"))
print(paste("Intervalo de confiança (99%): [", interval_lower[2], ",", interval_upper[2], "]"))
