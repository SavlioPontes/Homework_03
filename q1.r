setwd("C:/Users/savli/Documents/Programação/R")

# ----------- QUESTÃO 1 ----------- #
# Considera-se que o tempo de vida segue uma distribuição exponencial com taxa λ > 0

# 1.1) PDF da distribuição exponencial
exp_pdf <- function(x, lambda){
  return(lambda * exp(-lambda * x))
}

# 1.2(a) Função de verossimilhança(likelihood)
likelihood_exp <- function(x, lambda) {
  return(prod(exp_pdf(x, lambda)))
}

# 1.2(b) Função log-verossimilhança(log likelihood)
loglike_exp <- function(x, lambda) {
  n <- length(x)
  return(n * log(lambda) - lambda * sum(x))
}

# 1.2(c) Estimador de Máxima Verossimilhança
# Para a exponencial, o MLE é o inverso da média amostral
mle_exp <- function(x){
  return(1 / mean(x))
}

# 1.3) Dados da amostra
d <- c(
  0.99, 2.31, 10.85, 6.15, 10.81, 3.72, 5.75, 4.15, 9.27, 7.84,
  2.31, 10.85, 6.15, 1.81, 3.72, 5.75, 10.40, 10.04, 4.15, 9.27
)

# Cálculo do estimador de máxima verossimilhança
estimador <- mle_exp(d)

# 1.4) Gráfico da função log-verossimilhança

# Valores de lambda analisados
lambda_vals <- seq(0.01, 1, length.out = 300)

# Cálculo da log-verossimilhança para cada valor de λ
loglike_vals <- sapply(lambda_vals, loglike_exp, x = d)

# Gráfico da log-verossimilhança em função de λ
plot(lambda_vals, loglike_vals, type = "l",
     xlab = expression(lambda),
     ylab = expression(ell(lambda)))

# Linha vertical indicando o valor do MLE
abline(v = estimador, col = "red", lwd = 2, lty = 2)

# Legenda do gráfico
legend("topright",
       legend = expression(hat(lambda)),
       col = "red",
       lty = 2,
       lwd = 2)

# 1.5(a) Tempo médio de vida estimado
# Para a exponencial, a expectativa é 1/λ
expectativa_exp <- function(lambda){
  1 / lambda
}
tempomedio <- expectativa_exp(estimador) #para o tempo medio estimado,usamos o estimador

  #  1.5(b) Probabilidade de o computador durar mais de 5 anos,ou seja, P(X > 5)
  prob5b <- exp(-estimador * 5)
