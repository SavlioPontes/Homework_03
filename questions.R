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
# ----------- QUESTÃO 2 ----------- #
penguins_data <-na.omit(penguins) # desconsiderando os dados faltantes

# 2.1) Gráfico de dispersão entre x (massa) and y (comprimento do bico)
x <- penguins_data$body_mass_g
y <- penguins_data$bill_length_mm

ggplot(penguins_data, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point() +
  labs(title = "",
       x = "Massa Corporal (g)",
       y = "Comprimento do Bico (mm)")

# 2.2) Regressão linear

#Método dos mínimos quadrados
dados <- data.frame(x, y)
n <- length(x)
beta1 <- (n * sum(x*y) - sum(x) * sum(y)) / (n * sum(x^2) - (sum(x))^2)
beta0 <- mean(y) - beta1 * mean(x)

mmq <- c(beta0, beta1)

# Função Lm
modelo <- lm(y ~ x, data = dados)
funcao_lm <- as.numeric(coef(modelo)) 

#Comparação dos resultados
tabela <- data.frame(
  Metodo = c("MMQ", "Comando lm()"),
  Intercepto = c(mmq[1], funcao_lm[1]),
  Inclinação = c(mmq[2], funcao_lm[2])
)

print(tabela)

summary(modelo) #mostra informações geradas por lm()

#Gráfico com reta de regressão
ggplot(penguins_data, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "",
       x = "Massa Corporal (g)",
       y = "Comprimento do Bico (mm)")

# 2.3) 1.Resíduos
resíduos <- modelo$residuals

#Gráfico para visualização
plot(penguins_data$body_mass_g, resíduos,
     xlab = "Massa Corporal (g)",
     ylab = "Resíduos",
     main = "",
     pch = 16)

abline(h = 0, lwd = 2)

#Cálculo do RSME
cat('Raiz do erro quadrático médio:', "\n")
rmse
rmse <- sqrt(mean(resíduos^2))

#Cálculo do R^2
cat("Coeficiente de determinação:", "\n")
summary(modelo)$r.squared

# 2.4) Introdução de um outlier

# Criando um ponto extremo
penguins_outlier <- rbind(
  penguins_data ,
  data.frame(
    species = "Adelie",
    island = "Torgersen",
    bill_length_mm = 45,   # valor típico
    bill_depth_mm = 19,
    flipper_length_mm = 190,
    body_mass_g = 8000,    # valor muito alto
    sex = "male",
    year = 2007
  )
)

modelo_outlier <- lm(bill_length_mm ~ body_mass_g,
                     data = penguins_outlier)

summary(modelo_outlier)

# novos valores para RSME e R^2
rmse_outlier <- sqrt(mean(residuals(modelo_outlier)^2))
r2_outlier <- summary(modelo_outlier)$r.squared
r2_original <- summary(modelo)$r.squared

#comparando coeficientes
coef_original <- coef(modelo)
coef_outlier <- coef(modelo_outlier)

tabela_comparacao <- data.frame(
  Modelo = c("Original", "Com outlier"),
  Intercepto = c(coef_original[1], coef_outlier[1]),
  Inclinação = c(coef_original[2], coef_outlier[2]),
  RMSE = c(rmse, rmse_outlier),
  R2 = c(r2_original, r2_outlier)
)

tabela_comparacao

# comparação entre as retas de regressão
plot(penguins_outlier$body_mass_g,
     penguins_outlier$bill_length_mm,
     xlab = "Massa Corporal (g)",
     ylab = "Comprimento do Bico (mm)",
     main = "",
     pch = 16,
     col = "black")

# Reta do modelo ORIGINAL
abline(modelo,
       col = "blue",
       lwd = 2)

# Reta do modelo COM OUTLIER
abline(modelo_outlier,
       col = "red",
       lwd = 2)

# Legenda
legend("topleft",
       legend = c("Modelo original", "Modelo com outlier"),
       col = c("blue", "red"),
       lwd = 2,
       bty = "n")
