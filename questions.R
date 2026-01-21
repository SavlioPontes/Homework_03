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
