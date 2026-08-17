## -----------------------------------------------------------------------------
#| label: lm-vs-ttest-15
set.seed(42)
grupo_control <- rnorm(20, mean = 50, sd = 10)
grupo_exp     <- rnorm(20, mean = 58, sd = 10)

datos <- data.frame(
  respuesta = c(grupo_control, grupo_exp),
  grupo     = factor(rep(c("Control","Experimental"), each = 20))
)

# t-test clásico
t.test(respuesta ~ grupo, data = datos, var.equal = TRUE)


## -----------------------------------------------------------------------------
#| label: c15-lm-equivalente
# Modelo lineal equivalente
datos$grupo_num <- ifelse(datos$grupo == "Experimental", 1, 0)
summary(lm(respuesta ~ grupo_num, data = datos))


## -----------------------------------------------------------------------------
#| label: lm-vs-anova-15
set.seed(42)
ga <- rnorm(20, 45, 7); gb <- rnorm(20, 55, 7); gc <- rnorm(20, 65, 7)

datos3 <- data.frame(
  respuesta = c(ga, gb, gc),
  grupo     = factor(rep(c("A","B","C"), each = 20))
)

# ANOVA clásico
summary(aov(respuesta ~ grupo, data = datos3))


## -----------------------------------------------------------------------------
#| label: c15-lm-anova-equivalente
# Modelo lineal equivalente
summary(lm(respuesta ~ grupo, data = datos3))


## -----------------------------------------------------------------------------
#| label: fig-glm-unificador
#| echo: false
#| fig-cap: "El modelo lineal general como marco unificador. Regresión lineal (izquierda), prueba t —2 grupos— (centro) y ANOVA —3 grupos— (derecha) son el mismo modelo con diferentes tipos de predictores."
#| fig-width: 9
#| fig-height: 4

par(mfrow = c(1, 3))

# Regresión
x <- seq(1, 10, length = 50)
y <- 2 + 3 * x + rnorm(50, 0, 3)
plot(x, y, pch = 16, col = "#2E86C1", main = "Regresión lineal",
     xlab = "X (continua)", ylab = "Y", cex = 0.7)
abline(lm(y ~ x), col = "#E74C3C", lwd = 2)

# t-test
set.seed(1)
g1 <- rnorm(20, 50, 8); g2 <- rnorm(20, 60, 8)
boxplot(list(Control = g1, Experimental = g2),
        col = c("#AED6F1","#FADBD8"), main = "Prueba t (2 grupos)", ylab = "Y")

# ANOVA
set.seed(2)
ga2 <- rnorm(20, 45, 7); gb2 <- rnorm(20, 55, 7); gc2 <- rnorm(20, 65, 7)
boxplot(list(A = ga2, B = gb2, C = gc2),
        col = c("#AED6F1","#A9DFBF","#FADBD8"), main = "ANOVA (3 grupos)", ylab = "Y")

par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| label: c15-sintaxis-lm
#| eval: false
# # Sintaxis básica
# modelo <- lm(Y ~ X1 + X2, data = mis_datos)
# summary(modelo)
# 
# # Con interacción
# modelo_int <- lm(Y ~ X1 * X2, data = mis_datos)   # efectos principales + interacción
# modelo_int2 <- lm(Y ~ X1 + X2 + X1:X2, data = mis_datos)  # equivalente


## -----------------------------------------------------------------------------
#| label: fig-diagnosticos-glm
#| echo: false
#| fig-cap: "Gráficos diagnósticos del modelo lineal. Residuals vs. Fitted evalúa linealidad y homocedasticidad; Normal Q-Q evalúa la normalidad de los residuos; Scale-Location evalúa homocedasticidad; Residuals vs. Leverage identifica observaciones influyentes."
#| fig-width: 8
#| fig-height: 7

set.seed(42)
x_d <- rnorm(50, 5, 2)
y_d <- 3 + 2 * x_d + rnorm(50, 0, 2)
mod_d <- lm(y_d ~ x_d)

par(mfrow = c(2, 2))
plot(mod_d)
par(mfrow = c(1, 1))

