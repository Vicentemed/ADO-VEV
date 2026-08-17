## -----------------------------------------------------------------------------
#| label: fig-logistica-funcion
#| echo: false
#| fig-cap: "Comparación entre la regresión lineal y la logística sobre una respuesta binaria (0/1). Con la **regresión lineal** (izquierda), la recta ajustada produce **predicciones imposibles** —menores que 0 y mayores que 1—, fuera del rango de una probabilidad. La **regresión logística** (derecha) emplea la función logística (sigmoide), que mantiene la probabilidad predicha acotada entre 0 y 1."
#| fig-width: 9
#| fig-height: 4

set.seed(123)
n <- 100
x <- runif(n, -5, 5)
y <- rbinom(n, 1, plogis(1.1 * x))         # respuesta binaria simulada

mod_lin <- lm(y ~ x)                        # regresión lineal
mod_log <- glm(y ~ x, family = binomial)    # regresión logística
xs <- seq(-7, 7, length.out = 200)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Regresión lineal: la recta se sale del rango [0, 1]
plot(x, y, pch = 16, col = "#88888880", ylim = c(-0.3, 1.3), xlim = c(-7, 7),
     xlab = "Predictor (X)", ylab = "P(Y = 1)", main = "Regresión lineal")
abline(h = c(0, 1), lty = 3, col = "gray55")
lines(xs, predict(mod_lin, newdata = data.frame(x = xs)),
      col = "#E74C3C", lwd = 2.5)
text(-5.5, 1.22, "predicciones > 1", col = "#E74C3C", cex = 0.7, adj = 0)
text( 5.5, -0.22, "predicciones < 0", col = "#E74C3C", cex = 0.7, adj = 1)

# Regresión logística: la curva queda acotada en [0, 1]
plot(x, y, pch = 16, col = "#88888880", ylim = c(-0.3, 1.3), xlim = c(-7, 7),
     xlab = "Predictor (X)", ylab = "P(Y = 1)", main = "Regresión logística")
abline(h = c(0, 1), lty = 3, col = "gray55")
lines(xs, predict(mod_log, newdata = data.frame(x = xs), type = "response"),
      col = "#2E86C1", lwd = 2.5)

par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| label: or-demo
#| message: false
library(MASS)
data(birthwt)
dat <- birthwt
dat <- within(dat, {
  ht    <- as.factor(ht); low   <- as.factor(low)
  race  <- as.factor(race); smoke <- as.factor(smoke)
  ui    <- as.factor(ui)
})

# Modelo de regresión logística simple
mod_simple <- glm(low ~ smoke, data = dat, family = binomial)
cat("=== Coeficiente (log-odds) ===\n")
coef(mod_simple)
cat("\n=== OR (exp del coeficiente) ===\n")
exp(coef(mod_simple))


## -----------------------------------------------------------------------------
#| label: epv-check
eventos <- sum(dat$low == 1)
n_predictores <- 7
EPV <- eventos / n_predictores
cat("Eventos (bajo peso):", eventos, "\n")
cat("Predictores:", n_predictores, "\n")
cat("EPV:", round(EPV, 1), "(recomendado ≥ 10)\n")


## -----------------------------------------------------------------------------
#| label: fig-separacion-completa
#| echo: false
#| fig-cap: "Efecto de la separación completa sobre el ajuste logístico. Con **solapamiento** entre las clases (izquierda), la curva logística se ajusta con normalidad. Con **separación completa** (derecha) —un hueco en el eje X entre las categorías del desenlace— la curva se vuelve casi vertical y el error estándar del coeficiente se dispara."
#| fig-width: 9
#| fig-height: 4
#| warning: false

set.seed(1)
# Clases con solapamiento (ajuste normal)
xA <- c(rnorm(25, -1, 1.3), rnorm(25, 1, 1.3)); yA <- c(rep(0, 25), rep(1, 25))
mA <- glm(yA ~ xA, family = binomial)

# Separación completa (hueco entre las clases)
xB <- c(runif(25, -3, -0.6), runif(25, 0.6, 3)); yB <- c(rep(0, 25), rep(1, 25))
mB <- glm(yB ~ xB, family = binomial)

xs <- seq(-4, 4, length.out = 300)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(xA, yA, pch = 16, col = "#88888880", xlim = c(-4, 4), ylim = c(-0.1, 1.1),
     xlab = "Predictor (X)", ylab = "P(Y)", main = "Con solapamiento")
lines(xs, predict(mA, data.frame(xA = xs), type = "response"), col = "#2E86C1", lwd = 2.5)

plot(xB, yB, pch = 16, col = "#88888880", xlim = c(-4, 4), ylim = c(-0.1, 1.1),
     xlab = "Predictor (X)", ylab = "P(Y)", main = "Separación completa")
lines(xs, predict(mB, data.frame(xB = xs), type = "response"), col = "#E74C3C", lwd = 2.5)
abline(v = 0, lty = 3, col = "gray50")
par(mfrow = c(1, 1))

