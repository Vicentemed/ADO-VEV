## -----------------------------------------------------------------------------
#| label: fig-normal-10
#| fig-cap: "Distribución normal estándar (μ=0, σ=1). El 68% de los datos se encuentran dentro de ±1 desviación estándar, el 95% dentro de ±2, y el 99.7% dentro de ±3."
#| fig-width: 7
#| fig-height: 4
#| echo: false

curve(dnorm(x, mean = 0, sd = 1),
      from = -4, to = 4,
      xlab = "Desviaciones estándar (z)",
      ylab = "Densidad",
      main = "Distribución normal estándar",
      lwd = 2, col = "#2C7BB6")

x1 <- seq(-1, 1, length = 200)
polygon(c(-1, x1, 1), c(0, dnorm(x1), 0),
        col = rgb(0.17, 0.48, 0.71, 0.3), border = NA)
abline(v = c(-1, 1), lty = 2, col = "#D7191C")
abline(v = c(-2, 2), lty = 3, col = "#FDAE61")
abline(v = c(-3, 3), lty = 4, col = "#7B3294")
legend("topright",
       legend = c("Distribución N(0,1)", "±1 SD (68%)", "±2 SD (95%)", "±3 SD (99.7%)"),
       col = c("#2C7BB6", "#D7191C", "#FDAE61", "#7B3294"),
       lty = c(1, 2, 3, 4), lwd = c(2, 1, 1, 1),
       bty = "n", cex = 0.85)


## -----------------------------------------------------------------------------
#| label: fig-qq-10
#| echo: false
#| fig-cap: "Gráfico Q-Q de datos simulados con distribución normal. Los puntos sobre la línea de referencia indican normalidad."
#| fig-width: 5
#| fig-height: 5

set.seed(42)
datos_normales <- rnorm(n = 50, mean = 70, sd = 10)

qqnorm(datos_normales,
       main = "Gráfico Q-Q Normal",
       pch = 16, col = "#2C7BB6")
qqline(datos_normales, col = "#D7191C", lwd = 2)


## -----------------------------------------------------------------------------
#| label: shapiro-10
shapiro.test(datos_normales)


## -----------------------------------------------------------------------------
#| label: levene-10
#| message: false
set.seed(42)
grupo_A <- rnorm(30, mean = 50, sd = 10)
grupo_B <- rnorm(30, mean = 55, sd = 11)

datos <- data.frame(
  valor = c(grupo_A, grupo_B),
  grupo = factor(rep(c("A", "B"), each = 30))
)

library(car)
leveneTest(valor ~ grupo, data = datos)


## -----------------------------------------------------------------------------
#| label: fig-transformacion-10
#| echo: false
#| fig-cap: "Efecto de la transformación logarítmica sobre una distribución sesgada. La distribución original (izquierda) muestra asimetría positiva; después de la transformación log (derecha), la distribución se aproxima más a la normalidad."
#| fig-width: 8
#| fig-height: 4

set.seed(42)
datos_sesgados <- exp(rnorm(200, mean = 2, sd = 0.8))

par(mfrow = c(1, 2))
hist(datos_sesgados,
     main = "Distribución original (sesgada)",
     xlab = "Valor", col = "#FDAE61", border = "white", breaks = 20)
hist(log(datos_sesgados),
     main = "Transformación log",
     xlab = "log(Valor)", col = "#2C7BB6", border = "white", breaks = 20)
par(mfrow = c(1, 1))

