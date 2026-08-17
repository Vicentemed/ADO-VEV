## -----------------------------------------------------------------------------
#| label: fig-modelo-estadistico
#| fig-cap: "Esquema general del proceso de modelado estadístico. Los datos recolectados del mundo real se utilizan para ajustar un modelo que permite realizar predicciones e inferencias hacia la población."
#| fig-width: 8
#| fig-height: 3

par(mar = c(0, 0, 0, 0))
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 5),
     xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

# Cajas
rect(0.1, 0.8, 2.4, 2.2, col = "#AED6F1", border = "#2471A3", lwd = 2)
rect(3.8, 0.8, 6.2, 2.2, col = "#A9DFBF", border = "#1E8449", lwd = 2)
rect(7.6, 0.8, 9.9, 2.2, col = "#FAD7A0", border = "#CA6F1E", lwd = 2)
rect(3.8, 2.4, 6.2, 3.8, col = "#a9c9dd", border = "#475492", lwd = 2)

text(1.25, 1.5, "Muestra", cex = 0.9, font = 2, col = "#1A5276")
text(5.0,  1.5, "Modelo\nEstadístico",  cex = 0.9, font = 2, col = "#145A32")
text(8.75, 1.5, "Predicciones", cex = 0.85, font = 2, col = "#784212")
text(5, 3.1, "Población", cex = 0.85, font = 2, col = "#03045e")

# Flechas
arrows(2.5, 1.5, 3.7, 1.5, lwd = 2, col = "#555555", length = 0.12)
arrows(6.3, 1.5, 7.5, 1.5, lwd = 2, col = "#555555", length = 0.12)
arrows(3.7, 3.1, 1.5, 2.3, lwd = 2, col = "#555555", length = 0.12)
arrows(8.6, 2.3, 6.28, 3.1, lwd = 2, col = "#555555", length = 0.12)
text(3.1, 1.75, "ajuste", cex = 0.75, col = "#555555")
text(6.9, 1.75, "validación", cex = 0.75, col = "#555555")
text(7.3, 3.3, "inferencia", cex = 0.75, col = "#555555")


## -----------------------------------------------------------------------------
#| label: fig-mediana-cuartiles
#| echo: false
#| fig-cap: "Medidas de tendencia central y dispersión. La línea central de la caja representa la mediana (Q2); los bordes de la caja representan el primer (Q1) y tercer cuartil (Q3). Los bigotes se extienden hasta 1.5 veces el IQR. Los puntos fuera de los bigotes son valores atípicos (outliers)."
#| fig-width: 6
#| fig-height: 4

set.seed(42)
# Datos simulados de escala NRS (0-10) en 60 pacientes
vas <- round(c(rnorm(50, mean = 4.5, sd = 1.8), runif(10, 8, 10)), 1)
vas <- pmax(pmin(vas, 10), 0)

boxplot(vas,
        ylab = "Puntuación NRS (0–10)",
        main = "Distribución de dolor posquirúrgico (NRS)",
        col  = "#AED6F1",
        border = "#2471A3",
        notch  = FALSE,
        horizontal = FALSE)

# Líneas de cuartiles y etiquetas
q <- quantile(vas)
text(1.35, q[2], paste0("Q1 = ", round(q[2], 1)), cex = 0.8, col = "#1A5276")
text(1.35, q[3], paste0("Mediana = ", round(q[3], 1)), cex = 0.8, col = "#145A32", font = 2)
text(1.35, q[4], paste0("Q3 = ", round(q[4], 1)), cex = 0.8, col = "#1A5276")


## -----------------------------------------------------------------------------
#| label: fig-media-errores
#| echo: false
#| fig-cap: "Media aritmética como modelo estadístico. La línea verde representa la media del grupo (4 puntos NRS). Las líneas punteadas rojas indican la desviación de cada observación respecto a la media — lo que se denomina 'error del modelo'."
#| fig-width: 6
#| fig-height: 4

vas7 <- c(5, 2, 5, 4, 6, 3, 3)
media <- mean(vas7)

plot(1:7, vas7,
     pch  = 16,
     col  = "#2471A3",
     cex  = 1.5,
     xlab = "Sujeto",
     ylab = "Puntuación NRS",
     main = "Media como modelo estadístico",
     ylim = c(0, 8),
     xaxt = "n")
axis(1, at = 1:7, labels = paste0("S", 1:7))

abline(h = media, col = "#27AE60", lwd = 2, lty = 1)
text(7.3, media, paste0("x̄ = ", media), col = "#27AE60", font = 2, cex = 0.9)

for (i in 1:7) {
  segments(i, media, i, vas7[i], col = "#E74C3C", lty = 2, lwd = 1.5)
}


## -----------------------------------------------------------------------------
#| label: calculo-ss
# Cálculo manual de SS, varianza y desviación estándar
vas7     <- c(5, 2, 5, 4, 6, 3, 3)
media    <- mean(vas7)
errores  <- vas7 - media
SS       <- sum(errores^2)
varianza <- SS / (length(vas7) - 1)
DE       <- sqrt(varianza)

cat("Media:", media, "\n")
cat("Errores:", errores, "\n")
cat("SS (suma de cuadrados):", SS, "\n")
cat("Varianza:", varianza, "\n")
cat("Desviación estándar:", round(DE, 4), "\n")


## -----------------------------------------------------------------------------
#| label: fig-tlc
#| echo: false
#| fig-cap: "Ilustración del Teorema del Límite Central. Se simulan 10,000 muestras de una distribución uniforme (no normal). A medida que el tamaño de muestra (n) aumenta de 1 a 30, la distribución de las medias muestrales se aproxima a la distribución normal."
#| fig-width: 9
#| fig-height: 3.4

set.seed(42)
par(mfrow = c(1, 3))

for (n in c(1, 5, 30)) {
  medias <- replicate(10000, mean(runif(n, 0, 10)))
  # Calcular el máximo entre la densidad del histograma y el pico de la curva
  h         <- hist(medias, breaks = 40, plot = FALSE)
  pico_norm <- dnorm(mean(medias), mean = mean(medias), sd = sd(medias))
  y_max     <- max(h$density, pico_norm) * 1.1
  hist(medias,
       breaks = 40,
       main   = paste0("n = ", n),
       xlab   = "Media muestral",
       col    = "#AED6F1",
       border = "white",
       freq   = FALSE,
       ylim   = c(0, y_max))
  curve(dnorm(x, mean = mean(medias), sd = sd(medias)),
        add = TRUE, col = "#E74C3C", lwd = 2)
}
par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| label: z-critico
qnorm(0.975)   # nivel de confianza del 95 %
qnorm(0.995)   # nivel de confianza del 99 %


## -----------------------------------------------------------------------------
#| label: fig-z-normal
#| fig-cap: "Distribución normal estándar y valores críticos (z) de los niveles de confianza más usados. El área azul corresponde al 95 % central de los datos, comprendido entre z = ±1.96 (líneas rojas). Las líneas moradas marcan z = ±2.576, que delimitan el 99 %. Las colas no sombreadas contienen el 5 % y el 1 % restante, respectivamente."
#| fig-width: 8
#| fig-height: 4.5
#| echo: false

z95 <- qnorm(0.975)   # 1.96
z99 <- qnorm(0.995)   # 2.576

curve(dnorm(x), from = -3.6, to = 3.6, n = 400,
      xlab = "z (desviaciones estándar respecto a la media)",
      ylab = "Densidad", lwd = 2, col = "#1A5276",
      xaxt = "n", bty = "n")

# Sombrear el 95 % central
xx <- seq(-z95, z95, length.out = 200)
polygon(c(-z95, xx, z95), c(0, dnorm(xx), 0),
        col = "#AED6F1", border = NA)
curve(dnorm(x), from = -3.6, to = 3.6, add = TRUE, lwd = 2, col = "#1A5276")

# Valores críticos
abline(v = c(-z95, z95), col = "#E74C3C", lwd = 2, lty = 2)
abline(v = c(-z99, z99), col = "#7D3C98", lwd = 2, lty = 3)

# Eje x con los valores relevantes
axis(1, at = c(-z99, -z95, 0, z95, z99),
     labels = c("-2.576", "-1.96", "0", "1.96", "2.576"), cex.axis = 0.8)
       


## -----------------------------------------------------------------------------
#| label: fig-ic
#| echo: false
#| fig-cap: "Intervalos de confianza al 95% de 100 muestras simuladas (n=15 cada una). La línea roja punteada representa la media poblacional verdadera (μ=70). Las barras en azul contienen al verdadero valor poblacional; las barras en naranja no lo contienen — lo cual ocurre aproximadamente el 5% de las veces."
#| fig-width: 8
#| fig-height: 9

set.seed(42)
mu    <- 70   # media poblacional verdadera
sigma <- 10   # desviación estándar poblacional
n     <- 15   # tamaño de cada muestra
nsim  <- 100  # número de muestras

resultados <- data.frame(
  muestra = 1:nsim,
  media   = numeric(nsim),
  li      = numeric(nsim),
  ls      = numeric(nsim)
)

for (i in 1:nsim) {
  muestra_i <- rnorm(n, mean = mu, sd = sigma)
  se_i      <- sd(muestra_i) / sqrt(n)
  t_crit    <- qt(0.975, df = n - 1)
  resultados$media[i] <- mean(muestra_i)
  resultados$li[i]    <- mean(muestra_i) - t_crit * se_i
  resultados$ls[i]    <- mean(muestra_i) + t_crit * se_i
}

contiene <- resultados$li <= mu & resultados$ls >= mu
colores  <- ifelse(contiene, "#2E86C1", "#E67E22")

plot(resultados$media, 1:nsim,
     xlim = c(min(resultados$li) - 2, max(resultados$ls) + 2),
     pch  = 16, col = colores, cex = 0.6,
     xlab = "Valor de la variable de respuesta", ylab = "Número de Muestra",
     main = "Intervalos de confianza al 95%")

for (i in 1:nsim) {
  segments(resultados$li[i], i, resultados$ls[i], i,
           col = colores[i], lwd = 1.2)
}
abline(v = mu, col = "#E74C3C", lty = 2, lwd = 2)
legend("topright",
       legend = c(paste0("Contiene μ (", sum(contiene), "/", nsim, ")"),
                  paste0("No contiene μ (", sum(!contiene), "/", nsim, ")")),
       col = c("#2E86C1", "#E67E22"),
       lty = 1, lwd = 2, bty = "n", cex = 0.85)


## -----------------------------------------------------------------------------
#| label: fig-colas-distribucion
#| echo: false
#| fig-cap: "Regiones de rechazo en la distribución t con 20 grados de libertad. A la izquierda, prueba a dos colas: la región de rechazo (α = 0.05) se divide igualmente en 2.5% en cada cola. A la derecha, prueba a una cola: toda la región de rechazo (5%) se concentra en un solo extremo. La selección a una cola vuelve la prueba más estricta para detectar diferencias en una dirección específica."
#| fig-width: 9
#| fig-height: 4

par(mfrow = c(1, 2))
df_val <- 20
t_crit2 <- qt(0.975, df = df_val)
t_crit1 <- qt(0.95,  df = df_val)

# --- Dos colas ---
curve(dt(x, df = df_val), from = -4, to = 4,
      lwd = 2, col = "#2C7BB6",
      main = "Prueba a dos colas (α = 0.05)",
      xlab = "Estadístico t", ylab = "Densidad")
x_r <- seq(t_crit2, 4, length = 200)
x_l <- seq(-4, -t_crit2, length = 200)
polygon(c(x_r, rev(x_r)), c(dt(x_r, df_val), rep(0, 200)),
        col = "#E74C3C90", border = NA)
polygon(c(x_l, rev(x_l)), c(dt(x_l, df_val), rep(0, 200)),
        col = "#E74C3C90", border = NA)
abline(v = c(-t_crit2, t_crit2), lty = 2, col = "#E74C3C", lwd = 1.5)
text(c(-t_crit2, t_crit2), 0.03,
     c(paste0("-", round(t_crit2, 2)), round(t_crit2, 2)),
     col = "#E74C3C", cex = 0.75)
text(0, 0.15, "No rechazo\nH0", cex = 0.85, font = 2)
text(c(-3.2, 3.2), 0.05, "2.5%", col = "#E74C3C", cex = 0.85, font = 2)

# --- Una cola ---
curve(dt(x, df = df_val), from = -4, to = 4,
      lwd = 2, col = "#2C7BB6",
      main = "Prueba a una cola (α = 0.05)",
      xlab = "Estadístico t", ylab = "Densidad")
x_r1 <- seq(t_crit1, 4, length = 200)
polygon(c(x_r1, rev(x_r1)), c(dt(x_r1, df_val), rep(0, 200)),
        col = "#E74C3C90", border = NA)
abline(v = t_crit1, lty = 2, col = "#E74C3C", lwd = 1.5)
text(t_crit1, 0.03, round(t_crit1, 2), col = "#E74C3C", cex = 0.75)
text(-1, 0.15, "No rechazo\nH0", cex = 0.85, font = 2)
text(3.4, 0.06, "5%", col = "#E74C3C", cex = 0.85, font = 2)

par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| label: fig-fuentes-variabilidad
#| fig-cap: "Descomposición de la variabilidad total de la variable de respuesta. El círculo completo representa el 100 % de la varianza, repartida por área en: la varianza explicada por el modelo (variable explicativa de interés, 43 %), la varianza explicada por una covariable (32 %) y la varianza no sistemática o error aleatorio (25 %)."
#| fig-width: 7.5
#| fig-height: 5.5
#| echo: false

par(mar = c(1, 2, 3, 2))

valores   <- c(43, 32, 25)
colores   <- c("#2E86C1", "#27AE60", "#E67E22")
etiquetas <- c("Varianza explicada\npor el modelo\n(43%)",
               "Varianza explicada\npor la covariable\n(32%)",
               "Varianza no\nsistemática\n(25%)")

pie(valores,
    labels    = etiquetas,
    col       = colores,
    border    = "white",
    clockwise = TRUE,
    radius    = 0.6,
    cex       = 0.85,
    main      = "Variabilidad total de la variable de respuesta (100%)")


## -----------------------------------------------------------------------------
#| label: fig-clasificacion-variables
#| fig-cap: "Clasificación de variables de acuerdo con su nivel de información. La línea representa el gradiente de información desde variables nominales (menor información) hasta variables de razón (mayor información). La escala de medición determina qué pruebas estadísticas son apropiadas para cada tipo de variable."
#| fig-width: 8
#| fig-height: 3.5
#| echo: false

par(mar = c(3, 1, 2, 1))
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 3),
     xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "",
     main = "Niveles de medición de las variables")

# Flecha de gradiente
rect(0.5, 1.3, 9.5, 1.7, col = "#D5D8DC", border = NA)
for (i in 0:200) {
  x_start <- 0.5 + i * (9.0 / 200)
  x_end   <- x_start + 9.0 / 200
  col_i   <- colorRampPalette(c("#AED6F1", "#1A5276"))(201)[i + 1]
  rect(x_start, 1.3, x_end, 1.7, col = col_i, border = NA)
}
arrows(0.4, 1.5, 9.8, 1.5, length = 0.12, lwd = 2, col = "#1A5276")

# Etiquetas
niveles <- c("Nominal", "Ordinal", "Intervalo", "Razón")
pos_x   <- c(1.5, 3.5, 6.0, 8.5)
for (i in seq_along(niveles)) {
  text(pos_x[i], 2.2, niveles[i], font = 2, cex = 0.85, col = "#1A5276")
  segments(pos_x[i], 2.0, pos_x[i], 1.7, col = "#1A5276", lwd = 1.5)
}

# Ejemplos
ejemplos <- c("Raza,\ngrupo\nsanguíneo", "Estadio\ntumoral,\nEVA", "Escala\nLikert,\nn° dientes", "Peso,\ntalla,\ntiempo")
for (i in seq_along(ejemplos)) {
  text(pos_x[i], 0.75, ejemplos[i], cex = 0.7, col = "#555555", font = 3)
  segments(pos_x[i], 1.3, pos_x[i], 1.0, col = "#555555", lwd = 1, lty = 2)
}

text(0.5, 0.3, "Menor\ninformación", cex = 0.7, col = "#888888", adj = 0)
text(9.5, 0.3, "Mayor\ninformación →", cex = 0.7, col = "#888888", adj = 1)


## -----------------------------------------------------------------------------
#| label: fig-or-rr
#| echo: false
#| fig-cap: "Relación entre el Odds Ratio (OR) y el Riesgo Relativo (RR) según la prevalencia del evento en el grupo de referencia. Cuando la prevalencia es baja (<10%), OR y RR son numéricamente similares. A mayor prevalencia, el OR sobreestima la magnitud de la asociación respecto al RR."
#| fig-width: 7
#| fig-height: 4

# Simulación OR vs RR según prevalencia
prevalencia <- seq(0.01, 0.50, by = 0.01)
OR_true     <- 3   # OR verdadero fijo

# RR aproximado a partir del OR y la prevalencia de referencia
# Fórmula: RR = OR / (1 - P0 + P0 * OR)
RR_aprox    <- OR_true / (1 - prevalencia + prevalencia * OR_true)

plot(prevalencia * 100, RR_aprox,
     type = "l", lwd = 2, col = "#2E86C1",
     xlab = "Prevalencia en grupo control (%)",
     ylab = "Riesgo Relativo (RR) estimado",
     main = "OR = 3.0: RR estimado según prevalencia de referencia",
     ylim = c(1, 4))
abline(h = OR_true, lty = 2, col = "#E74C3C", lwd = 1.5)
abline(v = 10, lty = 3, col = "#888888")
legend("topright",
       legend = c("RR estimado", paste0("OR verdadero = ", OR_true),
                  "Prevalencia = 10%"),
       col = c("#2E86C1", "#E74C3C", "#888888"),
       lty = c(1, 2, 3), lwd = c(2, 1.5, 1), bty = "n", cex = 0.85)

