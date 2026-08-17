## -----------------------------------------------------------------------------
#| label: fig-concepto-regresion
#| fig-cap: "Conceptos fundamentales de la regresión lineal simple. La línea recta está definida por su intercepto (b₀, punto de cruce con el eje Y) y su pendiente (b₁, inclinación de la línea). Los residuos (ε) son las diferencias verticales entre cada punto observado y la línea de regresión."
#| fig-width: 6
#| fig-height: 4
#| echo: false

set.seed(42)
x_demo <- c(1, 2, 3, 4, 5, 6)
y_demo <- 2 + 1.5 * x_demo + rnorm(6, 0, 0.8)
mod_demo <- lm(y_demo ~ x_demo)

plot(x_demo, y_demo,
     pch = 16, cex = 1.8, col = "#2E86C1",
     xlim = c(0, 7), ylim = c(0, 12),
     xlab = "Variable X (explicativa)",
     ylab = "Variable Y (respuesta)",
     main = "Modelo de regresión lineal simple")

abline(mod_demo, col = "#E74C3C", lwd = 2)

# Residuos
y_hat <- fitted(mod_demo)
for (i in 1:6) {
  segments(x_demo[i], y_demo[i], x_demo[i], y_hat[i],
           col = "#27AE60", lty = 2, lwd = 1.5)
}

# Intercepto
points(0, coef(mod_demo)[1], pch = 18, cex = 2, col = "#E67E22")
segments(0, 0, 0, coef(mod_demo)[1], col = "#E67E22", lty = 3)

legend("topleft",
       legend = c("Datos observados", "Línea de regresión",
                  paste0("Intercepto (b0 = ", round(coef(mod_demo)[1],2), ")"),
                  "Residuos (ε)"),
       col = c("#2E86C1","#E74C3C","#E67E22","#27AE60"),
       pch = c(16, NA, 18, NA),
       lty = c(NA, 1, 3, 2),
       lwd = c(NA, 2, 1, 1.5),
       bty = "n", cex = 0.8)

text(6.6, coef(mod_demo)[1] + 6.2*coef(mod_demo)[2] - 0.6,
     paste0("b1 = ", round(coef(mod_demo)[2], 2)), col = "#E74C3C", cex = 0.9)


## -----------------------------------------------------------------------------
#| label: datos-uvvis
# Datos: absorbancia (X) y concentración (Y) en curva de calibración
y <- c(1, 3, 5, 7, 15)
x <- c(0.1472, 0.3949, 0.5635, 0.5861, 1.6347)

Conc <- data.frame(Absorbancia = x, Concentracion = y)
knitr::kable(Conc, caption = "Datos de la curva de calibración espectrofotométrica.")


## -----------------------------------------------------------------------------
#| label: fig-scatter-uvvis
#| fig-cap: "Curva de calibración espectrofotométrica: relación entre la absorbancia (UV-Vis) y la concentración de la molécula. La línea de regresión (rojo) se ajusta por mínimos cuadrados; la banda gris representa el intervalo de confianza al 95% del modelo. El elevado R² indica un excelente ajuste del modelo lineal a los datos."
#| fig-width: 6
#| fig-height: 4
#| message: false

library(ggplot2)
ggplot(Conc, aes(Absorbancia, Concentracion)) +
  geom_point(size = 3.5, colour = "#2E86C1") +
  geom_smooth(method = "lm", se = TRUE, colour = "#E74C3C", alpha = 0.15) +
  labs(x = "Absorbancia (UA)",
       y = "Concentración (unidades arbitrarias)") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: modelo-uvvis
LM <- lm(Concentracion ~ Absorbancia, data = Conc)
summary(LM)


## -----------------------------------------------------------------------------
#| label: fig-ss-regresion
#| fig-cap: "Descomposición de la suma de cuadrados en el modelo de regresión. La línea roja punteada representa la media de Y (modelo basal). La línea de regresión (azul) reduce sustancialmente los errores respecto al modelo basal. La diferencia entre SST (variabilidad total) y SSR (variabilidad residual) es SSM (variabilidad explicada por el modelo)."
#| fig-width: 7
#| fig-height: 4
#| echo: false

media_y <- mean(Conc$Concentracion)
y_hat   <- fitted(LM)

plot(Conc$Absorbancia, Conc$Concentracion,
     pch = 16, cex = 1.8, col = "#2E86C1",
     xlab = "Absorbancia (UA)",
     ylab = "Concentración",
     main = "SS total vs. SS residual vs. SS modelo")

abline(h = media_y, col = "#E74C3C", lty = 2, lwd = 2)
abline(LM, col = "#2E86C1", lwd = 2)

for (i in seq_along(Conc$Absorbancia)) {
  # SSR: residuos del modelo (verde)
  segments(Conc$Absorbancia[i], Conc$Concentracion[i],
           Conc$Absorbancia[i], y_hat[i],
           col = "#27AE60", lty = 1, lwd = 1.5)
  # SSM: mejora del modelo vs media (naranja)
  segments(Conc$Absorbancia[i], media_y,
           Conc$Absorbancia[i], y_hat[i],
           col = "#E67E22", lty = 2, lwd = 1.2)
}

legend("topleft",
       legend = c("Datos", "Media de Y (modelo basal)", "Línea de regresión",
                  "Residuos (SSR)", "Mejora vs. media (SSM)"),
       col = c("#2E86C1","#E74C3C","#2E86C1","#27AE60","#E67E22"),
       pch = c(16,NA,NA,NA,NA),
       lty = c(NA,2,1,1,2),
       lwd = c(NA,2,2,1.5,1.2),
       bty = "n", cex = 0.75)


## -----------------------------------------------------------------------------
#| label: calculo-r2
# Extracción de componentes del modelo
SS_T <- sum((Conc$Concentracion - mean(Conc$Concentracion))^2)
SS_R <- sum(residuals(LM)^2)
SS_M <- SS_T - SS_R
R2   <- SS_M / SS_T

cat("SS Total (SST):", round(SS_T, 4), "\n")
cat("SS Modelo (SSM):", round(SS_M, 4), "\n")
cat("SS Residual (SSR):", round(SS_R, 4), "\n")
cat("R² calculado manualmente:", round(R2, 4), "\n")
cat("R² del summary(LM):", round(summary(LM)$r.squared, 4), "\n")


## -----------------------------------------------------------------------------
#| label: calculo-F
df_M <- 1          # 1 predictor
df_R <- nrow(Conc) - 1 - 1  # N - p - 1
MS_M <- SS_M / df_M
MS_R <- SS_R / df_R
F_stat <- MS_M / MS_R

cat("F calculado:", round(F_stat, 3), "\n")
cat("p-value:", round(pf(F_stat, df_M, df_R, lower.tail = FALSE), 5), "\n")


## -----------------------------------------------------------------------------
#| label: coeficientes-regresion
coef_tabla <- data.frame(
  Parámetro   = c("Intercepto (b₀)", "Pendiente (b₁)"),
  Estimado    = round(coef(LM), 5),
  Error_Std   = round(summary(LM)$coefficients[,2], 5),
  t_value     = round(summary(LM)$coefficients[,3], 3),
  p_value     = round(summary(LM)$coefficients[,4], 5)
)
knitr::kable(coef_tabla, row.names = FALSE,
             caption = "Coeficientes del modelo de regresión lineal para la curva de calibración.")


## -----------------------------------------------------------------------------
#| label: prediccion-modelo
# Predicción para absorbancia = 0.5635
abs_nueva <- data.frame(Absorbancia = 0.5635)
pred <- predict(LM, newdata = abs_nueva, interval = "confidence", level = 0.95)

cat("Concentración observada: 5\n")
cat("Concentración predicha:", round(pred[1,"fit"], 4), "\n")
cat("IC 95% predicción: [", round(pred[1,"lwr"],3), ",", round(pred[1,"upr"],3), "]\n")

