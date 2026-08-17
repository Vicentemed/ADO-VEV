## -----------------------------------------------------------------------------
#| label: c14-sintaxis-power
#| eval: false
# power.t.test(n         = NULL,   # tamaño de muestra por grupo
#              delta     = NULL,   # diferencia de medias esperada
#              sd        = NULL,   # desviación estándar combinada
#              sig.level = 0.05,   # alfa
#              power     = NULL,   # poder deseado (1-beta)
#              type      = "two.sample",  # "two.sample", "one.sample", "paired"
#              alternative = "two.sided") # "two.sided" o "one.sided"


## -----------------------------------------------------------------------------
#| label: power-paired
power.t.test(delta     = 0.27,
             sd        = 1.78,
             power     = 0.80,
             type      = "paired",
             sig.level = 0.05)


## -----------------------------------------------------------------------------
#| label: fig-power-curve-paired
#| fig-cap: "Curva de poder para la prueba t apareada con Δ = 0.27 y SD = 1.78. El eje X muestra el número de fresas (pares) y el eje Y el poder estadístico. La línea punteada roja marca el umbral de poder del 80% y la línea punteada verde el tamaño de muestra requerido: la curva alcanza ese poder alrededor de n ≈ 343 fresas."
#| fig-width: 7
#| fig-height: 4

n_seq <- seq(10, 400, by = 5)
poder_seq <- sapply(n_seq, function(n) {
  power.t.test(n = n, delta = 0.27, sd = 1.78,
               type = "paired", sig.level = 0.05)$power
})

plot(n_seq, poder_seq, type = "l", lwd = 2, col = "#2E86C1",
     xlab = "Tamaño de muestra (n de pares — fresas)",
     ylab = "Poder estadístico (1 − β)",
     main = "Curva de poder — t apareada (Δ = 0.27, SD = 1.78)")
abline(h = 0.80, lty = 2, col = "#E74C3C", lwd = 1.5)
abline(v = 343, lty = 3, col = "#27AE60", lwd = 1.5)
legend("bottomright",
       legend = c("Curva de poder", "Poder = 80%", "n ≈ 343"),
       col    = c("#2E86C1","#E74C3C","#27AE60"),
       lty    = c(1, 2, 3), lwd = c(2, 1.5, 1.5), bty = "n", cex = 0.85)


## -----------------------------------------------------------------------------
#| label: power-independent
power.t.test(delta     = 0.58,    # diferencia observada entre grupos
             sd        = 2.37,    # DE promedio de los grupos
             power     = 0.80,
             type      = "two.sample",
             sig.level = 0.05)


## -----------------------------------------------------------------------------
#| label: power-posthoc
# Poder post hoc para el estudio de cementos (n=15 por grupo, delta=0.58, sd=2.37)
power.t.test(n         = 15,
             delta     = 0.58,
             sd        = 2.37,
             type      = "two.sample",
             sig.level = 0.05)


## -----------------------------------------------------------------------------
#| label: fig-power-comparativa
#| echo: false
#| fig-cap: "Comparación del poder estadístico para la prueba t de dos muestras en función del tamaño muestral, para tres escenarios de tamaño de efecto (pequeño: Δ = 0.2, mediano: Δ = 0.5, grande: Δ = 0.8) con SD = 1.0 y α = 0.05. La línea punteada marca el umbral convencional de poder = 80%. Tamaños de efecto grandes requieren muestras mucho menores para alcanzar un poder adecuado."
#| fig-width: 8
#| fig-height: 4

n_seq <- seq(5, 100, by = 1)
escenarios <- list(
  "Efecto pequeño (Δ=0.2)" = 0.2,
  "Efecto mediano (Δ=0.5)" = 0.5,
  "Efecto grande (Δ=0.8)"  = 0.8
)
colores <- c("#E74C3C","#E67E22","#2E86C1")

plot(0, 0, type = "n",
     xlim = c(5, 100), ylim = c(-0.2, 1),
     xlab = "Tamaño de muestra (n por grupo)",
     ylab = "Poder estadístico (1 − β)",
     main = "Curvas de poder — prueba t de dos muestras (SD = 1)")

for (i in seq_along(escenarios)) {
  poder_i <- sapply(n_seq, function(n) {
    power.t.test(n = n, delta = escenarios[[i]], sd = 1,
                 type = "two.sample", sig.level = 0.05)$power
  })
  lines(n_seq, poder_i, lwd = 2, col = colores[i])
}

abline(h = 0.80, lty = 2, col = "gray40", lwd = 1.5)
legend("bottomright",
       legend = c(names(escenarios), "Poder = 80%"),
       col    = c(colores, "gray40"),
       lty    = c(1,1,1,2),
       lwd    = c(2,2,2,1.5), bty = "n", cex = 0.85)

