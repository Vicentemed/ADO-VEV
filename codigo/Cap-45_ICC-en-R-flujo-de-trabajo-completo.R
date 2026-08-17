## -----------------------------------------------------------------------------
#| label: carga-icc
#| message: false
library(psych); library(irr)

data(anxiety)
dat <- anxiety
head(dat)
str(dat)


## -----------------------------------------------------------------------------
#| label: varianzas-icc
cat("Varianza rater1:", round(var(dat$rater1), 4), "\n")
cat("Varianza rater2:", round(var(dat$rater2), 4), "\n")
cat("Varianza rater3:", round(var(dat$rater3), 4), "\n")


## -----------------------------------------------------------------------------
#| label: icc-calculo
# ICC con paquete irr
# type: "consistency" (variabilidad sistemática entre evaluadores aceptada)
# "agreement" (diferencias absolutas entre evaluadores importan)
# unit: "single" (una medición) o "average" (promedio de k mediciones)

# ICC(3,1) — dos factores, modelo mixto, CONSISTENCIA (evaluadores fijos)
icc_consist <- icc(dat, model = "twoway", type = "consistency", unit = "single")
icc_consist

# ICC(2,1) — dos factores, modelo aleatorio, ACUERDO ABSOLUTO
icc_absol <- icc(dat, model = "twoway", type = "agreement", unit = "single")
icc_absol


## -----------------------------------------------------------------------------
#| label: c46-icc-psych
# ICC con paquete psych
ICC(dat)


## -----------------------------------------------------------------------------
#| label: tbl-icc-equivalencia
#| tbl-cap: "Equivalencia entre la nomenclatura de psych (Shrout y Fleiss) y los argumentos de irr::icc(), para una sola medición. Las versiones promedio se obtienen con unit = average (ICC1k, ICC2k, ICC3k)."
#| echo: false
equiv <- data.frame(
  `Shrout-Fleiss` = c("ICC(1,1)", "ICC(2,1)", "ICC(3,1)"),
  psych           = c("ICC1", "ICC2", "ICC3"),
  `irr::icc()`    = c('model="oneway", unit="single"',
                      'model="twoway", type="agreement", unit="single"',
                      'model="twoway", type="consistency", unit="single"'),
  Modelo          = c("Un factor, aleatorio",
                      "Dos factores aleatorios — acuerdo absoluto",
                      "Dos factores mixto — consistencia"),
  check.names = FALSE
)
knitr::kable(equiv, align = c("c", "c", "l", "l"))


## -----------------------------------------------------------------------------
#| label: fig-icc-scatter
#| fig-cap: "Gráficos de dispersión entre pares de evaluadores para la escala de ansiedad. La línea de identidad (roja punteada, pendiente 1, intercepto 0) representa acuerdo perfecto. Los puntos cercanos a esta línea indican alta concordancia. La línea de regresión (azul) muestra la relación lineal observada."
#| fig-width: 9
#| fig-height: 3
#| message: false

library(ggplot2)
par(mfrow = c(1, 3))

for (pares in list(c("rater1","rater2"), c("rater1","rater3"), c("rater2","rater3"))) {
  plot(dat[[pares[1]]], dat[[pares[2]]],
       xlab = pares[1], ylab = pares[2],
       pch = 16, col = "#2E86C1",
       main = paste(pares[1], "vs.", pares[2]))
  abline(0, 1, col = "#E74C3C", lty = 2, lwd = 2)
  abline(lm(dat[[pares[2]]] ~ dat[[pares[1]]]), col = "#2E86C1", lwd = 1.5)
}
par(mfrow = c(1, 1))

