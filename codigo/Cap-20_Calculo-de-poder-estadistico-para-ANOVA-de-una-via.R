## -----------------------------------------------------------------------------
#| label: power-anova-ttest
# Ejemplo: comparación principal Control vs. Light(20s) con delta=2.71, sd=2.2
power.t.test(delta     = 2.71,
             sd        = 2.2,
             power     = 0.80,
             type      = "two.sample",
             sig.level = 0.05)


## -----------------------------------------------------------------------------
#| label: cohen-f-funcion
# Función para calcular la F de Cohen:  f = sigma_m / sigma_within
# SDwithin = desviación estándar DENTRO de los grupos (sqrt(MS_R)), común a todos
Cohen_f <- function(meangroups, kgroups, SDwithin, Gmean) {
  dif     <- Gmean - meangroups        # desviación de cada media respecto a la general
  sumd    <- sum(dif^2)
  sigma_m <- sqrt(sumd / kgroups)      # SD de las medias grupales (asume n iguales)
  f       <- sigma_m / SDwithin
  cat("Cohen f =", round(f, 4), "\n")
  return(invisible(f))
}

# Ejemplo: 4 grupos con medias 18.4, 22.2, 25.1, 32.1
mg       <- c(18.4, 22.2, 25.1, 32.1)
ng       <- 4
SDwithin <- 8.6      # desviación estándar dentro de los grupos
Gmean    <- 24.5

f_cohen <- Cohen_f(mg, ng, SDwithin, Gmean)


## -----------------------------------------------------------------------------
#| label: power-anova-f
#| message: false
library(pwr)

# Cálculo de tamaño muestral con F de Cohen
pwr.anova.test(k         = 4,
               f         = f_cohen,
               sig.level = 0.05,
               power     = 0.80)


## -----------------------------------------------------------------------------
#| label: fig-power-anova
#| echo: false
#| fig-cap: "Curvas de poder para ANOVA de una vía con 3 grupos, en función del tamaño muestral por grupo, para tres magnitudes de la F de Cohen. Con un efecto mediano (f = 0.25), se requieren aproximadamente 52 sujetos por grupo (156 total) para alcanzar un poder del 80% con α = 0.05. La línea gris punteada indica el umbral convencional de poder del 80%."
#| fig-width: 8
#| fig-height: 4

n_seq <- seq(5, 120, by = 2)
escenarios <- list("f pequeño (0.10)" = 0.10,
                   "f mediano (0.25)" = 0.25,
                   "f grande (0.40)"  = 0.40)
colores <- c("#E74C3C","#E67E22","#2E86C1")

plot(0, 0, type = "n",
     xlim = c(5, 120), ylim = c(-0.1, 1),
     xlab = "Tamaño de muestra (n por grupo)",
     ylab = "Poder estadístico (1 − β)",
     main = "Curvas de poder — ANOVA de una vía (k = 3 grupos)")

for (i in seq_along(escenarios)) {
  poder_i <- sapply(n_seq, function(n) {
    pwr.anova.test(k = 3, n = n,
                   f = escenarios[[i]],
                   sig.level = 0.05)$power
  })
  lines(n_seq, poder_i, lwd = 2, col = colores[i])
}

abline(h = 0.80, lty = 2, col = "gray40", lwd = 1.5)
legend("bottomright",
       legend = c(names(escenarios), "Poder = 80%"),
       col    = c(colores, "gray40"),
       lty    = c(1, 1, 1, 2),
       lwd    = c(2, 2, 2, 1.5),
       bty    = "n", cex = 0.85)


## -----------------------------------------------------------------------------
#| label: power-posthoc-anova
# F de Cohen del estudio de Vickers (ML)
dat <- read.table("sets de datos/VickersF.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
ML <- subset(dat, Group == "Multilink")
ML$Cure.time <- factor(ML$Cure.time, levels = c("Control","Dual","Light(20s)"))

grand_mean_ml <- mean(ML$VH)
medias_ml     <- tapply(ML$VH, ML$Cure.time, mean)
sd_within_ml  <- sqrt(mean(tapply(ML$VH, ML$Cure.time, var)))
f_vickers     <- Cohen_f(medias_ml, 3, sd_within_ml, grand_mean_ml)

# Poder post hoc (n=15 por grupo)
pwr.anova.test(k = 3, n = 15, f = f_vickers, sig.level = 0.05)

