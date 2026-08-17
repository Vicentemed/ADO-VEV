## -----------------------------------------------------------------------------
#| label: sim-bland47
set.seed(123)
n <- 40
# Potencia de lámparas de fotocurado (mW/cm²)
Gold <- round(rnorm(n, mean = 1000, sd = 150))      # equipo de referencia (gold standard)
A    <- round(Gold + rnorm(n, mean = 15, sd = 30))  # genérico A: diferencia con Gold NORMAL
B    <- round(Gold + (rexp(n, rate = 1/40) - 40))   # genérico B: diferencia con Gold SESGADA

dat <- data.frame(Gold, A, B)
head(dat)


## -----------------------------------------------------------------------------
#| label: normalidad-bland47
sh_A <- shapiro.test(dat$A - dat$Gold)   # A − Gold
sh_A
sh_B <- shapiro.test(dat$B - dat$Gold)   # B − Gold
sh_B


## -----------------------------------------------------------------------------
#| label: def-bland-fun
#| message: false
library(ggplot2)

bland_altman <- function(metodo, referencia, nombre) {
  promedio   <- (metodo + referencia) / 2
  diferencia <- metodo - referencia
  bias  <- mean(diferencia)
  sd_d  <- sd(diferencia)
  loa_u <- bias + 1.96 * sd_d
  loa_l <- bias - 1.96 * sd_d
  ref   <- mean(promedio)                                 # nivel medio de medición
  pct   <- function(v) sprintf("%.1f%%", 100 * v / ref)   # valor como % de ese nivel

  cat(sprintf("%s vs. Gold — sesgo = %.2f (%s) | LoA [%.1f (%s), %.1f (%s)]\n",
              nombre, bias, pct(bias), loa_l, pct(loa_l), loa_u, pct(loa_u)))

  # Etiquetas: cada línea con su valor absoluto y el porcentaje que representa
  etiquetas <- data.frame(
    y   = c(bias, loa_u, loa_l),
    txt = c(sprintf("Sesgo = %.1f (%s)",    bias,  pct(bias)),
            sprintf("+1.96 SD = %.1f (%s)", loa_u, pct(loa_u)),
            sprintf("-1.96 SD = %.1f (%s)", loa_l, pct(loa_l)))
  )

  ggplot(data.frame(promedio, diferencia), aes(promedio, diferencia)) +
    geom_point(size = 2.5, colour = "#2E86C1", alpha = 0.8) +
    geom_hline(yintercept = bias, colour = "#E74C3C", linewidth = 1.1) +
    geom_hline(yintercept = c(loa_u, loa_l), colour = "#E74C3C",
               lty = 2, linewidth = 0.8) +
    geom_text(data = etiquetas, aes(x = max(promedio), y = y, label = txt),
              inherit.aes = FALSE, hjust = 1, vjust = -0.5,
              colour = "#E74C3C", size = 3.2) +
    scale_y_continuous(expand = expansion(mult = c(0.10, 0.15))) +
    labs(x = "Promedio de las dos mediciones (mW/cm²)",
         y = paste0("Diferencia (", nombre, " − Gold)"),
         title = paste0("Bland-Altman — ", nombre, " vs. Gold")) +
    theme_bw(base_size = 12)
}


## -----------------------------------------------------------------------------
#| label: fig-bland-A
#| fig-cap: "Bland-Altman del equipo A frente al gold standard. La línea roja sólida es el sesgo (diferencia media) y las punteadas, los límites de acuerdo al 95 %. Cada línea se rotula con su valor absoluto (mW/cm²) y el porcentaje que representa respecto al nivel medio de medición. Como las diferencias son normales, estos límites son válidos: se espera que el 95 % de las diferencias caiga dentro de ellos."
#| fig-width: 7
#| fig-height: 5
bland_altman(dat$A, dat$Gold, "A")


## -----------------------------------------------------------------------------
#| label: fig-bland-B
#| fig-cap: "Bland-Altman del equipo B frente al gold standard, con cada línea rotulada con su valor absoluto y su porcentaje. La función calcula el sesgo y los límites igual que antes, pero como las diferencias B − Gold son sesgadas (no normales), los límites simétricos media ± 1.96·SD no describen bien la distribución real de las diferencias."
#| fig-width: 7
#| fig-height: 5
bland_altman(dat$B, dat$Gold, "B")


## -----------------------------------------------------------------------------
#| label: ic-bland47
d        <- dat$A - dat$Gold
se_bias  <- sd(d) / sqrt(length(d))
t_crit   <- qt(0.975, df = length(d) - 1)
# Sesgo y límites de acuerdo (A vs. Gold) para el reporte
bias  <- mean(d)
sd_d  <- sd(d)
loa_u <- bias + 1.96 * sd_d
loa_l <- bias - 1.96 * sd_d
ref   <- mean((dat$A + dat$Gold) / 2)
cat("IC 95% del sesgo (A vs. Gold): [",
    round(mean(d) - t_crit * se_bias, 2), ",",
    round(mean(d) + t_crit * se_bias, 2), "]\n")


## -----------------------------------------------------------------------------
#| label: ccc-bridge47
#| message: false
library(epiR)
cat("CCC de Lin — A vs. Gold:\n"); print(epi.ccc(dat$A, dat$Gold)$rho.c)
cat("\nCCC de Lin — B vs. Gold:\n"); print(epi.ccc(dat$B, dat$Gold)$rho.c)

