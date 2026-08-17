## -----------------------------------------------------------------------------
#| label: fig-ccc-concepto
#| fig-cap: "Ilustración de la diferencia entre correlación de Pearson (r) y CCC de Lin (ρc). Los tres paneles muestran una correlación de Pearson alta y similar (r ≈ 0.96–0.98), pero el CCC difiere: (1) CCC alto — los puntos siguen la línea de identidad; (2) CCC bajo — hay sesgo sistemático (desplazamiento vertical); (3) CCC bajo — la escala es diferente entre métodos (pendiente ≠ 1)."
#| fig-width: 10
#| fig-height: 3.5
#| echo: false

set.seed(42)
x <- seq(5, 25, length = 50)

par(mfrow = c(1, 3))

# Panel A: CCC alto
ya <- x + rnorm(50, 0, 1.5)
plot(x, ya, pch=16, col="#2E86C1", xlim=c(0,30), ylim=c(0,30),
     main="(1) r=0.96, CCC alto", xlab="Método 1", ylab="Método 2")
abline(0, 1, col="#E74C3C", lty=2, lwd=2)
abline(lm(ya~x), col="#2E86C1", lwd=1.5)

# Panel B: sesgo sistemático
yb <- x + 5 + rnorm(50, 0, 1.5)
plot(x, yb, pch=16, col="#2E86C1", xlim=c(0,35), ylim=c(0,35),
     main="(2) r=0.97, CCC bajo\n(sesgo sistemático)", xlab="Método 1", ylab="Método 2")
abline(0, 1, col="#E74C3C", lty=2, lwd=2)
abline(lm(yb~x), col="#2E86C1", lwd=1.5)

# Panel C: pendiente diferente
yc <- 1.5*x + rnorm(50, 0, 2)
plot(x, yc, pch=16, col="#2E86C1", xlim=c(0,40), ylim=c(0,40),
     main="(3) r=0.98, CCC bajo\n(pendiente ≠ 1)", xlab="Método 1", ylab="Método 2")
abline(0, 1, col="#E74C3C", lty=2, lwd=2)
abline(lm(yc~x), col="#2E86C1", lwd=1.5)

par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| message: false
#| label: sim-ccc48
library(epiR)
library(ggplot2)

set.seed(123)
n <- 40
# Potencia de lámparas de fotocurado (mW/cm²)
Gold <- round(rnorm(n, mean = 1000, sd = 150))      # equipo de referencia (gold standard)
A    <- round(Gold + rnorm(n, mean = 15, sd = 30))  # genérico A: diferencia con Gold NORMAL
B    <- round(Gold + (rexp(n, rate = 1/40) - 40))   # genérico B: diferencia con Gold SESGADA

dat <- data.frame(Gold, A, B)
head(dat)


## -----------------------------------------------------------------------------
#| label: normalidad-ccc48
shapiro.test(dat$B - dat$Gold)   # B − Gold


## -----------------------------------------------------------------------------
#| label: fig-identidad-ccc
#| fig-cap: "Gráfico de identidad para comparar los equipos B y Gold. La línea de identidad (roja punteada, pendiente=1, intercepto=0) representa acuerdo perfecto. La línea de regresión (azul) muestra la relación observada. Cuando ambas líneas coinciden, el CCC es alto."
#| fig-width: 6
#| fig-height: 5

ggplot(dat, aes(B, Gold)) +
  geom_point(size = 3, colour = "#2E86C1", alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1,
              colour = "#E74C3C", lty = 2, linewidth = 1.2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15,
              colour = "#2E86C1", fill = "#AED6F1") +
  labs(x = "Equipo B (mW/cm²)", y = "Gold standard (mW/cm²)",
       title = "Gráfico de identidad — Equipo B vs. Gold") +
  coord_equal() +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: ccc-calculo
ccc_res <- epi.ccc(dat$B, dat$Gold, conf.level = 0.95, rep.measure = FALSE)
ccc_res


## -----------------------------------------------------------------------------
#| label: ccc-resumen
cat("=== Resultados CCC de Lin ===\n")
cat("CCC (ρc):", round(ccc_res$rho.c$est, 4), "\n")
cat("IC 95%: [", round(ccc_res$rho.c$lower, 3),
    ",", round(ccc_res$rho.c$upper, 3), "]\n")
cat("r (Pearson):", round(cor(dat$B, dat$Gold, use = "complete.obs"), 4), "\n")
cat("Cb (factor corrección sesgo):", round(ccc_res$C.b, 4), "\n")
cat("Sesgo medio (diferencia):", round(ccc_res$sblalt$est, 4), "\n")
cat("LoA inferior:", round(ccc_res$sblalt$lower, 3), "\n")
cat("LoA superior:", round(ccc_res$sblalt$upper, 3), "\n")


## -----------------------------------------------------------------------------
#| label: tbl-ccc-interp
#| tbl-cap: "Escala de interpretación del CCC de Lin."
#| echo: false

interp_ccc <- data.frame(
  `Valor CCC` = c("< 0.90","0.90–0.95","0.95–0.99","> 0.99"),
  Interpretación = c("Pobre","Moderada","Sustancial","Casi perfecta"),
  check.names = FALSE
)
knitr::kable(interp_ccc, align = c("c","l"))

