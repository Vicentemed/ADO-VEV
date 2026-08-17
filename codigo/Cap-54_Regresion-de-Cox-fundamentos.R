## -----------------------------------------------------------------------------
#| label: fig-hr-concepto
#| fig-cap: "Interpretación del Hazard Ratio. HR = 2 significa que el grupo expuesto tiene el doble de riesgo instantáneo de presentar el evento en comparación con el grupo de referencia, en cualquier momento del seguimiento."
#| fig-width: 7
#| fig-height: 4
#| echo: false
#| message: false

library(survival)
set.seed(42)
n <- 100
dat_demo <- data.frame(
  tiempo = c(rexp(n, 0.05), rexp(n, 0.10)),
  evento = rbinom(n*2, 1, 0.7),
  grupo  = rep(c("Referencia","HR = 2"), each = n)
)

library(survminer)
km_demo <- survfit(Surv(tiempo, evento) ~ grupo, data = dat_demo)
ggsurvplot(km_demo, data = dat_demo,
           palette = c("#2E86C1","#E74C3C"),
           xlab = "Tiempo", ylab = "S(t)",
           title = "Efecto de un HR = 2",
           ggtheme = theme_bw(base_size = 11),
           legend.labs = c("HR = 2","Referencia (HR = 1)"))


## -----------------------------------------------------------------------------
#| label: fig-log-log-plot
#| fig-cap: "Gráfico de log(-log(S(t))) según la presencia de bruxismo en el estudio de coronas de acero (CRIT). Si el supuesto de riesgos proporcionales se cumple, las dos curvas deben ser aproximadamente paralelas y no cruzarse; una falta de paralelismo indicaría que el HR varía con el tiempo."
#| fig-width: 6
#| fig-height: 5
#| message: false

library(survival)

dat <- read.table("Examples/CRIT1.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat$status <- as.integer(dat$status)

km_brux <- survfit(Surv(time, status) ~ brux, data = dat)

plot(km_brux, fun = "cloglog",
     col  = c("#27AE60","#E74C3C"),
     xlab = "log(tiempo)",
     ylab = "log(-log(S(t)))",
     main = "Gráfico log(-log) — Verificación de riesgos proporcionales",
     lwd  = 2)
legend("topleft", legend = c("Sin bruxismo","Con bruxismo"),
       col = c("#27AE60","#E74C3C"), lwd = 2, bty = "n")

