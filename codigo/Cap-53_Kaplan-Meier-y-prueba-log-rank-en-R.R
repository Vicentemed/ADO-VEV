## -----------------------------------------------------------------------------
#| label: carga-CRIT1
#| message: false
library(survival); library(survminer); library(dplyr)

dat <- read.table("Examples/CRIT1.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

# status: 0 = sin evento, 1 = evento (fractura, desalojo, perforación o pérdida)
# stat:   NO = sin evento, SI = evento
str(dat)


## -----------------------------------------------------------------------------
#| label: c55-descripcion-muestra
#| message: false
library(table1)
table1(~ age + sex + asa + brux | stat, data = dat)


## -----------------------------------------------------------------------------
#| label: km-descriptivo55
dat$status <- as.integer(dat$status)
km_global  <- survfit(Surv(time, status) ~ 1, data = dat)
summary(km_global)


## -----------------------------------------------------------------------------
#| label: km-descriptivo-vals
#| include: false
s288  <- summary(km_global, times = 288)
s2078 <- summary(km_global, times = 2078)


## -----------------------------------------------------------------------------
#| label: fig-km-global
#| fig-cap: "Curva de Kaplan-Meier global de la supervivencia de las coronas de acero inoxidable. El eje horizontal representa el tiempo en días desde la cementación; el eje vertical, la probabilidad estimada de supervivencia S(t). La banda sombreada corresponde al intervalo de confianza al 95 % y las marcas (+) señalan las observaciones censuradas."
#| fig-width: 8
#| fig-height: 5
#| message: false
ggsurvplot(km_global, data = dat,
           conf.int = TRUE,
           palette  = "#2E86C1",
           xlab     = "Días",
           ylab     = "Probabilidad de supervivencia S(t)",
           ggtheme  = theme_bw(base_size = 11))


## -----------------------------------------------------------------------------
#| label: km-365
summary(km_global, times = 365.25)


## -----------------------------------------------------------------------------
#| label: km-365-val
#| include: false
s365 <- summary(km_global, times = 365.25)


## -----------------------------------------------------------------------------
#| label: logrank-sexo55
lr_sexo <- survdiff(Surv(time, status) ~ sex, data = dat)
lr_sexo


## -----------------------------------------------------------------------------
#| label: fig-km-sexo
#| fig-cap: "Curvas de Kaplan-Meier de la supervivencia de las coronas de acero según el sexo del paciente. El valor de p corresponde a la prueba log-rank y las bandas sombreadas representan los intervalos de confianza al 95 % de cada curva. Debido a la baja tasa de eventos, las diferencias observadas deben interpretarse con cautela."
#| fig-width: 8
#| fig-height: 6
#| message: false
km_sexo <- survfit(Surv(time, status) ~ sex, data = dat)
ggsurvplot(km_sexo, data = dat,
           pval        = TRUE,
           conf.int    = TRUE,
           palette     = c("#2E86C1","#E74C3C"),
           legend.labs = c("Femenino","Masculino"),
           xlab        = "Días",
           ylab        = "Probabilidad de supervivencia S(t)",
           ggtheme     = theme_bw(base_size = 11))


## -----------------------------------------------------------------------------
#| label: logrank-brux55
lr_brux <- survdiff(Surv(time, status) ~ brux, data = dat)
lr_brux


## -----------------------------------------------------------------------------
#| label: fig-km-brux
#| fig-cap: "Curvas de Kaplan-Meier de la supervivencia de las coronas de acero según la presencia de bruxismo, con la tabla de sujetos en riesgo. Las bandas sombreadas representan los intervalos de confianza al 95 % de cada curva. Los pacientes con bruxismo presentan una probabilidad de supervivencia significativamente menor (prueba log-rank)."
#| fig-width: 8
#| fig-height: 7
#| message: false
km_brux <- survfit(Surv(time, status) ~ brux, data = dat)
ggsurvplot(km_brux, data = dat,
           pval          = TRUE,
           conf.int      = TRUE,
           risk.table    = TRUE,
           tables.height = 0.25,
           tables.theme  = theme_cleantable(),
           palette       = c("#27AE60","#E74C3C"),
           legend.labs   = c("Sin bruxismo","Con bruxismo"),
           xlab          = "Días",
           ylab          = "Probabilidad de supervivencia S(t)",
           ggtheme       = theme_bw(base_size = 11))


## -----------------------------------------------------------------------------
#| label: km-brux-summary
summary(km_brux)


## -----------------------------------------------------------------------------
#| label: km-brux-730
summary(km_brux, times = 730.5)


## -----------------------------------------------------------------------------
#| label: km-brux-2889
summary(km_brux, times = 2889)


## -----------------------------------------------------------------------------
#| label: km-brux-2889-val
#| include: false
s2889 <- summary(km_brux, times = 2889)

