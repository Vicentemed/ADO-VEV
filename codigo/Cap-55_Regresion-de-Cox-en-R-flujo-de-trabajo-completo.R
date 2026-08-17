## -----------------------------------------------------------------------------
#| label: prep-cox57
#| message: false
library(survival); library(survminer); library(car); library(rms)

dat <- read.table("Examples/CRIT1.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

# La variable de estatus debe ser un entero (1 = evento, 0 = censura)
dat$status <- as.integer(dat$status)
dat$sex    <- factor(dat$sex)
dat$asa    <- factor(dat$asa)
dat$brux   <- factor(dat$brux)

str(dat[, c("time", "status", "asa", "age", "sex", "brux")])


## -----------------------------------------------------------------------------
#| label: vif-cox57
modS <- coxph(Surv(time, status) ~ asa + age + sex + brux, data = dat)
vif(modS)


## -----------------------------------------------------------------------------
#| label: anova-cox57
Anova(modS)


## -----------------------------------------------------------------------------
#| label: summary-cox57
summary(modS)


## -----------------------------------------------------------------------------
#| label: simplif-cox57
modS2 <- coxph(Surv(time, status) ~ sex + brux, data = dat)
Anova(modS2)


## -----------------------------------------------------------------------------
#| label: modelo-final-cox57
modF <- coxph(Surv(time, status) ~ brux, data = dat)
summary(modF)


## -----------------------------------------------------------------------------
#| label: anova-comp-cox57
anova(modS, modF)


## -----------------------------------------------------------------------------
#| label: cph-fit-cox57
#| message: false
ddist <- datadist(dat)
options(datadist = "ddist")

modSF <- cph(Surv(time, status) ~ asa + age + sex + brux, x = TRUE, y = TRUE, data = dat)
modSF


## -----------------------------------------------------------------------------
#| label: cph-final-cox57
modFi <- cph(Surv(time, status) ~ brux, x = TRUE, y = TRUE, data = dat)
modFi


## -----------------------------------------------------------------------------
#| label: validate-cox57
#| message: false
set.seed(1)
validate(modSF, B = 200)


## -----------------------------------------------------------------------------
#| label: zph-cox57
zph <- cox.zph(modF)
zph


## -----------------------------------------------------------------------------
#| label: fig-schoenfeld
#| fig-cap: "Residuos de Schoenfeld escalados frente al tiempo transformado para el modelo final. Una dispersión sin tendencia sistemática alrededor de una línea horizontal indica que el supuesto de riesgos proporcionales se cumple; las líneas discontinuas representan la envolvente de ± 2 errores estándar."
#| fig-width: 7
#| fig-height: 5
#| message: false
ggcoxzph(zph, ggtheme = theme_bw(base_size = 11))


## -----------------------------------------------------------------------------
#| label: predict-cox57
nuevos <- data.frame(
  asa  = factor(c("I", "II", "II"), levels = levels(dat$asa)),
  age  = c(4, 6, 5),
  sex  = factor(c("F", "F", "F"), levels = levels(dat$sex)),
  brux = factor(c("NO", "NO", "SI"), levels = levels(dat$brux))
)
pred <- predict(modS, newdata = nuevos, type = "risk", se.fit = TRUE)


## -----------------------------------------------------------------------------
#| label: predictS-cox57
predictS <- function(pred) {
  Risk    <- pred$fit
  Risk.se <- pred$se.fit
  data.frame(
    Riesgo   = round(Risk, 3),
    EE       = round(Risk.se, 3),
    IC_inf   = round(pmax(0, Risk - 1.96 * Risk.se), 3),  # truncado en 0 (un riesgo no puede ser negativo)
    IC_sup   = round(Risk + 1.96 * Risk.se, 3)
  )
}
knitr::kable(predictS(pred),
             caption = "Riesgo relativo estimado (risk score) para tres pacientes hipotéticos.")


## -----------------------------------------------------------------------------
#| label: fig-cox-pred
#| fig-cap: "Funciones de supervivencia estimadas por el modelo de Cox para tres pacientes hipotéticos. El paciente 3 (con bruxismo) presenta la menor probabilidad de supervivencia de la corona a lo largo del seguimiento."
#| fig-width: 7
#| fig-height: 5
sf_nuevos <- survfit(modS, newdata = nuevos)
plot(sf_nuevos, col = c("#2E86C1", "#27AE60", "#E74C3C"), lwd = 2,
     lty = c(1, 2, 4), ylim = c(0, 1),
     xlab = "Tiempo (días)", ylab = "Probabilidad de supervivencia S(t)")
legend("bottomleft", bty = "n",
       legend = c("Paciente 1 (ASA I, sin bruxismo)",
                  "Paciente 2 (ASA II, sin bruxismo)",
                  "Paciente 3 (ASA II, con bruxismo)"),
       col = c("#2E86C1", "#27AE60", "#E74C3C"), lwd = 2, lty = c(1, 2, 4))

