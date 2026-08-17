## -----------------------------------------------------------------------------
#| label: prep-logistic53
#| message: false
library(MASS); library(car); library(gmodels); library(ggplot2); library(dplyr)

data(birthwt)
dat <- birthwt
dat <- within(dat, {
  ht    <- as.factor(ht); low   <- as.factor(low)
  race  <- as.factor(race); smoke <- as.factor(smoke)
  ui    <- as.factor(ui)
})
dat$low <- relevel(dat$low, ref = "0")


## -----------------------------------------------------------------------------
#| label: fig-desc-logistic
#| fig-cap: "Distribución del peso materno por estado de bajo peso al nacer. Las madres de recién nacidos con bajo peso tienden a pesar menos antes del embarazo. Esta diferencia sugiere que el peso materno es un predictor relevante del bajo peso neonatal."
#| fig-width: 6
#| fig-height: 4

ggplot(dat, aes(low, lwt, fill = low)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#2E86C1","#E74C3C"),
                    labels = c("Peso normal","Bajo peso")) +
  labs(x = "Peso al nacer", y = "Peso materno (lb)", fill = "RN") +
  theme_bw(base_size = 12) + theme(legend.position = "top")


## -----------------------------------------------------------------------------
#| label: c53-tabla-descriptiva
#| message: false
library(table1)
table1(~ age + lwt + ftv + ptl + ht + race + smoke + ui | low, data = dat)


## -----------------------------------------------------------------------------
#| label: modelo-logistico-Sat
# Modelo completo
mod_log <- glm(low ~ age + lwt + ftv + ht + ptl + race + smoke + ui,
               data = dat, family = binomial(link = "logit"))
summary(mod_log)


## -----------------------------------------------------------------------------
#| label: fig-lwt-bwt-low
#| fig-cap: "Dirección del efecto del peso materno (`lwt`) según el tipo de respuesta. Izquierda: con una respuesta continua (`bwt`), a mayor `lwt` mayor peso al nacer (relación lineal positiva). Derecha: con una respuesta dicotómica (`low`), la probabilidad de bajo peso al nacer disminuye al aumentar `lwt` (relación inversa), modelada con una curva logística."
#| fig-width: 9
#| fig-height: 4
#| message: false

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Respuesta continua: bwt ~ lwt (lineal, positiva)
plot(dat$lwt, dat$bwt, pch = 16, col = "#88888880",
     xlab = "Peso de la madre, lwt (lb)", ylab = "Peso al nacer, bwt (g)",
     main = "Respuesta continua (bwt)")
abline(lm(bwt ~ lwt, data = dat), col = "#2E86C1", lwd = 2.5)

# Respuesta binaria: low ~ lwt (logística, inversa)
low_num <- as.numeric(as.character(dat$low))
plot(dat$lwt, low_num, pch = 16, col = "#88888880", ylim = c(-0.1, 1.1),
     xlab = "Peso de la madre, lwt (lb)", ylab = "Bajo peso al nacer, low (0/1)",
     main = "Respuesta binaria (low)")
ml_low <- glm(low ~ lwt, data = dat, family = binomial)
xs <- seq(min(dat$lwt), max(dat$lwt), length.out = 200)
lines(xs, predict(ml_low, newdata = data.frame(lwt = xs), type = "response"),
      col = "#E74C3C", lwd = 2.5)

par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| label: c53-or-ic
# OR con IC 95%
OR_tabla <- cbind(
  OR    = round(exp(coef(mod_log)), 3),
  round(exp(confint(mod_log)), 3)
)
knitr::kable(OR_tabla, col.names = c("OR","IC 95% Inferior","IC 95% Superior"),
             caption = "Odds Ratios con intervalos de confianza al 95% para el modelo de regresión logística.")


## -----------------------------------------------------------------------------
#| label: c53-chi-estadistico
chistat <- mod_log$null.deviance - mod_log$deviance
chistat


## -----------------------------------------------------------------------------
#| label: c53-chi-df
chidf <- mod_log$df.null - mod_log$df.residual
chidf


## -----------------------------------------------------------------------------
#| label: c53-chi-pvalor
chisqp <- 1 - pchisq(chistat, chidf)
chisqp


## -----------------------------------------------------------------------------
#| label: c53-vif
vif(mod_log)


## -----------------------------------------------------------------------------
#| label: centrar-vars-log
dat$age_c <- dat$age - mean(dat$age)


## -----------------------------------------------------------------------------
#| label: c53-anova-saturado
Logm1 <- glm(low ~ age_c +I(age_c^2) + ftv + ht + lwt + ptl + race + smoke + ui, data=dat, family=binomial(logit))
Anova(Logm1)


## -----------------------------------------------------------------------------
#| label: c53-anova-simplificado
Logm1 <- glm(low ~ ht + lwt + race + smoke + ui, data=dat, family=binomial(logit))
Anova(Logm1)


## -----------------------------------------------------------------------------
#| label: c53-comparar-nulo
modB<-glm(low~1,data=dat, family=binomial(logit))
LogmF <- glm(low ~ ht + lwt + race + smoke + ui, data=dat, family=binomial(logit))
anova(modB,LogmF, test="Chisq")


## -----------------------------------------------------------------------------
#| label: c53-aic-modelos
AIC(modB,LogmF)


## -----------------------------------------------------------------------------
#| label: c53-bic-modelos
BIC(modB,LogmF)


## -----------------------------------------------------------------------------
#| label: simp-leaps-log
#| message: false
library(leaps)
Logleaps_mod <- regsubsets(low ~ age_c +I(age_c^2) + ftv + ht + lwt + ptl + race + smoke + ui, data=dat, nbest = 5, nvmax = 10)

res_sumlog <- summary(Logleaps_mod)
res_sumlog



## -----------------------------------------------------------------------------
#| label: c53-mejor-modelo-criterio
res.sum <- summary(Logleaps_mod)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic))



## -----------------------------------------------------------------------------
#| label: Pseudo-Rsquared
#| message: false
LogisticPseudoR2s<-function(LogModel) { 
dev<-LogModel$deviance 
nullDev<-LogModel$null.deviance 
modelN<-length(LogModel$fitted.values) 
R.l<-1-dev/nullDev 
R.cs<-1-exp(-(nullDev-dev)/modelN) 
R.n<-R.cs/(1-(exp(-nullDev/modelN))) 
cat("Pseudo R^2 for logistic regression\n") 
cat("Hosmer and Lemeshow R^2",round(R.l,3),"\n") 
cat("Cox and Snell R^2",round(R.cs,3),"\n") 
cat("Nagelkerke R^2",round(R.n,3),"\n")
}


## -----------------------------------------------------------------------------
#| label: c53-pseudo-r2-calculo
LogmF <- glm(low ~ ht + lwt + race + smoke + ui, data=dat, family=binomial(logit))
LogisticPseudoR2s(LogmF)


## -----------------------------------------------------------------------------
#| label: gof-lrm
#| message: false
if (!requireNamespace("rms", quietly = TRUE)) install.packages("rms")
library(rms)
Temp <- lrm(low ~ ht + lwt + race + smoke + ui, x = TRUE, y = TRUE, data = dat)


## -----------------------------------------------------------------------------
#| label: gof-lrm-pol
#| eval: false
# Temp <- lrm(low ~ pol(age, 2) + ht + lwt + race + smoke + ui,
#             x = TRUE, y = TRUE, data = dat)


## -----------------------------------------------------------------------------
#| label: gof-resid
gof_res <- resid(Temp, "gof")
gof_res


## -----------------------------------------------------------------------------
#| label: overdisp-table
#| message: false
if (!requireNamespace("qcc", quietly = TRUE)) install.packages("qcc")
library(qcc)
table(dat$low)


## -----------------------------------------------------------------------------
#| label: overdisp-test
x <- c(0, 1)       # categorías
n <- c(130, 59)    # conteos
od_test <- qcc.overdispersion.test(x, n)


## -----------------------------------------------------------------------------
#| label: tbl-funciones-enlace
tabla_enlaces <- data.frame(
  "Modelo (distribución)" = c("Binomial, ordinal, nominal", "Binomial, ordinal",
                              "Binomial, ordinal", "Poisson", "Poisson", "Poisson"),
  "Función de enlace"     = c("Logit", "Probit", "Log-log complementario (cloglog)",
                              "Logaritmo natural", "Raíz cuadrada", "Identidad"),
  check.names = FALSE
)
knitr::kable(tabla_enlaces,
             caption = "Funciones de enlace disponibles según la distribución de la variable de respuesta.")


## -----------------------------------------------------------------------------
#| label: link-logit
modlog <- glm(low ~ ht + lwt + race + smoke + ui,
              family = binomial(link = logit), data = dat)
c(AIC = AIC(modlog), BIC = BIC(modlog))


## -----------------------------------------------------------------------------
#| label: link-probit
modprobit <- glm(low ~ ht + lwt + race + smoke + ui,
                 family = binomial(link = probit), data = dat)
c(AIC = AIC(modprobit), BIC = BIC(modprobit))


## -----------------------------------------------------------------------------
#| label: link-cloglog
modclog <- glm(low ~ ht + lwt + race + smoke + ui,
               family = binomial(link = cloglog), data = dat)
c(AIC = AIC(modclog), BIC = BIC(modclog))


## -----------------------------------------------------------------------------
#| label: link-anova
anova(modlog, modprobit, modclog, test = "Chisq")


## -----------------------------------------------------------------------------
#| label: fig-roc-logistic
#| fig-cap: "Curva ROC del modelo final de regresión logística para la predicción de bajo peso al nacer. El área bajo la curva (AUC) mide la capacidad discriminatoria del modelo: AUC = 0.5 indica discriminación al azar y AUC = 1.0 discriminación perfecta. Un AUC superior a 0.70 se considera aceptable en la práctica clínica."
#| fig-width: 5
#| fig-height: 5
#| message: false
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
library(pROC)

# Probabilidades predichas por el modelo final
prob_final <- predict(LogmF, type = "response")
roc_obj <- roc(dat$low, prob_final, levels = c("0","1"), direction = "<")
plot(roc_obj,
     col  = "#2E86C1", lwd = 2,
     main = paste0("Curva ROC — AUC = ", round(auc(roc_obj), 3)),
     xlab = "1 - Especificidad (tasa de falsos positivos)",
     ylab = "Sensibilidad (tasa de verdaderos positivos)")
abline(a = 0, b = 1, lty = 2, col = "gray50")


## -----------------------------------------------------------------------------
#| label: punto-corte
#| message: false
coords_optimas <- coords(roc_obj, "best", best.method = "youden",
                         ret = c("threshold","sensitivity","specificity"))
coords_optimas


## -----------------------------------------------------------------------------
#| label: matriz-confusion
pred_clase <- ifelse(prob_final >= coords_optimas$threshold, 1, 0)
conf_mat <- table(Predicho = pred_clase, Observado = dat$low)
knitr::kable(conf_mat,
             caption = "Matriz de confusión del modelo logístico con el punto de corte óptimo.")


## -----------------------------------------------------------------------------
#| label: tbl-compromiso-umbral
clasificacion <- function(thr) {
  cl  <- ifelse(prob_final >= thr, 1, 0)
  obs <- as.numeric(as.character(dat$low))
  c(Sensibilidad  = sum(cl == 1 & obs == 1) / sum(obs == 1),
    Especificidad = sum(cl == 0 & obs == 0) / sum(obs == 0),
    Error         = mean(cl != obs))
}
tabla_umbral <- round(rbind(
  "Umbral 0.5"    = clasificacion(0.5),
  "Umbral 0.2"    = clasificacion(0.2),
  "Umbral óptimo" = clasificacion(coords_optimas$threshold)), 3)
knitr::kable(tabla_umbral,
             caption = "Sensibilidad, especificidad y error de clasificación según el punto de corte.")


## -----------------------------------------------------------------------------
#| label: hosmer-lemeshow
#| message: false
if (!requireNamespace("ResourceSelection", quietly = TRUE)) install.packages("ResourceSelection")
library(ResourceSelection)
hl_test <- hoslem.test(as.numeric(dat$low) - 1, prob_final, g = 10)
hl_test


## -----------------------------------------------------------------------------
#| label: split-datos
#| message: false
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
library(caret)
set.seed(123)
idx <- createDataPartition(dat$low, p = 0.8, list = FALSE)
data_train <- dat[idx, ]
data_test  <- dat[-idx, ]


## -----------------------------------------------------------------------------
#| label: modelo-train
modTRAIN  <- glm(low ~ ht + lwt + race + smoke + ui,
                 data = data_train, family = binomial(logit))
predicted <- predict(modTRAIN, newdata = data_test, type = "response")


## -----------------------------------------------------------------------------
#| label: fig-roc-validacion
#| fig-cap: "Curvas ROC sobre el subconjunto de prueba (20 %) para el modelo final y para el modelo que añade la variable `age`. La escasa diferencia entre las áreas confirma que incorporar `age` no mejora de forma relevante la capacidad predictiva."
#| fig-width: 5
#| fig-height: 5
#| message: false
roc_test <- roc(data_test$low, predicted, levels = c("0","1"), direction = "<")

modTRAINage   <- glm(low ~ age + ht + lwt + race + smoke + ui,
                     data = data_train, family = binomial(logit))
predicted_age <- predict(modTRAINage, newdata = data_test, type = "response")
roc_age       <- roc(data_test$low, predicted_age, levels = c("0","1"), direction = "<")

plot(roc_test, col = "#2E86C1", lwd = 2, main = "Validación: curvas ROC en datos de prueba")
plot(roc_age,  col = "#E74C3C", lwd = 2, lty = 2, add = TRUE)
legend("bottomright", bty = "n",
       legend = c(paste0("Modelo final (AUC = ", round(auc(roc_test), 3), ")"),
                  paste0("Modelo + age (AUC = ", round(auc(roc_age), 3), ")")),
       col = c("#2E86C1","#E74C3C"), lwd = 2, lty = c(1, 2))


## -----------------------------------------------------------------------------
#| label: validacion-bootstrap
#| message: false
library(rms)
mod_rms <- lrm(low ~ ht + lwt + race + smoke + ui,
               x = TRUE, y = TRUE, data = dat)
set.seed(42)
val_boot <- validate(mod_rms, B = 200)
val_boot


## -----------------------------------------------------------------------------
#| label: confint-logOR
#| message: false
library(car)
ci_logOR <- Confint(LogmF, level = 0.95, type = "LR")
round(ci_logOR, 4)


## -----------------------------------------------------------------------------
#| label: confint-OR
round(exp(ci_logOR), 4)


## -----------------------------------------------------------------------------
#| label: predictor-lineal
b  <- coef(LogmF)
PL <- b[["(Intercept)"]] + b[["lwt"]] * 155 + b[["race3"]]   # ht=0, smoke=0, ui=0
p_low <- plogis(PL)
cat("Predictor lineal (PL):", round(PL, 4), "\n")
cat("Probabilidad de bajo peso:", round(p_low, 4), "\n")


## -----------------------------------------------------------------------------
#| label: fig-nomograma
#| fig-cap: "Nomograma del modelo final de regresión logística. Cada predictor aporta una cantidad de puntos; la suma total se traduce en la probabilidad de bajo peso al nacer. El desglose para la paciente del ejemplo (≈ 89 puntos → probabilidad ≈ 0.17) se detalla en el callout siguiente."
#| fig-width: 7
#| fig-height: 6
#| message: false
ddist <- datadist(dat)
options(datadist = "ddist")
LogmFnom <- lrm(low ~ ht + lwt + race + smoke + ui, data = dat)
nom <- nomogram(LogmFnom, fun = plogis, funlabel = "Probabilidad de bajo peso")
plot(nom)

# Puntos del paciente de ejemplo (ht=0, lwt=155, race=3, smoke=0, ui=0), usados en el callout
pts_pac <- function(comp, val) {
  d <- nom[[comp]]; xv <- d[[1]]
  if (is.numeric(xv)) approx(xv, d$points, xout = val)$y
  else d$points[match(as.character(val), as.character(xv))]
}
p_lwt  <- pts_pac("lwt", 155)
p_race <- pts_pac("race", 3)
tot_ej <- sum(c(pts_pac("ht", 0), p_lwt, p_race, pts_pac("smoke", 0), pts_pac("ui", 0)))
bnom   <- coef(LogmFnom)
p_ej   <- plogis(bnom[["Intercept"]] + bnom[["lwt"]] * 155 + bnom[["race=3"]])


## -----------------------------------------------------------------------------
#| label: prediccion-nuevo
nuevo_paciente <- data.frame(
  lwt   = 120,                                # peso materno 120 lb
  ht    = factor(0, levels = c("0","1")),     # sin hipertensión
  race  = factor(1, levels = c("1","2","3")), # raza 1
  smoke = factor(1, levels = c("0","1")),     # fumadora
  ui    = factor(0, levels = c("0","1"))      # sin irritabilidad uterina
)
prob_nuevo <- predict(LogmF, newdata = nuevo_paciente, type = "response")
cat("Probabilidad de bajo peso al nacer:", round(prob_nuevo, 4),
    "(", round(prob_nuevo * 100, 1), "%)\n")


## -----------------------------------------------------------------------------
#| label: fig-dca
#| fig-cap: "Análisis de curva de decisión del modelo final. La curva del modelo se compara con las estrategias de tratar a todos los pacientes y de no tratar a ninguno. El modelo incrementa el beneficio neto respecto a tratar a todos en el rango de umbrales considerado."
#| fig-width: 7
#| fig-height: 5
#| message: false
if (!requireNamespace("dcurves", quietly = TRUE)) install.packages("dcurves")
library(dcurves)
dat$lowpredict <- predict(LogmF, type = "response")
dca_obj <- dca(low ~ lowpredict, data = dat, thresholds = seq(0, 0.5, 0.01))
plot(dca_obj, smooth = TRUE)

