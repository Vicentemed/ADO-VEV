## -----------------------------------------------------------------------------
#| label: prep-mlr51
#| message: false
library(MASS); library(VIM); library(car); library(ggplot2); library(dplyr)

data(birthwt)
dat <- birthwt
dat <- within(dat, {
  ht    <- as.factor(ht); low   <- as.factor(low)
  race  <- as.factor(race); smoke <- as.factor(smoke)
  ui    <- as.factor(ui)
})
str(dat)
summary(dat)


## -----------------------------------------------------------------------------
#| label: c51-tabla-por-low
library(table1)
T1 <- table1(~ age + ftv + lwt + ptl + ht + race + smoke + ui | low, data = dat)
T1



## -----------------------------------------------------------------------------
#| label: c51-tabla-por-smoke
library(table1)
T2 <- table1(~ age + ftv + lwt + ptl + ht + race + low + ui | smoke, data = dat)
T2


## -----------------------------------------------------------------------------
#| label: fig-eda-mlr
#| fig-cap: "Relación entre el peso de la madre (lwt) y el peso del recién nacido (bwt) por estado de tabaquismo. Las madres fumadoras (smoke=1) tienden a tener recién nacidos con menor peso, independientemente del peso materno."
#| fig-width: 7
#| fig-height: 4

ggplot(dat, aes(lwt, bwt, colour = smoke)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1) +
  scale_colour_manual(values = c("#2E86C1","#E74C3C"),
                      labels = c("No fumadora","Fumadora")) +
  labs(x = "Peso materno (lb)", y = "Peso RN (g)", colour = "Tabaquismo") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: modelo-mlr51
mod_full <- lm(bwt ~ age + ftv + ht + lwt + ptl + race + smoke + ui, data = dat)
summary(mod_full)


## -----------------------------------------------------------------------------
#| label: c51-confint-full
confint(mod_full)


## -----------------------------------------------------------------------------
#| label: diag-obs
#| echo: false
# Observaciones más extremas del modelo (usadas en el texto mediante código en línea)
res_top  <- sort(as.numeric(names(sort(abs(residuals(mod_full)), decreasing = TRUE))[1:3]))
cook_top <- sort(as.numeric(names(sort(cooks.distance(mod_full),  decreasing = TRUE))[1:3]))


## -----------------------------------------------------------------------------
#| label: fig-diagnosticos-mlr-1
#| fig-cap: "Gráficos diagnósticos del modelo de regresión lineal múltiple. Residuals vs. Fitted: verifica linealidad y homocedasticidad"
#| fig-width: 8
#| fig-height: 7

plot(mod_full, 1)


## -----------------------------------------------------------------------------
#| label: fig-diagnosticos-mlr-2
#| fig-cap: "Gráficos diagnósticos del modelo de regresión lineal múltiple. Normal Q-Q: normalidad de residuos"
#| fig-width: 8
#| fig-height: 7

plot(mod_full, 2)


## -----------------------------------------------------------------------------
#| label: fig-diagnosticos-mlr-3
#| fig-cap: "Gráficos diagnósticos del modelo de regresión lineal múltiple. Scale-Location: homocedasticidad"
#| fig-width: 8
#| fig-height: 7

plot(mod_full, 3)


## -----------------------------------------------------------------------------
#| label: fig-diagnosticos-mlr-5
#| fig-cap: "Gráficos diagnósticos del modelo de regresión lineal múltiple. Residuals vs. Leverage: identifica observaciones influyentes."
#| fig-width: 8
#| fig-height: 7

plot(mod_full, 5)


## -----------------------------------------------------------------------------
#| label: fig-diagnosticos-mlr-4
#| fig-cap: "Gráficos diagnósticos del modelo de regresión lineal múltiple. Cook's distance: identifica observaciones influyentes."
#| fig-width: 8
#| fig-height: 7

plot(mod_full, 4)


## -----------------------------------------------------------------------------
#| label: c51-dw-vif
# Test de Durbin-Watson para autocorrelación de residuos
durbinWatsonTest(mod_full)

# VIF para multicolinealidad
vif(mod_full)


## -----------------------------------------------------------------------------
#| label: c51-dffits
dat[which(dffits(mod_full) > 0.5),]


## -----------------------------------------------------------------------------
#| label: c51-valores-unicos
length(unique(dat$age))
length(unique(dat$ftv))
length(unique(dat$lwt))
length(unique(dat$ptl))


## -----------------------------------------------------------------------------
#| label: gam-tmp
#| message: false
library(gam)
tmp <- gam(bwt ~ s(age) + ftv + ht + s(lwt) + ptl + race + smoke + ui, data = dat)
summary(tmp)


## -----------------------------------------------------------------------------
#| label: fig-gam-suavizado
#| fig-cap: "Efectos parciales suavizados de los predictores continuos `age` y `lwt` en el modelo aditivo generalizado (GAM). La línea continua es el suavizado estimado con `s()` y las líneas punteadas representan su intervalo de confianza; las marcas del eje inferior (*rug*) indican la distribución de las observaciones. Una curvatura marcada sugiere una relación no lineal con el peso al nacer."
#| fig-width: 8
#| fig-height: 4

par(mfrow = c(1, 2))
plot(tmp, se = TRUE, terms = c("s(age)", "s(lwt)"))


## -----------------------------------------------------------------------------
#| label: fig-poly-age
#| fig-cap: "Relación bivariada entre la edad de la madre (`age`) y el peso al nacer (`bwt`), con ajuste lineal y cuadrático. El $R^2$ de cada tendencia (entre paréntesis en la leyenda) indica la varianza explicada; ambos valores son muy bajos y difieren entre sí."
#| fig-width: 6.5
#| fig-height: 4.5
#| message: false

# R² de cada ajuste bivariado
r2 <- function(f) round(summary(lm(f, data = dat))$r.squared, 3)

ggplot(dat, aes(age, bwt)) +
  geom_point(alpha = 0.35, colour = "gray55") +
  geom_smooth(aes(colour = "Lineal"),     method = "lm", formula = y ~ x,          se = FALSE, linewidth = 1.1) +
  geom_smooth(aes(colour = "Cuadrática"), method = "lm", formula = y ~ poly(x, 2), se = FALSE, linewidth = 1.1) +
  scale_colour_manual(name = "Tendencia (R²)",
                      breaks = c("Lineal", "Cuadrática"),
                      values = c("Lineal" = "#2E86C1", "Cuadrática" = "#E67E22"),
                      labels = c(paste0("Lineal (R² = ",     r2(bwt ~ age), ")"),
                                 paste0("Cuadrática (R² = ", r2(bwt ~ age + I(age^2)), ")"))) +
  labs(x = "Edad de la madre (años)", y = "Peso del recién nacido (g)") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: fig-poly-lwt
#| fig-cap: "Relación bivariada entre el peso de la madre (`lwt`) y el peso al nacer (`bwt`), con ajustes de grado creciente (de lineal a cuártico). El $R^2$ aparece en la leyenda: el aumento del grado polinómico apenas incrementa la varianza explicada, y de la tendencia cúbica a la cuártica el cambio es prácticamente nulo."
#| fig-width: 6.5
#| fig-height: 4.5
#| message: false

ggplot(dat, aes(lwt, bwt)) +
  geom_point(alpha = 0.35, colour = "gray55") +
  geom_smooth(aes(colour = "Lineal"),     method = "lm", formula = y ~ x,          se = FALSE, linewidth = 1) +
  geom_smooth(aes(colour = "Cuadrática"), method = "lm", formula = y ~ poly(x, 2), se = FALSE, linewidth = 1) +
  geom_smooth(aes(colour = "Cúbica"),     method = "lm", formula = y ~ poly(x, 3), se = FALSE, linewidth = 1) +
  geom_smooth(aes(colour = "Cuártica"),   method = "lm", formula = y ~ poly(x, 4), se = FALSE, linewidth = 1) +
  scale_colour_manual(name = "Tendencia (R²)",
                      breaks = c("Lineal", "Cuadrática", "Cúbica", "Cuártica"),
                      values = c("Lineal" = "#2E86C1", "Cuadrática" = "#27AE60",
                                 "Cúbica" = "#E67E22", "Cuártica" = "#8E44AD"),
                      labels = c(paste0("Lineal (R² = ",     r2(bwt ~ lwt), ")"),
                                 paste0("Cuadrática (R² = ", r2(bwt ~ lwt + I(lwt^2)), ")"),
                                 paste0("Cúbica (R² = ",     r2(bwt ~ lwt + I(lwt^2) + I(lwt^3)), ")"),
                                 paste0("Cuártica (R² = ",   r2(bwt ~ lwt + I(lwt^2) + I(lwt^3) + I(lwt^4)), ")"))) +
  labs(x = "Peso de la madre (lb)", y = "Peso del recién nacido (g)") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: centrar-vars
dat$age_c <- dat$age - mean(dat$age)
dat$lwt_c <- dat$lwt - mean(dat$lwt)

mod_centrado <- lm(bwt ~ age_c + I(age_c^2) + ht + lwt_c + race + smoke + ui, data = dat)
summary(mod_centrado)


## -----------------------------------------------------------------------------
#| label: fig-centrado-lwt
#| fig-cap: "Relación entre el peso de la madre (`lwt`) y el peso al nacer (`bwt`) antes (izquierda) y después (derecha) de centrar `lwt`. La pendiente y la dispersión de los datos son idénticas; el centrado solo desplaza el eje horizontal para que el 0 corresponda a la media (línea discontinua), sin alterar el ajuste del modelo."
#| fig-width: 9
#| fig-height: 3.8
#| message: false

library(patchwork)

p_raw <- ggplot(dat, aes(lwt, bwt)) +
  geom_point(alpha = 0.35, colour = "gray55") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#2E86C1") +
  labs(title = "Sin centrar", x = "lwt (lb)", y = "bwt (g)") +
  theme_bw(base_size = 11)

p_cen <- ggplot(dat, aes(lwt_c, bwt)) +
  geom_point(alpha = 0.35, colour = "gray55") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#E67E22") +
  geom_vline(xintercept = 0, lty = 2, colour = "gray40") +
  labs(title = "Centrada (media = 0)", x = "lwt centrada (lb)", y = NULL) +
  theme_bw(base_size = 11)

p_raw | p_cen


## -----------------------------------------------------------------------------
#| label: simp-saturado
mod <- lm(bwt ~ age_c + I(age_c^2) + ftv + ht + lwt_c + I(lwt_c^2) +
            I(lwt_c^3) + I(lwt_c^4) + ptl + race + smoke + ui, data = dat)
Anova(mod)


## -----------------------------------------------------------------------------
#| label: simp-modS
modS <- lm(bwt ~ age_c + I(age_c^2) + ht + lwt_c + race + smoke + ui, data = dat)
Anova(modS)


## -----------------------------------------------------------------------------
#| label: simp-modB
modB <- lm(bwt ~ 1, data = dat)
anova(modB, modS)
AIC(modB, modS)
BIC(modB, modS)


## -----------------------------------------------------------------------------
#| label: simp-age
modS1 <- lm(bwt ~ ht + lwt_c + race + smoke + ui, data = dat)
anova(modS1, modS)
AIC(modS1, modS)


## -----------------------------------------------------------------------------
#| label: simp-summary-modS
summary(modS)


## -----------------------------------------------------------------------------
#| label: simp-modF
modF <- lm(bwt ~ age + I(age^2) + ht + lwt + race + smoke + ui, data = dat)
summary(modF)


## -----------------------------------------------------------------------------
#| label: simp-leaps
#| message: false
library(leaps)
leaps_mod <- regsubsets(bwt ~ age_c + I(age_c^2) + ftv + ht + lwt_c + I(lwt_c^2) +
                          I(lwt_c^3) + I(lwt_c^4) + ptl + race + smoke + ui,
                        data = dat, nbest = 5, nvmax = 10)
res_sum <- summary(leaps_mod)
res_sum



## -----------------------------------------------------------------------------
#| label: c51-mejor-modelo-criterio
# Índice del mejor modelo según cada criterio
data.frame(
  Adj.R2 = which.max(res_sum$adjr2),
  CP     = which.min(res_sum$cp),
  BIC    = which.min(res_sum$bic)
)


## -----------------------------------------------------------------------------
#| label: simp-comparacion
comparar <- function(f) {
  m <- lm(f, data = dat)
  c(Adj.R2 = round(summary(m)$adj.r.squared, 4), AIC = round(AIC(m), 3))
}

tabla_comp <- rbind(
  "modF (backward)"     = comparar(bwt ~ age + I(age^2) + ht + lwt + race + smoke + ui),
  "modR (+ lwt²)"       = comparar(bwt ~ age + I(age^2) + ht + lwt + I(lwt^2) + race + smoke + ui),
  "modC (reducido)"     = comparar(bwt ~ ht + race + smoke + ui),
  "modBI (menor BIC)"   = comparar(bwt ~ ht + I(lwt^3) + race + smoke + ui)
)
knitr::kable(tabla_comp,
             caption = "Comparación de modelos candidatos por R² ajustado y AIC (escala original).")


## -----------------------------------------------------------------------------
#| label: seleccion-modelo51
# Comparar modelos
mod1 <- lm(bwt ~ age + I(age^2) + ht + lwt + race + smoke + ui, data = dat)
mod2 <- lm(bwt ~ age + I(age^2) + ht + lwt + I(lwt^2) + race + smoke + ui, data = dat)
mod3 <- lm(bwt ~ ht + lwt + race + smoke + ui, data = dat)

# Prueba de razón de verosimilitud entre modelos anidados
anova(mod1, mod2, mod3)


## -----------------------------------------------------------------------------
#| label: c51-modelo-final
# Modelo final elegido
mod_final <- lm(bwt ~ ht + lwt + race + smoke + ui, data = dat)
summary(mod_final)
confint(mod_final)


## -----------------------------------------------------------------------------
#| label: dw-final
#| message: false
set.seed(123)
dw_final <- durbinWatsonTest(mod_final)
dw_final


## -----------------------------------------------------------------------------
#| label: fig-influencia
#| fig-cap: "Distancia de Cook para cada observación del modelo final. Las barras más largas indican observaciones con mayor influencia en los coeficientes del modelo. Como regla práctica, los valores de Cook > 1 merecen revisión."
#| fig-width: 7
#| fig-height: 4

plot(mod_final, 4, id.n = 5)


## -----------------------------------------------------------------------------
#| label: dat-id
#| message: false
dat$id<-c(1:189)
str(dat)
head(dat)


## -----------------------------------------------------------------------------
#| label: dat-SIN91
#| message: false
datSIN91 <- dat[-(6),]
head(datSIN91)


## -----------------------------------------------------------------------------
#| label: split-particion
#| message: false
library(caret)
set.seed(123)

idx_train  <- createDataPartition(dat$bwt, p = 0.8, list = FALSE)
data_train <- dat[idx_train, ]
data_test  <- dat[-idx_train, ]


## -----------------------------------------------------------------------------
#| label: split-pred
modTRAIN    <- lm(bwt ~ age + I(age^2) + ht + lwt + race + smoke + ui, data = data_train)
predictions <- predict(modTRAIN, data_test)


## -----------------------------------------------------------------------------
#| label: split-metricas
modNF1 <- lm(bwt ~ ht + lwt + race + smoke + ui, data = data_train)

metricas <- function(modelo) {
  pred <- predict(modelo, data_test)
  obs  <- data_test$bwt
  c(R2   = round(R2(pred, obs), 4),
    RMSE = round(RMSE(pred, obs), 1),
    MAE  = round(MAE(pred, obs), 1),
    PER  = round(RMSE(pred, obs) / mean(obs), 4))
}

tabla_val <- rbind(modF = metricas(modTRAIN), modNF1 = metricas(modNF1))
knitr::kable(tabla_val,
             caption = "Desempeño predictivo sobre el conjunto de prueba (partición 80/20).")


## -----------------------------------------------------------------------------
#| label: split-cv-repetida
#| message: false
ctrl_rep <- trainControl(method = "repeatedcv", number = 10, repeats = 20)

set.seed(123)
cv_conAge <- train(bwt ~ age + I(age^2) + ht + lwt + race + smoke + ui,
                   data = dat, method = "lm", trControl = ctrl_rep)
set.seed(123)
cv_sinAge <- train(bwt ~ ht + lwt + race + smoke + ui,
                   data = dat, method = "lm", trControl = ctrl_rep)

tabla_cv <- rbind(
  "con age + age²" = round(cv_conAge$results[, c("RMSE", "Rsquared", "MAE")], 3),
  "sin age"        = round(cv_sinAge$results[, c("RMSE", "Rsquared", "MAE")], 3)
)
knitr::kable(tabla_cv,
             caption = "Validación cruzada repetida (10 particiones × 20 repeticiones): error de predicción promedio de cada modelo. Cuanto menor el RMSE y mayor el R², mejor.")


## -----------------------------------------------------------------------------
#| label: validacion-mlr
#| message: false
library(caret)
set.seed(123)

# Validación cruzada k-fold (k=10)
train_ctrl <- trainControl(method = "cv", number = 10)
mod_cv <- train(bwt ~ ht + lwt + race + smoke + ui,
                data    = dat,
                method  = "lm",
                trControl = train_ctrl)

cat("=== Métricas de validación cruzada (10-fold) ===\n")
print(mod_cv$results[, c("RMSE","Rsquared","MAE")])
cat("Error porcentual (RMSE/media):",
    round(mod_cv$results$RMSE / mean(dat$bwt) * 100, 2), "%\n")


## -----------------------------------------------------------------------------
#| label: efecto-mlr
#| message: false
if (!requireNamespace("lsr", quietly = TRUE)) install.packages("lsr")
library(lsr)

# Eta cuadrado (proporción de varianza explicada por cada término)
etaSquared(mod_final)

# La suma de los eta² (secuencial) equivale al R² múltiple del modelo
cat("Suma de η² =", round(sum(etaSquared(mod_final, type = 1)[, "eta.sq"]), 4), "\n")
cat("R² del modelo =", round(summary(mod_final)$r.squared, 4), "\n")


## -----------------------------------------------------------------------------
#| label: pred-xtest
x_test <- data_test[, c("ht", "lwt", "race", "smoke", "ui")]
head(x_test)


## -----------------------------------------------------------------------------
#| label: prediccion-nuevos
pred_ic <- predict(mod_final, newdata = x_test, interval = "confidence")
head(pred_ic)


## -----------------------------------------------------------------------------
#| label: reporte-valores
#| echo: false
# Valores del reporte, calculados del modelo final (código en línea en el texto)
.fs         <- summary(mod_final)$fstatistic
rep_F       <- round(unname(.fs[1]), 2)
rep_df1     <- unname(.fs[2])
rep_df2     <- unname(.fs[3])
rep_R2      <- round(summary(mod_final)$r.squared, 3)
rep_aR2     <- round(summary(mod_final)$adj.r.squared, 3)
rep_vif     <- round(max(vif(mod_final)[, "GVIF"]), 2)
rep_dw      <- round(dw_final$dw, 2)
rep_cv_RMSE <- round(mod_cv$results$RMSE, 1)
rep_cv_R2   <- round(mod_cv$results$Rsquared, 3)
rep_cv_MAE  <- round(mod_cv$results$MAE, 1)
rep_sp_R2   <- round(tabla_val["modNF1", "R2"], 3)
rep_sp_MAE  <- round(tabla_val["modNF1", "MAE"], 1)

