## -----------------------------------------------------------------------------
#| label: coef-simple-multi
#| message: false

library(MASS)
data(birthwt)
dat <- birthwt
dat <- within(dat, {
  ht    <- as.factor(ht); low   <- as.factor(low)
  race  <- as.factor(race); smoke <- as.factor(smoke)
  ui    <- as.factor(ui)
})

# Modelo simple vs. múltiple
mod_simple <- lm(bwt ~ lwt, data = dat)
mod_multi  <- lm(bwt ~ lwt + age + smoke, data = dat)

cat("=== Regresión simple: bwt ~ lwt ===\n")
cat("b(lwt) =", round(coef(mod_simple)["lwt"], 3), "\n\n")

cat("=== Regresión múltiple: bwt ~ lwt + age + smoke ===\n")
cat("b(lwt) =",   round(coef(mod_multi)["lwt"],    3), "\n")
cat("b(age) =",   round(coef(mod_multi)["age"],    3), "\n")
cat("b(smoke1) =", round(coef(mod_multi)["smoke1"], 3), "\n")


## -----------------------------------------------------------------------------
#| label: fig-plano-regresion
#| fig-cap: "Plano de regresión ajustado para el peso al nacer (`bwt`) en función de dos predictores continuos: el peso de la madre (`lwt`) y su edad (`age`). La superficie azul representa los valores predichos por el modelo y los puntos rojos son las observaciones. La pendiente del plano a lo largo de cada eje corresponde al coeficiente del predictor respectivo, manteniendo constante el otro."
#| fig-width: 6.5
#| fig-height: 5.5
#| message: false

mod_plano <- lm(bwt ~ lwt + age, data = dat)

# Malla de predicción sobre los dos predictores continuos
lwt_seq <- seq(min(dat$lwt), max(dat$lwt), length.out = 20)
age_seq <- seq(min(dat$age), max(dat$age), length.out = 20)
grid    <- expand.grid(lwt = lwt_seq, age = age_seq)
z       <- matrix(predict(mod_plano, grid), nrow = length(lwt_seq))

par(mar = c(1.5, 1.5, 1, 1.5))
pmat <- persp(lwt_seq, age_seq, z, theta = 40, phi = 20,
              col = "#AED6F1", border = "gray75", shade = 0.35, expand = 0.65,
              ticktype = "detailed", nticks = 4, cex.axis = 0.6,
              xlab = "lwt (peso madre)", ylab = "age (edad)", zlab = "bwt (peso RN)")

# Proyectar las observaciones sobre el plano de perspectiva
obs <- trans3d(dat$lwt, dat$age, dat$bwt, pmat)
points(obs, pch = 16, col = "#E74C3C", cex = 0.5)


## -----------------------------------------------------------------------------
#| label: vif-check
library(car)
mod_full <- lm(bwt ~ age + ftv + ht + lwt + ptl + race + smoke + ui, data = dat)
cat("=== VIF del modelo completo ===\n")
vif(mod_full)


## -----------------------------------------------------------------------------
#| label: seleccion-aic
# Comparación de modelos por AIC y BIC
mod1 <- lm(bwt ~ ht + lwt + race + smoke + ui, data = dat)
mod2 <- lm(bwt ~ age + ht + lwt + race + smoke + ui, data = dat)
mod3 <- lm(bwt ~ age + I(age^2) + ht + lwt + race + smoke + ui, data = dat)

AIC_tab <- AIC(mod1, mod2, mod3)
BIC_tab <- BIC(mod1, mod2, mod3)

knitr::kable(cbind(Modelo = c("sin age","con age","con age+age²"),
                   AIC=AIC_tab[,-1], BIC = BIC_tab[,2]),
             caption = "Comparación de modelos por AIC y BIC. Menores valores indican mejor ajuste penalizado por complejidad.")


## -----------------------------------------------------------------------------
#| label: fig-poly-regression
#| fig-cap: "Relación entre el peso pregestacional de la madre (lwt) y el peso del recién nacido (bwt). La línea azul representa el ajuste lineal univariado; la línea naranja representa el ajuste cuadrático (con lwt²). La adición del término cuadrático captura mejor la relación no lineal en el rango de pesos extremos."
#| fig-width: 7
#| fig-height: 4
#| message: false

library(ggplot2)
ggplot(dat, aes(lwt, bwt)) +
  geom_point(alpha = 0.4, colour = "gray40") +
  geom_smooth(method = "lm", formula = y ~ x,
              se = TRUE, colour = "#2E86C1", linewidth = 1.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              se = TRUE, colour = "#E67E22", lty = 2, linewidth = 1.2) +
  labs(x = "Peso de la madre antes del embarazo (lb)",
       y = "Peso del recién nacido (g)") +
  annotate("text", x = 200, y = 4200,
           label = "— Lineal\n-- Cuadrático", hjust = 0,
           colour = "gray30", size = 3.5) +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: fig-tendencias-polinomicas
#| fig-cap: "Formas características de las tendencias polinómicas. De izquierda a derecha: lineal (sin cambios de dirección), cuadrática (un cambio), cúbica (dos cambios) y cuártica (tres cambios). Cada término polinómico adicional añade un posible cambio de dirección en la relación entre el predictor y la respuesta."
#| fig-width: 9
#| fig-height: 2.8
#| echo: false

par(mfrow = c(1, 4), mar = c(2, 2, 3, 1))
x <- seq(-2.2, 2.2, length.out = 300)
plot(x, x,             type = "l", lwd = 2.5, col = "#2E86C1", main = "Lineal",     xlab = "", ylab = "", axes = FALSE); box()
plot(x, x^2,           type = "l", lwd = 2.5, col = "#27AE60", main = "Cuadrática", xlab = "", ylab = "", axes = FALSE); box()
plot(x, x^3 - 3 * x,   type = "l", lwd = 2.5, col = "#E67E22", main = "Cúbica",     xlab = "", ylab = "", axes = FALSE); box()
plot(x, x^4/4 - x^2/2, type = "l", lwd = 2.5, col = "#8E44AD", main = "Cuártica",   xlab = "", ylab = "", axes = FALSE); box()
par(mfrow = c(1, 1))

