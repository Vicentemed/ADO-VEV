## -----------------------------------------------------------------------------
#| label: calculo-covarianza
Age             <- c(22, 24, 35, 48, 49)
NSAIDConsumption <- c(2,  3,  5, 11, 13)

# Media de cada variable
cat("Media de Edad:", mean(Age), "\n")
cat("Media de NSAIDs:", mean(NSAIDConsumption), "\n")

# Desviaciones de cada variable respecto a su media
A1 <- Age - mean(Age)
B1 <- NSAIDConsumption - mean(NSAIDConsumption)

data.frame(
  Edad   = Age,
  NSAIDs = NSAIDConsumption,
  Dev_Edad   = round(A1, 2),
  Dev_NSAIDs = round(B1, 2),
  Producto   = round(A1 * B1, 2)
)


## -----------------------------------------------------------------------------
#| label: c08-covarianza-funcion
# Covarianza manual
cov_manual <- sum(A1 * B1) / (length(Age) - 1)
cat("Covarianza (manual):", round(cov_manual, 3), "\n")

# Función cov()
cat("Covarianza (función cov):", round(cov(Age, NSAIDConsumption), 3), "\n")


## -----------------------------------------------------------------------------
#| label: calculo-pearson
# Cálculo manual de r
sd_edad  <- sd(Age)
sd_nsaid <- sd(NSAIDConsumption)
r_manual <- cov_manual / (sd_edad * sd_nsaid)
cat("DE Edad:", round(sd_edad, 3), "\n")
cat("DE NSAIDs:", round(sd_nsaid, 3), "\n")
cat("r de Pearson (manual):", round(r_manual, 4), "\n")

# Función cor()
cat("r de Pearson (cor):", round(cor(Age, NSAIDConsumption, method = "pearson"), 4), "\n")


## -----------------------------------------------------------------------------
#| label: fig-scatter-pearson
#| fig-cap: "Relación entre la edad y el consumo de AINEs en 5 pacientes. La línea de regresión con banda de confianza al 95% ilustra la fuerte correlación positiva (r = 0.98) entre ambas variables. Esta asociación sugiere que a mayor edad, mayor consumo de antiinflamatorios."
#| fig-width: 5
#| fig-height: 4
#| message: false

library(ggplot2)
Pat <- data.frame(Edad = Age, Consumo_AINEs = NSAIDConsumption)

ggplot(Pat, aes(Edad, Consumo_AINEs)) +
  geom_point(size = 3, colour = "#2E86C1") +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, colour = "#E74C3C") +
  labs(x = "Edad (años)",
       y = "Consumo de AINEs (tabletas/mes)") +
  annotate("text", x = 25, y = 12,
           label = paste0("r = ", round(cor(Age, NSAIDConsumption), 2)),
           size = 4.5, colour = "#2E86C1", fontface = "bold") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: pearson-test
with(Pat, cor.test(Edad, Consumo_AINEs,
                   alternative = "two.sided",
                   method      = "pearson"))


## -----------------------------------------------------------------------------
#| label: imc-tqx
dat_dx <- read.table("Examples/DXKTw.txt", header = TRUE, sep = "\t",
                      na.strings = "NA", dec = ".", strip.white = TRUE)
str(dat_dx)


## -----------------------------------------------------------------------------
#| label: fig-qq-dx
#| fig-cap: "Gráficos Q-Q para el IMC y el tiempo quirúrgico en pacientes sometidos a la remoción de terceros molares mandibulares. Los puntos dentro de la banda de confianza al 95% (área sombreada) indican que ambas variables se aproximan a la distribución normal y cumplen el supuesto para la correlación de Pearson."
#| fig-width: 9
#| fig-height: 4
#| message: false

library(car)
par(mfrow = c(1, 2))
qqPlot(dat_dx$Imc, main = "Q-Q: IMC",
       xlab = "Cuantiles teóricos", ylab = "Cuantiles observados",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")
qqPlot(dat_dx$Tqx, main = "Q-Q: Tiempo Quirúrgico",
       xlab = "Cuantiles teóricos", ylab = "Cuantiles observados",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")
par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| label: pearson-dx
with(dat_dx, cor.test(Imc, Tqx,
                      alternative = "two.sided",
                      method      = "pearson"))


## -----------------------------------------------------------------------------
#| label: fig-scatter-pearson-dx
#| fig-cap: "Relación entre el IMC y el tiempo quirúrgico en la remoción de terceros molares mandibulares. La nube de puntos y la recta de regresión (con banda de confianza al 95%) ilustran la ausencia de asociación lineal entre ambas variables (r ≈ 0.03)."
#| fig-width: 6
#| fig-height: 4
#| message: false

# Visualización con jitter para mostrar la distribución de puntos
ggplot(dat_dx, aes(Imc, Tqx)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5, colour = "#2E86C1", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#E74C3C", alpha = 0.15) +
  labs(x = "IMC (kg/m²)", y = "Tiempo quirúrgico (min)") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: carga-cron
dat_cron <- read.table("Examples/datcron.txt", header = TRUE, sep = "\t",
                       na.strings = "NA", dec = ".", strip.white = TRUE)
head(dat_cron)


## -----------------------------------------------------------------------------
#| label: fig-qq-cron
#| fig-cap: "Gráficos Q-Q para la edad dental y edad cronológica. Ambas variables muestran desviaciones importantes — los puntos fuera de la banda de confianza indican distribución no normal, lo que requiere el uso de la correlación de Spearman."
#| fig-width: 9
#| fig-height: 4

par(mfrow = c(1, 2))
qqPlot(dat_cron$Dental_Age, main = "Q-Q: Edad Dental",
       xlab = "Cuantiles teóricos", ylab = "Cuantiles observados",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")
qqPlot(dat_cron$Cron_Age, main = "Q-Q: Edad Cronológica",
       xlab = "Cuantiles teóricos", ylab = "Cuantiles observados",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")
par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| label: spearman-cron
with(dat_cron, cor.test(Dental_Age, Cron_Age,
                        alternative = "two.sided",
                        method      = "spearman"))


## -----------------------------------------------------------------------------
#| label: fig-scatter-spearman
#| fig-cap: "Relación entre la edad dental y la edad cronológica (Reverte-Salazar et al., 2019). La fuerte asociación monótona positiva (ρ ≈ 0.86) justifica el uso de la correlación de Spearman."
#| fig-width: 6
#| fig-height: 4
#| message: false

# Visualización con jitter para mostrar la distribución de puntos
ggplot(dat_cron, aes(Dental_Age, Cron_Age)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.4, colour = "#2E86C1", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, colour = "#E74C3C", alpha = 0.15) +
  labs(x = "Edad dental (meses)", y = "Edad cronológica (meses)") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: kendall-vs-spearman
# Correlación de Kendall
kendall_res <- with(dat_cron,
                    cor.test(Cron_Age, Bone_Maturation,
                             alternative = "two.sided",
                             method      = "kendall"))

# Correlación de Spearman (comparativa)
spearman_res <- with(dat_cron,
                     cor.test(Cron_Age, Bone_Maturation,
                              alternative = "two.sided",
                              method      = "spearman"))

cat("Kendall tau:", round(kendall_res$estimate, 4),
    " | p =", round(kendall_res$p.value, 4), "\n")
cat("Spearman rho:", round(spearman_res$estimate, 4),
    " | p =", round(spearman_res$p.value, 4), "\n")


## -----------------------------------------------------------------------------
#| label: fig-scatter-kendall
#| fig-cap: "Relación entre la edad cronológica y el estadio de maduración ósea vertebral en 516 pacientes (Reverte-Salazar et al., 2019). Dado que la maduración ósea es una variable ordinal con solo 5 niveles y presenta muchos empates, la correlación de Kendall (τ = 0.59) es más apropiada que la de Spearman (ρ = 0.72) para cuantificar la asociación."
#| fig-width: 6
#| fig-height: 4
#| message: false

# Visualización con jitter para mostrar la distribución de puntos
ggplot(dat_cron, aes(Cron_Age, Bone_Maturation)) +
  geom_jitter(height = 0.15, alpha = 0.3, colour = "#2E86C1", size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "#E74C3C", alpha = 0.15) +
  scale_y_continuous(breaks = 1:5) +
  labs(x = "Edad Cronológica (meses)",
       y = "Estadio de Maduración Ósea (1–5)",
       caption = "Datos: Reverte-Salazar et al. (2019)") +
  annotate("text", x = min(dat_cron$Cron_Age, na.rm=T) + 10,
           y = 4.5,
           label = paste0("Kendall τ = ", round(kendall_res$estimate, 2),
                          "\nSpearman ρ = ", round(spearman_res$estimate, 2)),
           hjust = 0, size = 3.5, colour = "#333333") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: cor-tqx-imc
# Correlación bivariada sin ajuste
with(dat_dx, cor.test(Tqx, Imc,
                      alternative = "two.sided",
                      method      = "pearson"))


## -----------------------------------------------------------------------------
#| label: pcor-tqx-imc
#| message: false
if (!requireNamespace("ggm", quietly=TRUE)) install.packages("ggm")
library(ggm)

# Matriz de covarianzas de las variables numéricas de interés
S <- var(dat_dx[, c("Tqx", "Imc", "Ed")])

# Correlación parcial: Tqx ~ Imc controlando la edad (Ed)
r_parcial <- pcor(c("Tqx", "Imc", "Ed"), S)
cat("r parcial (Tqx ~ Imc | Ed):", round(r_parcial, 4), "\n")


## -----------------------------------------------------------------------------
#| label: cor-edad
# Relación de la edad (Ed) con cada variable
c1 <- with(dat_dx, cor.test(Tqx, Ed, method = "pearson"))
c2 <- with(dat_dx, cor.test(Imc, Ed, method = "pearson"))
cat("Tqx ~ Ed: r =", round(c1$estimate, 3), ", p =", round(c1$p.value, 3), "\n")
cat("Imc ~ Ed: r =", round(c2$estimate, 3), ", p =", round(c2$p.value, 3), "\n")


## -----------------------------------------------------------------------------
#| label: pps-classcx
#| message: false
library(ppsr)

# PPS: la clasificación quirúrgica (categórica) como predictora del tiempo quirúrgico
as.data.frame(score(dat_dx, x = "Classcx", y = "Tqx")[c("x", "y", "pps")])


## -----------------------------------------------------------------------------
#| label: pps-asimetria
#| eval: false
# score(dat_dx, x = "Classcx", y = "Tqx")$pps   # clasificación -> tiempo
# score(dat_dx, x = "Tqx", y = "Classcx")$pps   # tiempo -> clasificación


## -----------------------------------------------------------------------------
#| label: fig-pps-tqx
#| fig-cap: "Predictive Power Score de distintas variables para predecir el tiempo quirúrgico (Tqx). A diferencia de la correlación, el PPS incorpora predictores categóricos como la clasificación quirúrgica (Classcx) o el sexo, y detecta relaciones no lineales."
#| fig-width: 6
#| fig-height: 4
#| message: false

dat_pps <- dat_dx[, c("Tqx", "Classcx", "Imc", "Ed", "Sexo", "Grupo")]
visualize_pps(dat_pps, y = "Tqx") +
  theme_bw(base_size = 12)

