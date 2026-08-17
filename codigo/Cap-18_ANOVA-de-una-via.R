## -----------------------------------------------------------------------------
#| label: carga-anova16
dat <- read.table("sets de datos/VickersF.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

ML <- subset(dat, Group == "Multilink")
ML$Cure.time <- factor(ML$Cure.time,
                       levels = c("Control","Dual","Light(20s)"))
str(ML)


## -----------------------------------------------------------------------------
#| label: calculo-sst
grand_mean <- mean(ML$VH)
grand_var  <- var(ML$VH)
N          <- nrow(ML)
SS_T       <- grand_var * (N - 1)
cat("Grand mean:", round(grand_mean, 5), "\n")
cat("Grand variance:", round(grand_var, 5), "\n")
cat("SST:", round(SS_T, 4), "| df =", N - 1, "\n")


## -----------------------------------------------------------------------------
#| label: calculo-ssm
library(dplyr)
medias_grupos <- ML %>%
  group_by(Cure.time) %>%
  summarise(n = n(), media = mean(VH), varianza = var(VH))

knitr::kable(medias_grupos, digits = 5,
             caption = "Estadísticos por grupo para el cálculo de SSM y SSR.")

SS_M <- with(medias_grupos, sum(n * (media - grand_mean)^2))
df_M <- nrow(medias_grupos) - 1  # k - 1
cat("SSM:", round(SS_M, 5), "| df =", df_M, "\n")


## -----------------------------------------------------------------------------
#| label: calculo-ssr
SS_R <- SS_T - SS_M
df_R <- N - nrow(medias_grupos)  # N - k
cat("SSR:", round(SS_R, 5), "| df =", df_R, "\n")
# Verificación
SS_R_check <- with(medias_grupos, sum(varianza * (n - 1)))
cat("SSR (verificación directa):", round(SS_R_check, 5), "\n")


## -----------------------------------------------------------------------------
#| label: calculo-fratio
MS_M   <- SS_M / df_M
MS_R   <- SS_R / df_R
F_stat <- MS_M / MS_R
p_val  <- pf(F_stat, df_M, df_R, lower.tail = FALSE)

tabla_anova <- data.frame(
  Fuente     = c("Modelo (entre grupos)", "Residuos (dentro grupos)", "Total"),
  SS         = round(c(SS_M, SS_R, SS_T), 4),
  df         = c(df_M, df_R, N-1),
  MS         = round(c(MS_M, MS_R, NA), 4),
  F          = c(round(F_stat, 4), NA, NA),
  p          = c(round(p_val, 4), NA, NA)
)
knitr::kable(tabla_anova, na.rm = TRUE,
             caption = "Tabla ANOVA calculada manualmente.")


## -----------------------------------------------------------------------------
#| label: fig-particion-contrastes
#| fig-cap: "Partición de la varianza en el ANOVA con contrastes planeados. La varianza total (SST) se divide en la explicada por el modelo (SSM) y la no explicada (SSR). A su vez, la SSM se descompone en los contrastes ortogonales: el Contraste 1 separa el grupo Control de los experimentales (Dual + Light) y el Contraste 2 separa Dual de Light."
#| fig-width: 8
#| fig-height: 5.5
#| echo: false

par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10),
     xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

caja <- function(x1, y1, x2, y2, col, bord, txt, cex = 0.85, coltxt = "white") {
  rect(x1, y1, x2, y2, col = col, border = bord, lwd = 2)
  text((x1 + x2)/2, (y1 + y2)/2, txt, col = coltxt, font = 2, cex = cex)
}

# Nivel 1: SST
caja(3.6, 8.6, 6.4, 9.7, "#717D7E", "#566573",
     paste0("SST = ", round(SS_T, 2), "\n(Varianza total)"))
# Nivel 2: SSM y SSR
caja(0.8, 6.3, 4.0, 7.6, "#2E86C1", "#1A5276",
     paste0("SSM = ", round(SS_M, 2), "\n(Explicada por el modelo)"), cex = 0.8)
caja(6.0, 6.3, 9.2, 7.6, "#E67E22", "#9C640C",
     paste0("SSR = ", round(SS_R, 2), "\n(No explicada / error)"), cex = 0.8)
segments(4.6, 8.6, 2.4, 7.6, lwd = 1.5, col = "#566573")
segments(5.4, 8.6, 7.6, 7.6, lwd = 1.5, col = "#566573")
# Nivel 3: Contraste 1 (Control vs Dual+Light)
caja(0.2, 3.9, 2.3, 5.0, "#27AE60", "#1E8449", "Control", cex = 0.85)
caja(2.9, 3.9, 5.3, 5.0, "#27AE60", "#1E8449", "Dual + Light(20s)", cex = 0.8)
segments(2.0, 6.3, 1.25, 5.0, lwd = 1.5, col = "#1A5276")
segments(2.8, 6.3, 4.10, 5.0, lwd = 1.5, col = "#1A5276")
text(0.3, 5.55, "CONTRASTE 1", col = "#C0392B", font = 2, cex = 0.8, adj = 0)
# Nivel 4: Contraste 2 (Dual vs Light)
caja(2.7, 1.4, 3.8, 2.5, "#A9DFBF", "#1E8449", "Dual", cex = 0.8, coltxt = "#145A32")
caja(4.3, 1.4, 5.7, 2.5, "#A9DFBF", "#1E8449", "Light(20s)", cex = 0.72, coltxt = "#145A32")
segments(3.6, 3.9, 3.25, 2.5, lwd = 1.5, col = "#1E8449")
segments(4.5, 3.9, 5.00, 2.5, lwd = 1.5, col = "#1E8449")
text(2.5, 3.05, "CONTRASTE 2", col = "#C0392B", font = 2, cex = 0.8, adj = 0)


## -----------------------------------------------------------------------------
#| label: contrastes-anova
#| message: false
library(multcomp)

# Definir contrastes ortogonales
Control_vs_Exp <- c(-2,  1,  1)
Dual_vs_Light  <- c( 0, -1,  1)
contrasts(ML$Cure.time) <- cbind(Control_vs_Exp, Dual_vs_Light)

# Modelo con contrastes
mod_contr <- lm(VH ~ Cure.time, data = ML)
summary(mod_contr)


## -----------------------------------------------------------------------------
#| label: posthoc-tukey
mod_aov <- aov(VH ~ Cure.time, data = ML)
tukey   <- TukeyHSD(mod_aov)
tukey


## -----------------------------------------------------------------------------
#| label: fig-tukey
#| fig-cap: "Diferencias de medias con intervalos de confianza al 95% para las comparaciones post-hoc de Tukey HSD entre los protocolos de polimerización del cemento Multilink. Los intervalos que no incluyen 0 indican diferencias estadísticamente significativas."
#| fig-width: 6
#| fig-height: 3.5

par(mar = c(4, 12, 3, 2))
plot(tukey, las = 1, col = "#2E86C1")


## -----------------------------------------------------------------------------
#| label: tbl-comparacion-posthoc
#| tbl-cap: "Valores de p de las comparaciones por pares entre protocolos de polimerización, según el contraste planeado y cuatro métodos post-hoc. El contraste planeado solo aplica a Dual vs. Light (Contraste 2); el Contraste 1 (Control vs. experimentales) no es una comparación por pares y resultó significativo (p = 0.003)."
#| message: false

library(rstatix)

# Valores de p por par con cada método
bon  <- pairwise.t.test(ML$VH, ML$Cure.time, p.adjust.method = "bonferroni")$p.value
holm <- pairwise.t.test(ML$VH, ML$Cure.time, p.adjust.method = "holm")$p.value
tuk  <- TukeyHSD(aov(VH ~ Cure.time, data = ML))$Cure.time[, "p adj"]
gh   <- games_howell_test(ML, VH ~ Cure.time)

# p del Contraste 2 (Dual vs Light); se usa una copia para no alterar ML
MLc <- ML
contrasts(MLc$Cure.time) <- cbind(c(-2, 1, 1), c(0, -1, 1))
p_contr <- summary(lm(VH ~ Cure.time, data = MLc))$coefficients[, 4]

comparacion_ph <- data.frame(
  Comparación          = c("Control vs Dual", "Control vs Light(20s)", "Dual vs Light(20s)"),
  `Contraste planeado` = c("—", "—", sprintf("%.3f", p_contr[3])),
  Bonferroni     = c(bon["Dual", "Control"], bon["Light(20s)", "Control"], bon["Light(20s)", "Dual"]),
  `Tukey HSD`    = c(tuk["Dual-Control"], tuk["Light(20s)-Control"], tuk["Light(20s)-Dual"]),
  Holm           = c(holm["Dual", "Control"], holm["Light(20s)", "Control"], holm["Light(20s)", "Dual"]),
  `Games-Howell` = c(gh$p.adj[gh$group1 == "Control" & gh$group2 == "Dual"],
                     gh$p.adj[gh$group1 == "Control" & gh$group2 == "Light(20s)"],
                     gh$p.adj[gh$group1 == "Dual"    & gh$group2 == "Light(20s)"]),
  check.names = FALSE
)
knitr::kable(comparacion_ph, digits = 3, row.names = FALSE)

