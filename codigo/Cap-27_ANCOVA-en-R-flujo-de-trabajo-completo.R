## -----------------------------------------------------------------------------
#| label: carga-ancova25
library(car); library(ggplot2); library(dplyr)

dat <- read.table("sets de datos/datBF.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

# Establecer G2 como grupo control (referencia)
dat$Group <- factor(dat$Group, levels = c("G2","G1","G3"))
str(dat)


## -----------------------------------------------------------------------------
#| label: fig-boxplot-ancova
#| fig-cap: "Distribución de la fluorescencia final (UF) por grupo de tratamiento. El grupo control (G2) presenta valores consistentemente más altos que los grupos experimentales (G1 y G3), sugiriendo que los tratamientos de remineralización reducen la fluorescencia de las lesiones."
#| fig-width: 6
#| fig-height: 5

ggplot(dat, aes(Group, Final, fill = Group)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 2) +
  scale_fill_manual(values = c("#555555","#2E86C1","#E74C3C")) +
  labs(x = "Grupo de tratamiento",
       y = "Fluorescencia final (UF)",
       fill = "Grupo") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: c25-descriptiva-tabla
dat %>%
  group_by(Group) %>%
  summarise(n = n(),
            Basal_Media = round(mean(Basal, na.rm=T), 3),
            Basal_DE    = round(sd(Basal,   na.rm=T), 3),
            Final_Media = round(mean(Final, na.rm=T), 3),
            Final_DE    = round(sd(Final,   na.rm=T), 3)) |>
  knitr::kable(caption = "Estadística descriptiva de la covariable (Basal) y la variable de respuesta (Final) por grupo.")


## -----------------------------------------------------------------------------
#| label: c25-table1-resumen
#| message: false
library(table1)
table1(~ Basal + Final | Group, data = dat)


## -----------------------------------------------------------------------------
#| label: fig-supuestos-ancova
#| fig-cap: "Gráficos Q-Q para la covariable (Basal, izquierda) y la variable de respuesta (Final, derecha). Ambas variables muestran distribuciones aproximadamente normales en sus Q-Q plots, aunque Basal presenta desviaciones moderadas en los extremos."
#| fig-width: 9
#| fig-height: 4

par(mfrow = c(1, 2))
qqPlot(dat$Basal, main = "Q-Q: Basal (covariable)",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")
qqPlot(dat$Final, main = "Q-Q: Final (respuesta)",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")
par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| label: c25-shapiro-levene
sw_basal <- shapiro.test(dat$Basal); sw_basal
sw_final <- shapiro.test(dat$Final); sw_final
lev_basal <- leveneTest(Basal ~ Group, data = dat, center = median); lev_basal
leveneTest(Final ~ Group, data = dat, center = median)


## -----------------------------------------------------------------------------
#| label: independencia-cov
ANOVA_Basal <- aov(Basal ~ Group, data = dat)
Basal_sum <- summary(ANOVA_Basal)
Basal_sum


## -----------------------------------------------------------------------------
#| label: ancova-modelo
# Contrastes ortogonales
Control_vs_Exp <- c(-2,  1,  1)
G1_vs_G3       <- c( 0, -1,  1)
contrasts(dat$Group) <- cbind(Control_vs_Exp, G1_vs_G3)
str(dat)


## -----------------------------------------------------------------------------
#| label: c25-anova-tipo3

# Modelo ANCOVA
ANCOVAmodel <- aov(Final ~ Group + Basal, data = dat)

# Tabla ANOVA Tipo III
Anova(ANCOVAmodel, type = "III")
summary.lm(ANCOVAmodel)

## -----------------------------------------------------------------------------
#| label: fig-particion-ancova
#| fig-cap: "Partición de la varianza en el ANCOVA. La varianza total (SST = 794.20) se divide en la varianza explicada por el modelo (SSM = 167.21, efecto del grupo) y la varianza residual del ANOVA (SSR = 626.99). El SSM se descompone, a su vez, en los dos contrastes ortogonales planeados: Control vs. experimentales (G1+G3) y G1 vs. G3. La clave del ANCOVA es que la covariable (Basal) recupera parte de la varianza que el ANOVA dejaba sin explicar (276.50 de los 626.99), reduciendo el residual a 350.49 (una reducción ≈ 44 %) y aumentando el poder de la prueba."
#| fig-width: 9.5
#| fig-height: 5.5
#| echo: false

par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(0, 0, type = "n", xlim = c(0, 12), ylim = c(0, 10),
     xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

caja <- function(x1, y1, x2, y2, col, bord, txt, cex = 0.78, coltxt = "black") {
  rect(x1, y1, x2, y2, col = col, border = bord, lwd = 2)
  text((x1 + x2)/2, (y1 + y2)/2, txt, col = coltxt, font = 2, cex = cex)
}

# Nivel 1: SST
caja(4.3, 8.5, 7.7, 9.6, "#AED6F1", "#2471A3", "SST = 794.20\n(Varianza total)")

# Nivel 2: SSM (modelo) y SSR (residual de ANOVA)
caja(1.2, 5.9, 4.6, 7.0, "#A9DFBF", "#1E8449", "SSM = 167.21\n(explicada por el modelo)")
caja(7.4, 5.9, 10.8, 7.0, "#FAD7A0", "#CA6F1E", "SSR = 626.99\n(residual del ANOVA)")

# Nivel 3a: contrastes planeados (desglose del SSM)
caja(0.2, 3.0, 2.5, 4.4, "#D5F5E3", "#1E8449",
     "Contraste 1\nControl vs. G1+G3\nSS = 163.37", cex = 0.70)
caja(2.9, 3.0, 5.2, 4.4, "#D5F5E3", "#1E8449",
     "Contraste 2\nG1 vs. G3\nSS = 3.84", cex = 0.70)

# Nivel 3b: la covariable rescata parte del SSR
caja(6.5, 3.0, 8.9, 4.4, "#D2B4DE", "#6C3483",
     "Covariable (Basal)\nSS = 276.50", cex = 0.72)
caja(9.3, 3.0, 11.7, 4.4, "#FADBD8", "#C0392B",
     "Residual (ANCOVA)\nSS = 350.49", cex = 0.72)

# Flechas
arrows(5.4, 8.5, 2.9, 7.0, lwd = 1.5, length = 0.1, col = "gray35")
arrows(6.6, 8.5, 9.1, 7.0, lwd = 1.5, length = 0.1, col = "gray35")
arrows(2.9, 5.9, 1.35, 4.4, lwd = 1.5, length = 0.1, col = "gray35")
arrows(2.9, 5.9, 4.05, 4.4, lwd = 1.5, length = 0.1, col = "gray35")
arrows(9.1, 5.9, 7.7, 4.4, lwd = 1.5, length = 0.1, col = "gray35")
arrows(9.1, 5.9, 10.5, 4.4, lwd = 1.5, length = 0.1, col = "gray35")

# Identidades de la partición
text(2.7, 2.3, "167.21 = 163.37 + 3.84",     cex = 0.72, font = 3, col = "gray30")
text(9.1, 2.3, "626.99 = 276.50 + 350.49",   cex = 0.72, font = 3, col = "gray30")


## -----------------------------------------------------------------------------
#| label: comparacion-modelos
# ANOVA sin covariable
ANOVAmodel <- aov(Final ~ Group, data = dat)

cat("=== Tabla ANOVA (sin covariable) ===\n")
print(summary(ANOVAmodel))

cat("\n=== Tabla ANCOVA (con covariable Basal) ===\n")
print(summary(ANCOVAmodel))

# Reducción del SSR
ssr_anova  <- summary(ANOVAmodel)[[1]]["Residuals","Sum Sq"]
ssr_ancova <- summary(ANCOVAmodel)[[1]]["Residuals","Sum Sq"]
cat("\nSSR ANOVA:", round(ssr_anova, 2),
    "\nSSR ANCOVA:", round(ssr_ancova, 2),
    "\nReducción:", round((1 - ssr_ancova/ssr_anova)*100, 1), "%\n")


## -----------------------------------------------------------------------------
#| label: medias-ajustadas
#| message: false
if (!requireNamespace("effects", quietly=TRUE)) install.packages("effects")
library(effects)
ajustadas <- effect("Group", ANCOVAmodel, se = TRUE)
summary(ajustadas)


## -----------------------------------------------------------------------------
#| label: fig-medias-ajustadas
#| fig-cap: "Medias ajustadas por la covariable (Basal) con intervalos de confianza al 95% por grupo de tratamiento. Las medias ajustadas representan la fluorescencia final esperada para cada grupo, controlando el nivel basal de fluorescencia. Los grupos G1 y G3 presentan medias ajustadas significativamente menores que el control (G2)."
#| fig-width: 5
#| fig-height: 4

plot(ajustadas, main = "Medias ajustadas — ANCOVA",
     xlab = "Grupo de tratamiento",
     ylab = "Fluorescencia final ajustada (UF)",
     lines   = list(col = "#2E86C1"),
     symbols = list(col = "#2E86C1", pch = 16),
     confint = list(col = "#2E86C1"))


## -----------------------------------------------------------------------------
#| label: posthoc-ancova
#| message: false
library(multcomp)

# Tukey HSD sobre medias ajustadas
postHocs <- glht(ANCOVAmodel, linfct = mcp(Group = "Tukey"))
summary(postHocs)
confint(postHocs)


## -----------------------------------------------------------------------------
#| label: homogeneidad-pendientes
# Si la interacción Group:Basal es significativa, el supuesto se viola
RS <- aov(Final ~ Group + Basal + Group:Basal, data = dat)
rs3 <- Anova(RS, type = "III")
rs3


## -----------------------------------------------------------------------------
#| label: efecto-ancova
aov_tab  <- summary.lm(ANCOVAmodel)$coefficients
SS_tab   <- Anova(ANCOVAmodel, type="III")
SS_R     <- summary(ANCOVAmodel)[[1]]["Residuals","Sum Sq"]

# eta² parcial para cada efecto
eta2_Group <- SS_tab["Group","Sum Sq"] / (SS_tab["Group","Sum Sq"] + SS_R)
eta2_Basal <- SS_tab["Basal","Sum Sq"] / (SS_tab["Basal","Sum Sq"] + SS_R)

cat("η² parcial — Group:", round(eta2_Group, 4), "\n")
cat("η² parcial — Basal:", round(eta2_Basal, 4), "\n")

# r para cada contraste
rcontrast <- function(t, df) sqrt(t^2 / (t^2 + df))
df_r <- summary(ANCOVAmodel)[[1]]["Residuals","Df"]
t_c1 <- aov_tab["GroupControl_vs_Exp","t value"]
t_c2 <- aov_tab["GroupG1_vs_G3","t value"]

r_c1 <- rcontrast(t_c1, df_r)
r_c2 <- rcontrast(t_c2, df_r)
cat("r — Control vs. Experimentales:", round(r_c1, 3), "\n")
cat("r — G1 vs. G3:", round(r_c2, 3), "\n")

