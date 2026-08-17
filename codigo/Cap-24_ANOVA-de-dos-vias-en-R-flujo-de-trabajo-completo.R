## -----------------------------------------------------------------------------
#| label: prep-twoway22
library(car); library(ggplot2); library(dplyr)

dat <- read.table("sets de datos/VickersF.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

dat$Group     <- factor(dat$Group)
dat$Cure.time <- factor(dat$Cure.time, levels = c("Control","Dual","Light(20s)"))


## -----------------------------------------------------------------------------
#| label: fig-qq-twoway
#| fig-cap: "Q-Q plot de la dureza de Vickers (todos los grupos combinados). Los puntos dentro de la banda de confianza al 95% confirman el supuesto de normalidad para el ANOVA factorial."
#| fig-width: 5
#| fig-height: 5

qqPlot(dat$VH, main = "Q-Q: Dureza de Vickers",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")


## -----------------------------------------------------------------------------
#| label: c22-levene-interaccion
# Evaluación de homogeneidad de varianzas (considerando la interacción)
leveneTest(VH ~ interaction(Cure.time, Group), data = dat, center = median)


## -----------------------------------------------------------------------------
#| label: c22-descriptiva-tabla
dat %>%
  group_by(Group, Cure.time) %>%
  summarise(n = n(), Media = round(mean(VH,na.rm=T),3),
            DE = round(sd(VH,na.rm=T),3), .groups = "drop") |>
  knitr::kable(caption = "Estadística descriptiva de la dureza de Vickers por tipo de cemento y protocolo de polimerización.")


## -----------------------------------------------------------------------------
#| label: c22-tabla1-descriptiva
#| message: false
library(table1)
table1(~ VH | Group + Cure.time, data = dat)


## -----------------------------------------------------------------------------
#| label: twoway-anova22
# Definir contrastes
Control_vs_Exp <- c(-2,  1,  1)
Dual_vs_Light  <- c( 0, -1,  1)
Ulti_vs_Multi  <- c(-1,  1)

contrasts(dat$Cure.time) <- cbind(Control_vs_Exp, Dual_vs_Light)
contrasts(dat$Group)     <- cbind(Ulti_vs_Multi)

# Modelo ANOVA factorial con interacción
TW_ANOVA <- aov(VH ~ Group * Cure.time, data = dat)

# Tabla ANOVA tipo III (recomendada con contrastes)
aov3 <- Anova(TW_ANOVA, type = "III")
aov3


## -----------------------------------------------------------------------------
#| label: c22-resumen-lm
summary.lm(TW_ANOVA)


## -----------------------------------------------------------------------------
#| label: fig-contraste-interaccion
#| fig-cap: "Gráficas de medias para los dos contrastes de interacción. **Izquierda** (Ulti_vs_Multi × Control_vs_Exp): las pendientes son similares en ambos cementos —las líneas no se cruzan—, lo que indica ausencia de interacción (t = 0.363, p = 0.717). **Derecha** (Ulti_vs_Multi × Dual_vs_Light): las líneas se cruzan porque el cemento Ultimate experimenta una caída de dureza mucho mayor que el Multilink al pasar del protocolo Dual a Light(20s), confirmando la interacción significativa (t = −4.227, p < 0.001)."
#| fig-width: 9
#| fig-height: 4.5
#| message: false

# Medias de celda (no se ven afectadas por los atributos de contraste)
mc <- tapply(dat$VH, list(dat$Group, dat$Cure.time), mean)

ml_exp_avg <- (mc["Multilink","Dual"] + mc["Multilink","Light(20s)"]) / 2
ul_exp_avg <- (mc["Ultimate", "Dual"] + mc["Ultimate", "Light(20s)"]) / 2

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3.5, 1))

# ── Panel 1: Control vs. Experimentales (sin interacción) ──────────────
plot(1:2,
     c(mc["Multilink","Control"], ml_exp_avg),
     type = "b", lwd = 2.2, col = "#2E86C1", pch = 16, cex = 1.6,
     ylim = c(37, 46), xaxt = "n",
     xlab = "Protocolo de polimerización",
     ylab = "Media Dureza de Vickers (HV)",
     main = "Control vs. Experimentales")
lines(1:2,
      c(mc["Ultimate","Control"], ul_exp_avg),
      type = "b", lwd = 2.2, col = "#E74C3C", pch = 16, cex = 1.6)
axis(1, at = 1:2, labels = c("Control", "Exp. (Dual+Light)"))
legend("topright",
       legend = c("Multilink","Ultimate"),
       col = c("#2E86C1","#E74C3C"), lwd = 2, pch = 16,
       bty = "n", cex = 0.9)
text(1.5, 45.5, "t = 0.363,  p = 0.717", cex = 0.85, col = "gray35", font = 3)
text(1.5, 44.7, "Sin interacción", cex = 0.85, col = "gray35", font = 3)

# ── Panel 2: Dual vs. Light(20s) (interacción significativa) ───────────
plot(1:2,
     c(mc["Multilink","Dual"], mc["Multilink","Light(20s)"]),
     type = "b", lwd = 2.2, col = "#2E86C1", pch = 16, cex = 1.6,
     ylim = c(33, 46), xaxt = "n",
     xlab = "Protocolo de polimerización",
     ylab = "",
     main = "Dual vs. Light(20s)")
lines(1:2,
      c(mc["Ultimate","Dual"], mc["Ultimate","Light(20s)"]),
      type = "b", lwd = 2.2, col = "#E74C3C", pch = 16, cex = 1.6)
axis(1, at = 1:2, labels = c("Dual","Light(20s)"))
legend("bottomleft",
       legend = c("Multilink","Ultimate"),
       col = c("#2E86C1","#E74C3C"), lwd = 2, pch = 16,
       bty = "n", cex = 0.9)
text(1.5, 45.5, "t = −4.227,  p < 0.001", cex = 0.85, col = "gray35", font = 3)
text(1.5, 44.5, "Interacción significativa", cex = 0.85, col = "gray35", font = 3)

par(mfrow = c(1,1))


## -----------------------------------------------------------------------------
#| label: fig-interaccion22
#| fig-cap: "Interacción entre el tipo de cemento y el protocolo de polimerización para la dureza de Vickers. El cruce de las líneas en el protocolo Light(20s) confirma la interacción significativa: la reducción de dureza con Light(20s) es mucho mayor en el cemento Ultimate que en el Multilink."
#| fig-width: 7
#| fig-height: 4
#| message: false

dat$Group     <- factor(dat$Group)
dat$Cure.time <- factor(dat$Cure.time, levels = c("Control","Dual","Light(20s)"))

ggplot(dat, aes(Cure.time, VH, colour = Group, group = Group)) +
  stat_summary(fun = mean, geom = "point", size = 3.5) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.3) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.18) +
  scale_colour_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Protocolo de polimerización",
       y = "Media Dureza de Vickers (VH) ± IC 95%",
       colour = "Tipo de cemento") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: efectos-simples
# Crea variable combinada para efectos simples
dat$simple <- gl(6, 15)
dat$simple <- factor(dat$simple, levels = 1:6,
                     labels = c("Mult_L20", "Mult_Dual", "Mult_Con",
                                "Ulti_L20", "Ulti_Dual", "Ulti_Con"))
head(dat)


## -----------------------------------------------------------------------------
#| label: c22-contrastes-simples
Con_vs_Exp                <- c( 1,  1, -2,  1,  1, -2)
Dual_vs_Light             <- c(-1,  1,  0, -1,  1,  0)
Dual_Multi_vs_Dual_Ulti   <- c( 0, -1,  0,  0,  1,  0)
Con_Multi_vs_Con_Ulti     <- c( 0,  0, -1,  0,  0,  1)
Light_Multi_vs_Light_Ulti <- c(-1,  0,  0,  1,  0,  0)
simpleEff <- cbind(Con_vs_Exp, Dual_vs_Light, Dual_Multi_vs_Dual_Ulti,
                   Con_Multi_vs_Con_Ulti, Light_Multi_vs_Light_Ulti)
contrasts(dat$simple) <- simpleEff
dat$simple



## -----------------------------------------------------------------------------
#| label: fig-simple-effects-tree
#| fig-cap: "Árbol de contrastes para el análisis de efectos simples en el ANOVA factorial. A partir del factor *simple* (6 grupos resultantes de combinar Group × Cure.time), se definen cinco contrastes ortogonales: el **Contraste 1** compara el grupo Control contra los experimentales (Dual + Light) con ambos cementos combinados; el **Contraste 2** separa Dual de Light(20s) dentro de los grupos experimentales; el **Contraste 3** compara Multilink con Ultimate en el protocolo Control; el **Contraste 4** compara Multilink con Ultimate en el protocolo Dual; y el **Contraste 5** compara Multilink con Ultimate en el protocolo Light(20s). Los contrastes 3, 4 y 5 son los efectos simples del factor Group dentro de cada nivel de Cure.time."
#| fig-width: 10
#| fig-height: 6
#| echo: false

par(mar = c(0.5, 0.5, 2, 0.5))
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10),
     xaxt = "n", yaxt = "n", bty = "n",
     main = "Contrastes de efectos simples — factor simple (6 grupos)")

caja <- function(x1, y1, x2, y2, col, bord, txt, cex = 0.78, coltxt = "white") {
  rect(x1, y1, x2, y2, col = col, border = bord, lwd = 2)
  text((x1 + x2)/2, (y1 + y2)/2, txt, col = coltxt, font = 2, cex = cex)
}

# ── Fila 1 (y=8.5–9.3): Control vs. Experimental ──────── CONTRASTE 1 ──
caja(0.3, 8.5, 3.8, 9.3, "#E74C3C","#C0392B",
     "Control\n(Multilink + Ultimate)")
caja(4.6, 8.5, 9.7, 9.3, "#E74C3C","#C0392B",
     "Experimental (Dual+Light)\n(Multilink + Ultimate)")
text(4.2,  8.9,  "VS",          col = "#566573", font = 2, cex = 0.82)
text(0.0,  9.55, "CONTRASTE 1", col = "#C0392B", font = 2, cex = 0.78, adj = 0)

# ── Fila 2 (y=7.0–7.8): Dual vs. Light(20s) ─────────── CONTRASTE 2 ──
caja(4.6, 7.0, 6.4, 7.8, "#27AE60","#1E8449", "Dual\n(ML + UL)")
caja(6.8, 7.0, 9.7, 7.8, "#27AE60","#1E8449", "Light(20s)\n(ML + UL)")
text(6.6,  7.4,  "VS",          col = "#566573", font = 2, cex = 0.82)
text(4.5,  8.15, "CONTRASTE 2", col = "#27AE60", font = 2, cex = 0.78, adj = 0)

# ── Fila 3 (y=5.5–6.3): Control_ML vs. Control_UL ──────── CONTRASTE 3 ──
caja(0.2, 5.5, 1.9, 6.3, "#2E86C1","#1A5276", "Control\nMultilink")
caja(2.3, 5.5, 4.0, 6.3, "#2E86C1","#1A5276", "Control\nUltimate")
text(2.1,  5.9,  "VS",          col = "#566573", font = 2, cex = 0.82)
text(0.0,  6.55, "CONTRASTE 3", col = "#2E86C1", font = 2, cex = 0.78, adj = 0)

# ── Fila 4 (y=4.0–4.8): Dual_ML vs. Dual_UL ────────────── CONTRASTE 4 ──
caja(4.3, 4.0, 5.9, 4.8, "#8E44AD","#6C3483", "Dual\nMultilink")
caja(6.3, 4.0, 7.9, 4.8, "#8E44AD","#6C3483", "Dual\nUltimate")
text(6.1,  4.4,  "VS",          col = "#566573", font = 2, cex = 0.82)
text(4.2,  5.05, "CONTRASTE 4", col = "#8E44AD", font = 2, cex = 0.78, adj = 0)

# ── Fila 5 (y=2.5–3.3): Light_ML vs. Light_UL ───────────── CONTRASTE 5 ──
caja(7.1, 2.5, 8.4, 3.3, "#E67E22","#A04000", "Light\nMultilink")
caja(8.7, 2.5, 10.0, 3.3, "#E67E22","#A04000", "Light\nUltimate")
text(8.55, 2.9,  "VS",          col = "#566573", font = 2, cex = 0.82)
text(7.0,  3.55, "CONTRASTE 5", col = "#E67E22", font = 2, cex = 0.78, adj = 0)

# ── Conectores ───────────────────────────────────────────────────────────
# Fila1_Control  → Fila3 (Con_ML y Con_UL)
segments(2.05, 8.5, 1.05, 6.3, lwd = 1.5, col = "#566573")
segments(2.05, 8.5, 3.15, 6.3, lwd = 1.5, col = "#566573")
# Fila1_Experimental → Fila2 (Dual y Light)
segments(7.15, 8.5, 5.5,  7.8, lwd = 1.5, col = "#566573")
segments(7.15, 8.5, 8.25, 7.8, lwd = 1.5, col = "#566573")
# Fila2_Dual  → Fila4 (Dual_ML y Dual_UL)
segments(5.5,  7.0, 5.1,  4.8, lwd = 1.5, col = "#566573")
segments(5.5,  7.0, 7.1,  4.8, lwd = 1.5, col = "#566573")
# Fila2_Light → Fila5 (Light_ML y Light_UL)
segments(8.25, 7.0, 7.75, 3.3, lwd = 1.5, col = "#566573")
segments(8.25, 7.0, 9.35, 3.3, lwd = 1.5, col = "#566573")


## -----------------------------------------------------------------------------
#| label: Anova-contrastes-efectos-simples
# ANOVA de un factor (simple: 6 grupos) con contrastes planeados
SE_ANOVA <- aov(VH ~ simple, data = dat)
summary.lm(SE_ANOVA)


## -----------------------------------------------------------------------------
#| label: c22-tukey-simples
# Post-hoc Tukey sobre efectos simples (se reutiliza el modelo SE_ANOVA)
TukeyHSD(SE_ANOVA)


## -----------------------------------------------------------------------------
#| label: efecto-twoway
aov_sum  <- summary(TW_ANOVA)[[1]]
MS_R_val <- aov_sum["Residuals", "Mean Sq"]
SS_T_val <- sum(aov_sum[, "Sum Sq"])

efectos <- data.frame(
  Factor = rownames(aov_sum)[1:3],
  SS     = aov_sum[1:3, "Sum Sq"],
  df     = aov_sum[1:3, "Df"]
)
efectos$omega2_parcial <- with(efectos,
  (SS - df * MS_R_val) / (SS_T_val + MS_R_val))

knitr::kable(efectos, digits = 4,
             caption = "Tamaño de efecto ω² parcial para cada fuente de variación en el ANOVA factorial.")

