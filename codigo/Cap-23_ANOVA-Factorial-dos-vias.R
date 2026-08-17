## -----------------------------------------------------------------------------
#| label: fig-particion-factorial
#| fig-cap: "Descomposición de la varianza en el ANOVA factorial de dos vías. La varianza total (SST) se divide en el error residual (SSR) y la varianza explicada por el modelo (SSM). Esta última se subdivide en tres fuentes independientes: el efecto principal del Factor A (Group), el efecto principal del Factor B (Cure.time) y la interacción A×B, que captura si el efecto de un factor depende del nivel del otro."
#| fig-width: 9
#| fig-height: 4.5
#| echo: false

par(mar = c(0.5, 0.5, 2, 0.5))
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 6),
     xaxt = "n", yaxt = "n", bty = "n",
     main = "ANOVA Factorial (dos vías)")

# ── Nivel 1: SST ────────────────────────────────────────────────
rect(1, 4.2, 9, 5.5, col = "#AED6F1", border = "#2E86C1", lwd = 2)
text(5, 4.85, "SST (varianza total)", font = 2, cex = 0.85)

# ── Nivel 2: SSR (izquierda) y SSM (derecha) ────────────────────
rect(0.5, 2.4, 3.8, 3.8, col = "#FADBD8", border = "#E74C3C", lwd = 2)
text(2.15, 3.10, "SSR\n(error)", font = 2, cex = 0.80, col = "#E74C3C")

rect(4.8, 2.4, 9.5, 3.8, col = "#A9DFBF", border = "#1E8449", lwd = 2)
text(7.15, 3.10, "SSM\n(modelo)", font = 2, cex = 0.80, col = "#1E8449")

# ── Nivel 3: SSA, SSB, SSAxB ────────────────────────────────────
rect(0.3, 0.4, 3.1, 1.9, col = "#FAD7A0", border = "#E67E22", lwd = 2)
text(1.70, 1.32, "SSA",               font = 2, cex = 0.80)
text(1.70, 0.85, "Factor A (Group)",           cex = 0.74)

rect(3.5, 0.4, 6.5, 1.9, col = "#FAD7A0", border = "#E67E22", lwd = 2)
text(5.00, 1.32, "SSB",               font = 2, cex = 0.80)
text(5.00, 0.85, "Factor B (Cure.time)",       cex = 0.74)

rect(6.9, 0.4, 9.7, 1.9, col = "#FAD7A0", border = "#E67E22", lwd = 2)
text(8.30, 1.32, "SSA×B",        font = 2, cex = 0.80)
text(8.30, 0.85, "interacción A×B",  cex = 0.74)

# ── Flechas ──────────────────────────────────────────────────────
arrows(3.8, 4.2, 2.15, 3.8, lwd = 1.5, length = 0.1)   # SST → SSR
arrows(6.2, 4.2, 7.15, 3.8, lwd = 1.5, length = 0.1)   # SST → SSM
arrows(7.15, 2.4, 1.70, 1.9, lwd = 1.5, length = 0.1)  # SSM → SSA
arrows(7.15, 2.4, 5.00, 1.9, lwd = 1.5, length = 0.1)  # SSM → SSB
arrows(7.15, 2.4, 8.30, 1.9, lwd = 1.5, length = 0.1)  # SSM → SSAxB


## -----------------------------------------------------------------------------
#| label: carga-twoway
dat <- read.table("Examples/VickersF.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

dat$Group     <- factor(dat$Group)
dat$Cure.time <- factor(dat$Cure.time, levels = c("Control","Dual","Light(20s)"))
str(dat)


## -----------------------------------------------------------------------------
#| label: fig-interaccion
#| fig-cap: "Gráfico de interacción para la dureza de Vickers según el protocolo de polimerización y el tipo de cemento. Las líneas que se cruzan indican una interacción significativa: la reducción de dureza con el protocolo Light(20s) es mucho mayor en el cemento Ultimate que en el Multilink. Las barras de error representan los intervalos de confianza al 95% bootstrap."
#| fig-width: 7
#| fig-height: 4
#| message: false

library(ggplot2)
ggplot(dat, aes(Cure.time, VH, colour = Group, group = Group)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.15) +
  scale_colour_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Protocolo de polimerización",
       y = "Media Dureza de Vickers (VH)",
       colour = "Tipo de cemento") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: fig-boxplot-twoway
#| fig-cap: "Distribución de la dureza de Vickers por protocolo de polimerización, separada por tipo de cemento (facetas). El protocolo Light(20s) muestra una reducción marcada en el cemento Ultimate pero no en el Multilink, evidencia visual de la interacción."
#| fig-width: 8
#| fig-height: 4

ggplot(dat, aes(Cure.time, VH, fill = Cure.time)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  facet_wrap(~ Group) +
  scale_fill_manual(values = c("#AED6F1","#A9DFBF","#FADBD8")) +
  labs(x = "Protocolo de polimerización", y = "Dureza de Vickers (VH)") +
  theme_bw(base_size = 11) + theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: calculo-twoway
library(dplyr)

grand_mean <- mean(dat$VH)
grand_var  <- var(dat$VH)
N          <- nrow(dat)

# SST
SS_T <- grand_var * (N - 1)

# Medias por combinación de grupos (SSM)
medias_celda <- dat %>%
  group_by(Cure.time, Group) %>%
  summarise(n = n(), media = mean(VH), .groups = "drop")

SS_M <- sum(medias_celda$n * (medias_celda$media - grand_mean)^2)

# SSA (efecto principal Group)
medias_A <- dat %>% group_by(Group) %>%
  summarise(n = n(), media = mean(VH))
SS_A <- sum(medias_A$n * (medias_A$media - grand_mean)^2)

# SSB (efecto principal Cure.time)
medias_B <- dat %>% group_by(Cure.time) %>%
  summarise(n = n(), media = mean(VH))
SS_B <- sum(medias_B$n * (medias_B$media - grand_mean)^2)

# SS interacción
SS_AB <- SS_M - SS_A - SS_B

# SSR
SS_R <- SS_T - SS_M

tabla_ss <- data.frame(
  Fuente = c("Group (A)", "Cure.time (B)", "Group × Cure.time", "Residuos", "Total"),
  SS     = round(c(SS_A, SS_B, SS_AB, SS_R, SS_T), 3),
  df     = c(1, 2, 2, 84, 89)
)
MS_R_val    <- SS_R / 84
tabla_ss$MS <- round(c(SS_A/1, SS_B/2, SS_AB/2, MS_R_val, NA), 3)
tabla_ss$F  <- round(c(SS_A/MS_R_val, (SS_B/2)/MS_R_val, (SS_AB/2)/MS_R_val, NA, NA), 3)

knitr::kable(tabla_ss, caption = "Tabla ANOVA de dos vías calculada manualmente.")

