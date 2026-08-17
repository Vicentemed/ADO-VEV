## -----------------------------------------------------------------------------
#| label: carga-anova17
dat <- read.table("Examples/VickersF.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

ML <- subset(dat, Group == "Multilink")
ML$Cure.time <- factor(ML$Cure.time,
                       levels = c("Control","Dual","Light(20s)"))
str(ML)


## -----------------------------------------------------------------------------
#| label: fig-boxplot-anova
#| fig-cap: "Dureza de Vickers (VH) por protocolo de polimerización del cemento Multilink. El protocolo Control muestra la mayor dureza media, mientras que Light(20s) presenta la menor. Los puntos individuales superpuestos permiten evaluar la distribución real de los datos dentro de cada grupo."
#| fig-width: 6
#| fig-height: 5
#| message: false

library(ggplot2)
ggplot(ML, aes(Cure.time, VH, fill = Cure.time)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.5, size = 2.5, colour = "gray20") +
  scale_fill_manual(values = c("#AED6F1","#A9DFBF","#FADBD8")) +
  labs(x = "Protocolo de polimerización",
       y = "Dureza de Vickers (VH)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: c17-descriptiva-dplyr
#| message: false
library(dplyr)
ML %>%
  group_by(Cure.time) %>%
  summarise(n = n(),
            Media = round(mean(VH, na.rm=T), 3),
            DE    = round(sd(VH,   na.rm=T), 3),
            EE    = round(sd(VH,   na.rm=T)/sqrt(n()), 3)) |>
  knitr::kable(caption = "Estadística descriptiva de la dureza de Vickers por protocolo de polimerización.")


## -----------------------------------------------------------------------------
#| label: c17-tabla1-descriptiva
#| message: false
library(table1)
table1(~ VH | Cure.time, data = ML)


## -----------------------------------------------------------------------------
#| label: fig-supuestos-anova
#| fig-cap: "Evaluación de supuestos del ANOVA: Q-Q plot de la dureza de Vickers (todos los grupos combinados). Los puntos dentro de la banda de confianza al 95% y el resultado no significativo de Shapiro-Wilk (W = 0.975, p = 0.44) confirman normalidad. La prueba de Levene (p = 0.21) confirma homocedasticidad."
#| fig-width: 5
#| fig-height: 5

library(car)
qqPlot(ML$VH,
       main = "Q-Q: Dureza de Vickers (todos los grupos)",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")


## -----------------------------------------------------------------------------
#| label: c17-supuestos-tests
sh <- shapiro.test(ML$VH)
sh
lev <- leveneTest(VH ~ Cure.time, data = ML, center = median)
lev


## -----------------------------------------------------------------------------
#| label: anova-contrastes17
# Definir contrastes ortogonales
Control_vs_Exp <- c(-2,  1,  1)
Dual_vs_Light  <- c( 0, -1,  1)
contrasts(ML$Cure.time) <- cbind(Control_vs_Exp, Dual_vs_Light)

# Ajustar modelo
ANOVA_PC <- aov(VH ~ Cure.time, data = ML)
summary.lm(ANOVA_PC)


## -----------------------------------------------------------------------------
#| label: anova-trend
contrasts(ML$Cure.time) <- contr.poly(3)
Trend <- aov(VH ~ Cure.time, data = ML)
summary.lm(Trend)


## -----------------------------------------------------------------------------
#| label: posthoc-bonferroni
# Restablecer contraste estándar para post-hoc
ML$Cure.time <- factor(ML$Cure.time,
                       levels = c("Control","Dual","Light(20s)"))
ANOVA_std <- aov(VH ~ Cure.time, data = ML)

# Bonferroni
pairwise.t.test(ML$VH, ML$Cure.time, p.adjust.method = "bonferroni")


## -----------------------------------------------------------------------------
#| label: posthoc-bh
# Benjamini-Hochberg (FDR)
pairwise.t.test(ML$VH, ML$Cure.time, p.adjust.method = "BH")


## -----------------------------------------------------------------------------
#| label: posthoc-tukey17
#| message: false
library(multcomp)

# Tukey HSD
postHocs <- glht(ANOVA_std, linfct = mcp(Cure.time = "Tukey"))
sum_ph <- summary(postHocs)
sum_ph
confint(postHocs)


## -----------------------------------------------------------------------------
#| label: fig-tukey17
#| fig-cap: "Comparaciones post-hoc de Tukey HSD para la dureza de Vickers entre los protocolos de polimerización del cemento Multilink. Los intervalos de confianza al 95% que no incluyen el 0 indican diferencias estadísticamente significativas. Solo la comparación Control vs. Light(20s) resultó significativa (p = 0.002)."
#| fig-width: 7
#| fig-height: 4

par(mar = c(4, 14, 3, 2))
plot(confint(postHocs), col = "#2E86C1",
     main = "Tukey HSD — IC 95%")


## -----------------------------------------------------------------------------
#| label: posthoc-dunnett
# Dunnett (comparación de cada grupo vs. control)
postHocs_D <- glht(ANOVA_std, linfct = mcp(Cure.time = "Dunnett"))
summary(postHocs_D)


## -----------------------------------------------------------------------------
#| label: efecto-anova
# Extracción de componentes
aov_sum  <- summary(ANOVA_std)[[1]]
SS_M_val <- aov_sum[1, 2]
SS_R_val <- aov_sum[2, 2]
SS_T_val <- SS_M_val + SS_R_val
df_M_val <- aov_sum[1, 1]
MS_R_val <- aov_sum[2, 3]

eta2  <- SS_M_val / SS_T_val
omega2 <- (SS_M_val - df_M_val * MS_R_val) / (SS_T_val + MS_R_val)

cat("η² (eta cuadrado):", round(eta2,  4), "\n")
cat("ω² (omega cuadrado):", round(omega2, 4), "\n")
cat("r (√η²):", round(sqrt(eta2), 4), "\n")


## -----------------------------------------------------------------------------
#| label: c17-efecto-contrastes
# Tamaño de efecto r para cada contraste planeado
rcontrast <- function(t, df) sqrt(t^2 / (t^2 + df))

contrasts(ML$Cure.time) <- cbind(c(-2,1,1), c(0,-1,1))
mod_c <- aov(VH ~ Cure.time, data = ML)
coef_tab <- summary.lm(mod_c)$coefficients

t_c1 <- coef_tab[2, 3]  # Contraste 1
t_c2 <- coef_tab[3, 3]  # Contraste 2
df_r <- summary(mod_c)[[1]]["Residuals","Df"]

cat("Contraste 1 (Control vs. Experimentales): r =", round(rcontrast(t_c1, df_r), 3), "\n")
cat("Contraste 2 (Dual vs. Light):             r =", round(rcontrast(t_c2, df_r), 3), "\n")

