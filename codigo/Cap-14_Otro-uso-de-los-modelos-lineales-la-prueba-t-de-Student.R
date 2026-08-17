## -----------------------------------------------------------------------------
#| label: carga-vickers
dat <- read.table("Examples/VickersF.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

# Solo protocolo Dual
dat <- subset(dat, Cure.time == "Dual")
dat$Group <- factor(dat$Group)
str(dat)


## -----------------------------------------------------------------------------
#| label: c11-descriptiva-vickers
#| message: false
library(dplyr)
resumen_vickers <- dat %>%
  group_by(Group) %>%
  summarise(n = n(),
            Media = round(mean(VH, na.rm=T), 3),
            DE    = round(sd(VH,   na.rm=T), 3),
            EE    = round(sd(VH,   na.rm=T)/sqrt(n()), 3))
knitr::kable(resumen_vickers,
             caption = "Estadística descriptiva de la dureza de Vickers por tipo de cemento (protocolo Dual).")


## -----------------------------------------------------------------------------
#| label: c11-tabla1-vickers
#| message: false
library(table1)
table1(~ VH | Group, data = dat)


## -----------------------------------------------------------------------------
#| label: lm-vickers
LM <- lm(VH ~ Group, data = dat)
summary(LM)


## -----------------------------------------------------------------------------
#| label: c11-coeficientes-lm
#| echo: false
b0 <- round(coef(LM)[1], 4)
b1 <- round(coef(LM)[2], 4)
cat("b0 (media Multilink):", b0, "\n")
cat("b1=diferencia (Ultimate - Multilink):", b1, "\n")
cat("Media Ultimate:", round(b0 + b1, 4), "\n")


## -----------------------------------------------------------------------------
#| label: fig-boxplot-vickers
#| fig-cap: "Dureza de Vickers (VH) por tipo de cemento de resina odontológica en el protocolo de polimerización Dual. Las cajas muestran la distribución de los datos para cada grupo; los puntos representan observaciones individuales."
#| fig-width: 5
#| fig-height: 5
#| message: false

library(ggplot2)
ggplot(dat, aes(Group, VH, fill = Group)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 2, colour = "gray30") +
  scale_fill_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Tipo de Cemento",
       y = "Dureza de Vickers (VH)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: fig-qq-vickers
#| fig-cap: "Gráfico Q-Q para la dureza de Vickers (VH). Los puntos dentro de las bandas de confianza al 95% indican distribución normal, confirmando el cumplimiento del supuesto de normalidad."
#| fig-width: 5
#| fig-height: 5

library(car)
qqPlot(dat$VH,
       main = "Q-Q: Dureza de Vickers",
       xlab = "Cuantiles teóricos",
       ylab = "Cuantiles observados",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")


## -----------------------------------------------------------------------------
#| label: supuestos-vickers
shapiro.test(dat$VH)
leveneTest(VH ~ Group, data = dat, center = median)

