## -----------------------------------------------------------------------------
#| label: carga-vickers-12
dat <- read.table("sets de datos/VickersF.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat <- subset(dat, Cure.time == "Dual")
dat$Group <- factor(dat$Group)
head(dat)


## -----------------------------------------------------------------------------
#| label: fig-boxplot-ind
#| fig-cap: "Comparación de la dureza de Vickers (VH) entre los cementos de resina Multilink y Ultimate en el protocolo de polimerización Dual. Los puntos individuales superpuestos sobre las cajas permiten observar la distribución real de los datos y detectar posibles valores atípicos."
#| fig-width: 5
#| fig-height: 5
#| message: false

library(ggplot2)
ggplot(dat, aes(Group, VH, fill = Group)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.55, size = 2.5, colour = "gray20") +
  scale_fill_manual(values = c("#AED6F1","#FADBD8")) +
  labs(x = "Tipo de cemento",
       y = "Dureza de Vickers (VH)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: c12-descriptiva
#| message: false
library(dplyr)
dat %>%
  group_by(Group) %>%
  summarise(n = n(),
            Media   = round(mean(VH, na.rm=T), 3),
            DE      = round(sd(VH,   na.rm=T), 3),
            Mediana = round(median(VH, na.rm=T), 3),
            IQR     = round(IQR(VH, na.rm=T), 3)) |>
  knitr::kable(caption = "Estadística descriptiva de la dureza de Vickers por grupo (protocolo Dual).")


## -----------------------------------------------------------------------------
#| label: c12-tabla1
#| message: false
library(table1)
table1(~ VH | Group, data = dat)


## -----------------------------------------------------------------------------
#| label: fig-supuestos-ind
#| fig-cap: "Evaluación de supuestos de la prueba t independiente: Q-Q plot de la dureza de Vickers — los puntos dentro de la banda de confianza indican distribución normal. Los resultados de Shapiro-Wilk (p > 0.05) y Levene (p > 0.05) confirman el cumplimiento de los supuestos."
#| fig-width: 5
#| fig-height: 5

library(car)
qqPlot(dat$VH,
       main = "Q-Q: Dureza de Vickers",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")


## -----------------------------------------------------------------------------
#| label: c12-supuestos
sw <- shapiro.test(dat$VH); sw
lev <- leveneTest(VH ~ Group, data = dat, center = median); lev


## -----------------------------------------------------------------------------
#| label: ttest-ind
itt <- t.test(VH ~ Group, data = dat,
              alternative = "two.sided",
              var.equal   = FALSE)   # Welch corrige si varianzas son distintas
itt


## -----------------------------------------------------------------------------
#| label: efecto-ttest-ind
t_val <- itt$statistic
df_val <- itt$parameter
r_efecto <- sqrt(t_val^2 / (t_val^2 + df_val))

# Diferencia e IC en la misma dirección (Ultimate − Multilink)
delta_um <- diff(itt$estimate)                    # Ultimate − Multilink
ic_um    <- c(-itt$conf.int[2], -itt$conf.int[1]) # IC en esa misma dirección

cat("t =", round(t_val, 4), "\n")
cat("df =", round(df_val, 2), "\n")
cat("r (tamaño de efecto) =", round(r_efecto, 3), "\n")
cat("Diferencia de medias (Ultimate − Multilink) =", round(delta_um, 3), "\n")
cat("IC 95%: [", round(ic_um[1], 3), ",", round(ic_um[2], 3), "]\n")


## -----------------------------------------------------------------------------
#| label: fig-efecto-ind
#| fig-cap: "Visualización de la diferencia de medias entre los grupos Multilink y Ultimate con intervalo de confianza al 95%. La línea de referencia en 0 representa la hipótesis nula (sin diferencia entre grupos). El IC incluye el 0, confirmando que la diferencia no es estadísticamente significativa."
#| fig-width: 6
#| fig-height: 3.5

medias <- tapply(dat$VH, dat$Group, mean, na.rm = TRUE)
delta  <- diff(medias)                            # Ultimate − Multilink
ic     <- c(-itt$conf.int[2], -itt$conf.int[1])   # IC en la misma dirección

df_plot <- data.frame(
  diferencia = delta,
  li = ic[1],
  ls = ic[2]
)

ggplot(df_plot, aes(x = "Ultimate − Multilink", y = diferencia)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#E74C3C", linewidth = 0.8) +
  geom_pointrange(aes(ymin = li, ymax = ls),
                  colour = "#2E86C1", size = 1, linewidth = 1.2) +
  labs(x = "", y = "Diferencia de medias (VH) con IC 95%") +
  coord_flip() +
  theme_bw(base_size = 12)

