## -----------------------------------------------------------------------------
#| label: carga-carbuse
dat <- read.table("sets de datos/Carbuse.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat$Groups <- factor(dat$Groups, levels = c("0_uses","1_use"))
str(dat)


## -----------------------------------------------------------------------------
#| label: fig-boxplot-dep
#| fig-cap: "Rugosidad superficial promedio (Ra) de fresas quirúrgicas de carburo antes (0 usos) y después (1 uso) del corte de hueso cortical. Cada punto representa una fresa individual. La rugosidad disminuye ligeramente después del uso, aunque la variabilidad es considerable y la diferencia no es marcada."
#| fig-width: 5
#| fig-height: 5
#| message: false

library(ggplot2)
ggplot(dat, aes(Groups, Ra, fill = Groups)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6, size = 2.5, colour = "gray20") +
  scale_fill_manual(values = c("#AED6F1","#FADBD8")) +
  labs(x = "Condición",
       y = "Rugosidad promedio Ra (μm)") +
  scale_x_discrete(labels = c("0_uses" = "Antes (0 usos)",
                               "1_use"  = "Después (1 uso)")) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: c13-descriptiva
#| message: false
library(dplyr)
dat %>%
  group_by(Groups) %>%
  summarise(n       = n(),
            Media   = round(mean(Ra, na.rm=T), 3),
            DE      = round(sd(Ra,   na.rm=T), 3),
            Mediana = round(median(Ra, na.rm=T), 3),
            IQR     = round(IQR(Ra, na.rm=T), 3)) |>
  knitr::kable(caption = "Estadística descriptiva de la rugosidad superficial Ra por condición (antes/después).")


## -----------------------------------------------------------------------------
#| label: c13-tabla1
#| message: false
library(table1)
# Etiquetas descriptivas de los grupos para la tabla
dat_t1 <- dat
levels(dat_t1$Groups) <- c("Antes (0 usos)", "Después (1 uso)")
table1(~ Ra | Groups, data = dat_t1)


## -----------------------------------------------------------------------------
#| label: transformacion-wide
library(reshape2)
datW <- reshape(dat,
                direction = "wide",
                idvar     = "ID",
                timevar   = "Groups")
datW$dif <- datW$Ra.0_uses - datW$Ra.1_use

knitr::kable(head(datW),
             caption = "Set de datos en formato wide con la diferencia entre mediciones calculada.")


## -----------------------------------------------------------------------------
#| label: fig-qq-diferencias
#| fig-cap: "Gráfico Q-Q de las diferencias entre la rugosidad superficial antes y después del uso. Los puntos dentro de la banda de confianza al 95% y el resultado no significativo de Shapiro-Wilk indican que las diferencias siguen una distribución normal, cumpliendo el supuesto de la prueba t apareada."
#| fig-width: 5
#| fig-height: 5

library(car)
qqPlot(datW$dif,
       main = "Q-Q: Diferencias Ra (antes − después)",
       xlab = "Cuantiles teóricos",
       ylab = "Cuantiles observados",
       pch = 16, col = "#2E86C1", col.lines = "#E74C3C")


## -----------------------------------------------------------------------------
#| label: c13-shapiro-diferencias
sw <- shapiro.test(datW$dif)
sw


## -----------------------------------------------------------------------------
#| label: ttest-dep
dtt <- t.test(datW$Ra.0_uses, datW$Ra.1_use,
              paired      = TRUE,
              alternative = "two.sided")
dtt


## -----------------------------------------------------------------------------
#| label: fig-pares-ttest
#| fig-cap: "Cambio en la rugosidad superficial (Ra) de cada fresa quirúrgica antes y después del uso. Cada línea conecta las mediciones de la misma fresa. Líneas ascendentes indican aumento de Ra después del uso; líneas descendentes, disminución. La ausencia de un patrón claro y sistemático concuerda con la diferencia no significativa observada."
#| fig-width: 6
#| fig-height: 5

dat_paired <- dat
dat_paired$Groups <- factor(dat_paired$Groups,
                            levels = c("0_uses","1_use"),
                            labels = c("Antes", "Después"))

ggplot(dat_paired, aes(Groups, Ra, group = ID)) +
  geom_line(alpha = 0.4, colour = "#555555") +
  geom_point(aes(colour = Groups), size = 2.5, alpha = 0.7) +
  scale_colour_manual(values = c("#2E86C1","#E74C3C")) +
  stat_summary(fun = mean, geom = "point", size = 4, colour = "black") +
  stat_summary(fun = mean, geom = "line", aes(group = 1),
               colour = "black", linewidth = 1.2) +
  labs(x = "Condición",
       y = "Rugosidad promedio Ra (μm)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: efecto-ttest-dep
t_val  <- dtt$statistic
df_val <- dtt$parameter
r_efecto <- sqrt(t_val^2 / (t_val^2 + df_val))

cat("t =", round(t_val, 4), "\n")
cat("df =", round(df_val, 0), "\n")
cat("Delta (diferencia de medias) =", round(dtt$estimate, 4), "\n")
cat("IC 95%: [", round(dtt$conf.int[1],3), ",", round(dtt$conf.int[2],3), "]\n")
cat("r (tamaño de efecto) =", round(r_efecto, 3), "\n")

