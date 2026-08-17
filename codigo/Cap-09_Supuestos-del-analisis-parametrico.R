## -----------------------------------------------------------------------------
#| label: carga-dxkt
dat <- read.table("Examples/DXKTw.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
head(dat[, 1:10])
str(dat[, 1:10])


## -----------------------------------------------------------------------------
#| label: c07-convertir-factor
# Convertir la variable de grupo a factor
dat <- within(dat, {Grupo <- as.factor(Grupo)})
levels(dat$Grupo) <- c("Dexketoprofeno", "Control")


## -----------------------------------------------------------------------------
#| label: c07-resumen-descriptivo
#| message: false
library(dplyr)

resumen <- dat %>%
  group_by(Grupo) %>%
  summarise(
    n       = sum(!is.na(X8)),
    Media   = round(mean(X8, na.rm = TRUE), 2),
    DE      = round(sd(X8,   na.rm = TRUE), 2),
    EE      = round(sd(X8, na.rm = TRUE) / sqrt(sum(!is.na(X8))), 2),
    Mediana = median(X8, na.rm = TRUE),
    IQR     = IQR(X8, na.rm = TRUE)
  )
knitr::kable(resumen,
             caption = "Estadística descriptiva del dolor postoperatorio a las 8 horas (X8) por grupo experimental.")


## -----------------------------------------------------------------------------
#| label: fig-boxplot-grupos
#| fig-cap: "Distribución del dolor postoperatorio a las 8 horas (X8) por grupo. La línea central de cada caja representa la mediana; los bordes representan Q1 y Q3; los bigotes se extienden hasta 1.5 × IQR."
#| fig-width: 5
#| fig-height: 5
#| message: false

library(ggplot2)
ggplot(dat, aes(Grupo, X8, fill = Grupo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  scale_fill_manual(values = c("#2E86C1", "#E74C3C")) +
  labs(x = "Grupo experimental",
       y = "Dolor a las 8 horas (NRS)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: fig-medias-grupos
#| fig-cap: "Medias del dolor postoperatorio a las 8 horas por grupo, con intervalos de confianza al 95% (barras de error). El paquete ggplot2 calcula los IC mediante bootstrap (mean_cl_boot)."
#| fig-width: 5
#| fig-height: 4

ggplot(dat, aes(Grupo, X8, colour = Grupo)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, linewidth = 0.8) +
  scale_colour_manual(values = c("#2E86C1", "#E74C3C")) +
  labs(x = "Grupo experimental",
       y = "Media del dolor a 8h (NRS) ± IC 95%") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: fig-histograma-x8
#| fig-cap: "Distribución del dolor postoperatorio a las 8 horas (X8) con la curva de densidad normal superpuesta. La discrepancia entre el histograma y la curva teórica sugiere que la distribución se aparta de la normalidad."
#| fig-width: 6
#| fig-height: 4

ggplot(dat, aes(X8)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins   = 10,
                 colour = "black",
                 fill   = "white",
                 alpha  = 0.8) +
  stat_function(fun  = dnorm,
                args = list(mean = mean(dat$X8, na.rm = TRUE),
                            sd   = sd(dat$X8,   na.rm = TRUE)),
                colour = "#E74C3C",
                linewidth = 1) +
  labs(x = "Dolor a las 8 horas (NRS)", y = "Densidad") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: c07-sesgo-curtosis
#| message: false
library(pastecs)
est_norm <- stat.desc(dat$X8, basic = FALSE, norm = TRUE)
round(est_norm, 4)


## -----------------------------------------------------------------------------
#| label: fig-qqplot-x8
#| fig-cap: "Gráfico Q-Q de la variable X8 (dolor a las 8 horas). La banda roja representa el intervalo de confianza al 95%. Los puntos fuera de la banda indican desviaciones significativas de la normalidad. La función qqPlot() del paquete car incluye automáticamente las bandas de confianza."
#| fig-width: 5
#| fig-height: 5
#| message: false

library(car)
qqPlot(dat$X8,
       main  = "Gráfico Q-Q — Dolor a las 8 horas",
       xlab  = "Cuantiles teóricos (Normal)",
       ylab  = "Cuantiles observados",
       pch   = 16,
       col   = "#2E86C1",
       col.lines = "#E74C3C")


## -----------------------------------------------------------------------------
#| label: c07-shapiro-wilk
shapiro.test(dat$X8)


## -----------------------------------------------------------------------------
#| label: c07-prueba-levene
#| message: false
library(car)
leveneTest(X8 ~ Grupo, data = dat, center = median)


## -----------------------------------------------------------------------------
#| label: fig-transformaciones
#| fig-cap: "Efecto de diferentes transformaciones matemáticas sobre una distribución con sesgo positivo (log-normal simulada). La transformación logarítmica es la más efectiva para aproximar la distribución a la normalidad en este caso. La transformación de raíz cuadrada logra una normalización parcial."
#| fig-width: 10
#| fig-height: 3
#| echo: false

set.seed(42)
x_orig <- exp(rnorm(200, mean = 2, sd = 0.9))

par(mfrow = c(1, 4))

hist(x_orig,
     main = "Original\n(sesgada)", xlab = "X",
     col = "#FADBD8", border = "white", breaks = 20)

hist(log(x_orig),
     main = "Log(X)", xlab = "log(X)",
     col = "#AED6F1", border = "white", breaks = 20)

hist(sqrt(x_orig),
     main = "√X", xlab = "√X",
     col = "#A9DFBF", border = "white", breaks = 20)

hist(1/x_orig,
     main = "1/X\n(recíproco)", xlab = "1/X",
     col = "#FAD7A0", border = "white", breaks = 20)

par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| label: c07-shapiro-log
# Aplicar y evaluar transformación logarítmica
x_log <- log(dat$X8[dat$X8 > 0])  # excluir ceros para log
shapiro.test(x_log)


## -----------------------------------------------------------------------------
#| label: demo-transformaciones
a <- c(2.5, 2.6, 5.8, 6.4, 8.3)

# Transformación logarítmica (log natural)
log_a <- log(a)

# Raíz cuadrada
sqrt_a <- sqrt(a)

# Recíproco
rec_a <- 1/a

data.frame(Original = a,
           Log      = round(log_a,  3),
           Sqrt     = round(sqrt_a, 3),
           Reci     = round(rec_a,  3))

