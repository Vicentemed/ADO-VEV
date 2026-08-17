## -----------------------------------------------------------------------------
#| label: fig-scatter-edad
#| fig-cap: "Relación entre la edad cronológica y la edad dental en pacientes de 5 a 15 años (datos simulados basados en Reverte-Salazar et al., 2019). La línea de regresión con banda de confianza al 95% muestra una fuerte asociación lineal positiva entre ambas variables."
#| fig-width: 6
#| fig-height: 5
#| message: false

# Datos simulados con las características del estudio original
set.seed(42)
n_pac <- 150
Cron_Age <- runif(n_pac, 5, 15)
Dental_Age <- Cron_Age + rnorm(n_pac, 0, 0.8)
SexL <- sample(c("Masculino","Femenino"), n_pac, replace = TRUE)
dat_cron <- data.frame(Cron_Age, Dental_Age, SexL)

library(ggplot2)
ggplot(dat_cron, aes(Cron_Age, Dental_Age)) +
  geom_point(aes(colour = SexL), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, colour = "gray30") +
  labs(x = "Edad Cronológica (años)",
       y = "Edad Dental (años)",
       colour = "Sexo") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: c06c-guardar-grafico
#| eval: false
# ggsave("scatter_edad.jpg", width = 6, height = 5, dpi = 300)


## -----------------------------------------------------------------------------
#| label: fig-histograma-edad
#| fig-cap: "Distribución de la edad cronológica en la muestra simulada. El histograma muestra la frecuencia de pacientes por intervalo de edad."
#| fig-width: 6
#| fig-height: 4

ggplot(dat_cron, aes(Cron_Age)) +
  geom_histogram(binwidth = 1, fill = "#2E86C1", colour = "white", alpha = 0.8) +
  labs(x = "Edad Cronológica (años)", y = "Frecuencia") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: fig-boxplot-sexo
#| fig-cap: "Comparación de la edad dental entre sexos. Los bordes de la caja representan Q1 y Q3; la línea central es la mediana; los bigotes se extienden hasta 1.5 × IQR; los puntos fuera de los bigotes son valores atípicos."
#| fig-width: 5
#| fig-height: 5

ggplot(dat_cron, aes(SexL, Dental_Age, fill = SexL)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  scale_fill_manual(values = c("#AED6F1","#FADBD8")) +
  labs(x = "Sexo", y = "Edad Dental (años)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: fig-densidad-edad
#| fig-cap: "Comparación de la distribución de la edad dental entre sexos mediante gráfico de densidad. Las curvas permiten identificar diferencias en la forma de la distribución."
#| fig-width: 6
#| fig-height: 4

ggplot(dat_cron, aes(Dental_Age, fill = SexL, colour = SexL)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values  = c("#2E86C1","#E74C3C")) +
  scale_colour_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Edad Dental (años)", y = "Densidad", fill = "Sexo", colour = "Sexo") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: fig-barras-edad
#| fig-cap: "Media de la edad dental por sexo con intervalos de confianza al 95%. Nota: las gráficas de barras pueden ser engañosas cuando la distribución no es simétrica; se recomienda complementarlas con boxplots o gráficas de violín."
#| fig-width: 5
#| fig-height: 4

ggplot(dat_cron, aes(SexL, Dental_Age, fill = SexL)) +
  stat_summary(fun = mean, geom = "bar", colour = "white", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.25, lwd = 0.8) +
  scale_fill_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Sexo", y = "Edad Dental (años) — Media ± IC 95%") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: fig-barras-dispersion
#| fig-cap: "La misma media de la edad dental por sexo representada con tres medidas de dispersión, sobre un eje Y común. Solo cambia el argumento `fun.data` de `stat_summary()`: error estándar (EE, `mean_se`), intervalo de confianza al 95 % (IC, `mean_cl_normal`) y desviación estándar (DE, `mean_sdl`). Se observa que EE < IC < DE."
#| fig-width: 9
#| fig-height: 3.6
#| message: false

library(patchwork)

# Las tres gráficas son idénticas salvo el argumento fun.data de stat_summary():
p_ee <- ggplot(dat_cron, aes(SexL, Dental_Age, fill = SexL)) +
  stat_summary(fun = mean, geom = "bar", colour = "white", alpha = 0.8) +
  stat_summary(fun.data = mean_se,                              # <- EE
               geom = "errorbar", width = 0.25, lwd = 0.8) +
  scale_fill_manual(values = c("#2E86C1","#E74C3C")) +
  coord_cartesian(ylim = c(0, 14)) +
  labs(title = "EE — mean_se", x = NULL, y = "Edad Dental (años)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))

p_ic <- ggplot(dat_cron, aes(SexL, Dental_Age, fill = SexL)) +
  stat_summary(fun = mean, geom = "bar", colour = "white", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_normal,                       # <- IC 95%
               geom = "errorbar", width = 0.25, lwd = 0.8) +
  scale_fill_manual(values = c("#2E86C1","#E74C3C")) +
  coord_cartesian(ylim = c(0, 14)) +
  labs(title = "IC 95% — mean_cl_normal", x = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))

p_de <- ggplot(dat_cron, aes(SexL, Dental_Age, fill = SexL)) +
  stat_summary(fun = mean, geom = "bar", colour = "white", alpha = 0.8) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),  # <- DE (±1, no ±2)
               geom = "errorbar", width = 0.25, lwd = 0.8) +
  scale_fill_manual(values = c("#2E86C1","#E74C3C")) +
  coord_cartesian(ylim = c(0, 14)) +
  labs(title = "DE — mean_sdl (mult = 1)", x = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))

p_ee | p_ic | p_de


## -----------------------------------------------------------------------------
#| label: fig-violin-edad
#| fig-cap: "Distribución de la edad dental por sexo mediante gráfico de violín, como complemento de la @fig-barras-edad. El ancho del violín representa la densidad de los datos; el diagrama de caja interior muestra la mediana y los cuartiles. A diferencia del gráfico de barras, revela la forma completa de la distribución en cada grupo."
#| fig-width: 5
#| fig-height: 4

ggplot(dat_cron, aes(SexL, Dental_Age, fill = SexL)) +
  geom_violin(alpha = 0.5, trim = FALSE, colour = NA) +
  geom_boxplot(width = 0.15, alpha = 0.85, outlier.shape = NA) +
  scale_fill_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Sexo", y = "Edad Dental (años)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: c06c-formato-long
# Formato long: cada observación en un renglón (repaso del capítulo anterior)
Long <- data.frame(
  SubjectID = rep(1:5, times = 3),
  Sex       = rep(c("M","F","F","M","M"), times = 3),
  Measures  = rep(c("T1","T2","T3"), each = 5),
  Response  = c(40.1,39.5,38.2,41.0,42.1,
                70.3,65.2,65.9,71.3,72.5,
                50.3,51.2,48.6,51.3,47.2)
)


## -----------------------------------------------------------------------------
#| label: fig-lineal-tiempo
#| fig-cap: "Evolución de la variable de respuesta en tres tiempos de medición, comparada entre sexos. Los puntos representan las medias; las barras de error representan el error estándar; las líneas conectan los tiempos de evaluación para cada grupo."
#| fig-width: 6
#| fig-height: 4

ggplot(Long, aes(Measures, Response, colour = Sex, group = Sex)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  scale_colour_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Tiempo de evaluación", y = "Respuesta (Media ± EE)", colour = "Sexo") +
  theme_bw(base_size = 12)


## -----------------------------------------------------------------------------
#| label: carga-formalin
#| eval: false
# dat <- read.table("Formalin.txt", header = TRUE, sep = "\t",
#                   na.strings = "NA", dec = ".", strip.white = TRUE)


## -----------------------------------------------------------------------------
#| label: simula-formalin
#| echo: false
# Datos simulados con la estructura del estudio original
set.seed(42)
grupos <- rep(c("G1-Control","G2-Dex","G3-CHX","G4-Dex+CHX","G5-Dex+CHX-0.3","G6-Dex+CHX-0.5"),
              each = 6)
dat_f <- data.frame(
  Rat    = factor(rep(1:6, times = 6)),
  Groups = factor(grupos),
  T1  = round(c(rnorm(6,10,4), rnorm(6,8.5,3), rnorm(6,9,4), rnorm(6,5.5,2), rnorm(6,5,2), rnorm(6,5,2))),
  T6  = round(c(rnorm(6,2.5,1.4), rnorm(6,0.7,1.2), rnorm(6,1.3,1.5), rnorm(6,2.0,2.2), rnorm(6,0.7,1.2), rnorm(6,1.5,1.2))),
  T11 = round(c(rnorm(6,20,6), rnorm(6,12,5), rnorm(6,14,5), rnorm(6,10,4), rnorm(6,11,4), rnorm(6,9,4)))
)


## -----------------------------------------------------------------------------
#| label: c06b-explorar-datos
head(dat_f)
str(dat_f)


## -----------------------------------------------------------------------------
#| label: c06b-resumen-dplyr
#| message: false
library(dplyr)

resumen <- dat_f %>%
  group_by(Groups) %>%
  summarise(
    n      = n(),
    Media_T1  = round(mean(T1, na.rm = TRUE), 2),
    DE_T1     = round(sd(T1,   na.rm = TRUE), 2),
    Mediana_T1 = median(T1, na.rm = TRUE)
  )

knitr::kable(resumen,
             caption = "Resumen descriptivo del número de patadas en T1 por grupo experimental.")


## -----------------------------------------------------------------------------
#| message: false
#| label: tbl-table1
#| tbl-cap: "Estadística descriptiva de los tiempos de evaluación por grupo experimental (N, media (DE) y mediana [mín, máx])."

library(table1)
table1(~ T1 + T6 + T11 | Groups, data = dat_f)


## -----------------------------------------------------------------------------
#| label: c06b-guardar-resumen
#| eval: false
# resumen_completo <- dat_f %>%
#   group_by(Groups) %>%
#   summarise(across(starts_with("T"),
#                    list(media = ~mean(., na.rm=TRUE),
#                         DE    = ~sd(.,   na.rm=TRUE),
#                         n     = ~sum(!is.na(.))),
#                    .names = "{.col}_{.fn}"))
# 
# write.csv(resumen_completo, "SummaryDescriptivo.csv", row.names = FALSE)

