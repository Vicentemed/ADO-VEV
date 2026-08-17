## -----------------------------------------------------------------------------
#| label: prep-rm20
#| message: false
library(dplyr); library(reshape2); library(ggplot2)

dat <- read.table("Examples/DXKTw.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

dat$Grupo <- as.factor(dat$Grupo)
G1 <- subset(dat, Grupo == "1") %>%
  select(Px, Grupo, X6, X7, X8)

G1_long <- melt(G1, id.vars = c("Px","Grupo"),
                measure.vars = c("X6","X7","X8"),
                variable.name = "Times", value.name = "Response")
G1_long$Times <- factor(G1_long$Times, levels = c("X6","X7","X8"))
G1_long$Px    <- factor(G1_long$Px)
str(G1_long)


## -----------------------------------------------------------------------------
#| label: fig-rm-anova-plot
#| fig-cap: "Distribución del dolor (NRS) en pacientes del grupo Dexketoprofeno a las 6, 7 y 8 horas postoperatorias. Las líneas grises conectan las mediciones del mismo paciente a lo largo del tiempo y permiten apreciar la trayectoria individual. En promedio, el dolor tiende a aumentar a las 7 horas y disminuye levemente a las 8 horas."
#| fig-width: 6
#| fig-height: 5

ggplot(G1_long, aes(Times, Response)) +
  geom_line(aes(group = Px), alpha = 0.3, colour = "gray50") +
  geom_boxplot(aes(fill = Times), alpha = 0.6, outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("#AED6F1","#A9DFBF","#FADBD8")) +
  stat_summary(fun = mean, geom = "point", size = 3.5, colour = "black") +
  stat_summary(fun = mean, geom = "line", aes(group = 1),
               colour = "black", linewidth = 1.2) +
  labs(x = "Tiempo postoperatorio", y = "Dolor (NRS)") +
  scale_x_discrete(labels = c("X6" = "6 horas", "X7" = "7 horas", "X8" = "8 horas")) +
  theme_bw(base_size = 12) + theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: c20-descriptiva-tiempos
#| message: false
G1_long %>%
  group_by(Times) %>%
  summarise(n = n(), Media = round(mean(Response,na.rm=T),2),
            DE = round(sd(Response,na.rm=T),2),
            Mediana = median(Response,na.rm=T)) |>
  knitr::kable(caption = "Estadística descriptiva del dolor postoperatorio por tiempo de evaluación (grupo Dexketoprofeno).")


## -----------------------------------------------------------------------------
#| label: c20-tabla1-descriptiva
#| message: false
library(table1)
table1(~ Response | Times, data = G1_long)


## -----------------------------------------------------------------------------
#| label: contrastes-rm
X6_vs_Others <- c(-2,  1,  1)
X7_vs_X8     <- c( 0, -1,  1)
contrasts(G1_long$Times) <- cbind(X6_vs_Others, X7_vs_X8)


## -----------------------------------------------------------------------------
#| label: rm-anova-ez
#| message: false
library(ez)

# Restablecer factor sin contrastes para ezANOVA
G1_long$Times <- factor(G1_long$Times, levels = c("X6","X7","X8"))

ezrmANOVA <- ezANOVA(data    = G1_long,
                     dv      = .(Response),
                     wid     = .(Px),
                     within  = .(Times),
                     detailed = TRUE,
                     type    = 3)
ezrmANOVA


## -----------------------------------------------------------------------------
#| label: rm-posthoc
# Bonferroni (recomendado cuando la esfericidad se viola)
pairwise.t.test(G1_long$Response, G1_long$Times,
                p.adjust.method = "bonferroni",
                paired = TRUE)


## -----------------------------------------------------------------------------
#| label: c20-posthoc-bh
# Benjamini-Hochberg (FDR)
pairwise.t.test(G1_long$Response, G1_long$Times,
                p.adjust.method = "BH",
                paired = TRUE)


## -----------------------------------------------------------------------------
#| label: rm-lme
#| message: false
library(nlme)

# Modelo con efecto del tiempo
G1_long$Times <- factor(G1_long$Times, levels = c("X6","X7","X8"))

rmANOVA <- lme(Response ~ Times,
               random  = ~1 | Px/Times,
               data    = G1_long,
               method  = "ML")

# Modelo nulo (sin efecto del tiempo)
rmANOVA_null <- lme(Response ~ 1,
                    random = ~1 | Px/Times,
                    data   = G1_long,
                    method = "ML")

# Comparación de modelos
anova(rmANOVA_null, rmANOVA)


## -----------------------------------------------------------------------------
#| label: c20-resumen-lme
summary(rmANOVA)


## -----------------------------------------------------------------------------
#| label: fig-lme-comparacion
#| fig-cap: "Comparación visual del modelo nulo y el modelo del experimento en el enfoque multinivel. Los diagramas de caja muestran la distribución del dolor (NRS) a las 6, 7 y 8 horas postoperatorias. La línea roja conecta las medias observadas por tiempo (modelo del experimento) y la línea azul punteada representa la media general constante a lo largo del tiempo (modelo nulo). La diferencia entre ambas líneas refleja la variabilidad adicional que el efecto del tiempo aporta; cuando esa diferencia es suficientemente grande, el modelo del experimento supera al modelo nulo en la prueba de razón de verosimilitud."
#| fig-width: 6
#| fig-height: 5

grand_mean_ch20 <- mean(G1_long$Response, na.rm = TRUE)
medias_times    <- tapply(G1_long$Response, G1_long$Times, mean, na.rm = TRUE)
df_medias <- data.frame(
  Times = factor(names(medias_times), levels = c("X6","X7","X8")),
  media = as.numeric(medias_times)
)

ggplot(G1_long, aes(x = Times, y = Response)) +
  geom_boxplot(aes(fill = Times), alpha = 0.55, outlier.shape = NA, width = 0.45) +
  geom_jitter(width = 0.1, alpha = 0.25, size = 1.5, colour = "gray40") +
  geom_line(data = df_medias,
            aes(x = Times, y = media, group = 1,
                colour = "Modelo del experimento"),
            linewidth = 1.6) +
  geom_point(data = df_medias,
             aes(x = Times, y = media,
                 colour = "Modelo del experimento"),
             size = 4, shape = 18) +
  geom_hline(aes(yintercept = grand_mean_ch20,
                 colour = "Modelo nulo"),
             linewidth = 1.4, linetype = "dashed") +
  scale_fill_manual(values = c("#AED6F1","#A9DFBF","#FADBD8"),
                    guide  = "none") +
  scale_colour_manual(
    name   = "",
    values = c("Modelo del experimento" = "#E74C3C",
               "Modelo nulo"            = "#2E86C1")
  ) +
  scale_x_discrete(labels = c("X6" = "6 horas",
                               "X7" = "7 horas",
                               "X8" = "8 horas")) +
  labs(x = "Tiempo postoperatorio", y = "Dolor (NRS)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
#| label: prep-drc
#| message: false
library(drc)

viab <- read.table("Examples/viabSBF.txt", header = TRUE, sep = "\t",
                   na.strings = "NA", dec = ".", strip.white = TRUE)
viab$Group <- as.factor(viab$Group)
str(viab)


## -----------------------------------------------------------------------------
#| label: drc-modelo-completo
#| warning: false
#| message: false
modPre <- drm(Absorbance ~ Timeb + Time, fct = LL.2(),
              data = viab, type = "event", curveid = Group)
summary(modPre)


## -----------------------------------------------------------------------------
#| label: fig-drc-crecimiento
#| fig-cap: "Curvas de crecimiento log-logísticas ajustadas a la viabilidad celular a lo largo de 21 días para los tres tipos de suero: C-FBS comercial (azul), E-FBS experimental (rojo) y E-Plasma experimental (verde). Los símbolos representan las mediciones observadas y las líneas el modelo ajustado por grupo. Las tres curvas son prácticamente indistinguibles, lo que anticipa la ausencia de diferencias significativas entre grupos."
#| warning: false
#| fig-width: 7
#| fig-height: 5.5
colores_drc <- c("#2E86C1","#E74C3C","#27AE60")   # C-FBS, E-FBS, E-Plasma

plot(modPre, log = "", xlab = "Tiempo (días)",
     ylab = "Viabilidad celular (absorbancia)",
     xlim = c(1, 25), ylim = c(0, 1.5),
     col = colores_drc, legendPos = c(7, 1.5))


## -----------------------------------------------------------------------------
#| label: drc-lr-test
#| warning: false
#| message: false
modNull <- drm(Absorbance ~ Timeb + Time, fct = LL.2(),
               data = viab, type = "event")

lr_test <- anova(modPre, modNull, test = "Chisq")
lr_test

