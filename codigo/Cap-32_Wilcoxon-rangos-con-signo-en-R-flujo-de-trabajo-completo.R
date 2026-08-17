## -----------------------------------------------------------------------------
#| label: prep-signed30
#| message: false
library(dplyr); library(reshape2); library(ggplot2); library(car)

dat <- read.table("Examples/DXKTw.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat$Grupo <- as.factor(dat$Grupo)
G2 <- subset(dat, Grupo == "2") %>%
  select(Px, Grupo, X0, X2)

# Transformar a formato long para visualización
G2_long <- melt(G2[, c("Px","X0","X2")], id.vars = "Px",
                variable.name = "Tiempo", value.name = "Dolor")
G2_long$Tiempo <- factor(G2_long$Tiempo,
                         labels = c("Inmediato (X0)","2 horas (X2)"))
str(G2_long)


## -----------------------------------------------------------------------------
#| label: fig-boxplot-signed30
#| fig-cap: "Dolor (NRS) en pacientes del grupo control inmediatamente después de la cirugía (X0) y a las 2 horas (X2). Cada línea conecta las mediciones del mismo paciente. En este grupo control, el dolor tiende a aumentar a las 2 horas (la mediana pasa de 5 a 10), aunque con una variabilidad considerable entre pacientes."
#| fig-width: 6
#| fig-height: 5

ggplot(G2_long, aes(Tiempo, Dolor)) +
  geom_line(aes(group = Px), alpha = 0.3, colour = "gray50") +
  geom_boxplot(aes(fill = Tiempo), alpha = 0.6, outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("#AED6F1","#FADBD8")) +
  stat_summary(fun = median, geom = "point", size = 4, colour = "black") +
  stat_summary(fun = median, geom = "line", aes(group = 1),
               colour = "black", linewidth = 1.2) +
  labs(x = "Tiempo de evaluación", y = "Dolor (NRS)") +
  theme_bw(base_size = 12) + theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: c30-descriptiva-mediana
G2_long %>%
  group_by(Tiempo) %>%
  summarise(n       = n(),
            Mediana = median(Dolor, na.rm = TRUE),
            IQR     = IQR(Dolor, na.rm = TRUE)) |>
  knitr::kable(caption = "Estadística descriptiva del dolor por tiempo de evaluación.")


## -----------------------------------------------------------------------------
#| label: c30-table1-resumen
#| message: false
library(table1)
table1(~ Dolor | Tiempo, data = G2_long)


## -----------------------------------------------------------------------------
#| label: c30-shapiro-diferencias
G2$diferencia <- G2$X0 - G2$X2
sw <- shapiro.test(G2$diferencia)
sw


## -----------------------------------------------------------------------------
#| label: wilcoxon-signed30
WSModel <- wilcox.test(G2$X0, G2$X2,
                       paired      = TRUE,
                       alternative = "two.sided",
                       exact       = FALSE,
                       correct     = TRUE)
WSModel


## -----------------------------------------------------------------------------
#| label: c30-efecto-wilcoxon
rbyW_signed <- function(mod, N) {
  z <- qnorm(mod$p.value / 2)
  r <- abs(z) / sqrt(N)   # se reporta la magnitud del efecto
  cat("r (tamaño de efecto) =", round(r, 3), "\n")
  invisible(r)
}
r_efecto <- rbyW_signed(WSModel, nrow(G2))

# Mediana, media y DE por tiempo (conviene reportarlas; ver @sec-pruebas-no-parametricas)
resumen <- rbind(
  Mediana = tapply(G2_long$Dolor, G2_long$Tiempo, median),
  Media   = tapply(G2_long$Dolor, G2_long$Tiempo, mean),
  DE      = tapply(G2_long$Dolor, G2_long$Tiempo, sd))
print(round(resumen, 2))

cat("\nDiferencia de medianas =",
    abs(median(G2$X0) - median(G2$X2)), "\n")
cat("Diferencia de medias   =",
    round(abs(mean(G2$X0) - mean(G2$X2)), 2), "\n")

