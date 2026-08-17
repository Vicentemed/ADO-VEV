## -----------------------------------------------------------------------------
#| label: carga-asthma
library(ggplot2)

dat <- read.table("Examples/datAsthma.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
str(dat)


## -----------------------------------------------------------------------------
#| label: tabla-asthma
# Orden de factores para orientación correcta de la tabla
dat$Asthma  <- factor(dat$Asthma,  levels = c("Yes","No"))
dat$OrParaF <- factor(dat$OrParaF, levels = c("Yes","No"))

# Crear tabla de contingencia
TablaAsOr <- xtabs(~ Asthma + OrParaF, data = dat)
knitr::kable(addmargins(TablaAsOr),
             caption = "Tabla de contingencia: Asma vs. Parafunción oral.")


## -----------------------------------------------------------------------------
#| label: c41-tabla1-asthma
#| message: false
library(table1)
table1(~ Age + Sex + Asthma  + OrParaF | Asthma, data = dat)


## -----------------------------------------------------------------------------
#| label: crosstable-asthma
#| message: false
library(gmodels)

CrossTable(dat$Asthma, dat$OrParaF,
           fisher        = TRUE,
           chisq         = TRUE,
           expected      = TRUE,
           prop.c        = FALSE,
           prop.t        = FALSE,
           prop.chisq    = FALSE,
           sresid        = TRUE,
           format        = "SPSS")


## -----------------------------------------------------------------------------
#| label: fig-mosaic-asthma
#| fig-cap: "Gráfico de mosaico para la relación entre asma y parafunción oral. El ancho de cada columna es proporcional a la frecuencia del diagnóstico de asma, y la altura de cada rectángulo, a la frecuencia condicional de la parafunción oral. Cada combinación de categorías se distingue con un color propio y se etiqueta con su porcentaje sobre el total y su frecuencia absoluta (n)."
#| fig-width: 6
#| fig-height: 4.6
#| message: false

# Coordenadas del mosaico calculadas en base R; el gráfico se dibuja con ggplot2
tab  <- TablaAsOr
N    <- sum(tab)
rsum <- rowSums(tab)               # totales por diagnóstico de asma
xr   <- c(0, cumsum(rsum / N))     # bordes de columna (anchos proporcionales)

# Etiquetas en español para los niveles
asL <- c(Yes = "Sí", No = "No")[rownames(tab)]
pfL <- c(Yes = "Sí", No = "No")[colnames(tab)]

# data.frame de rectángulos: una fila por combinación
recs <- data.frame()
for (i in 1:2) {                              # columnas = niveles de Asma
  yr <- c(0, cumsum(tab[i, ] / rsum[i]))      # bordes condicionales de Parafunción
  for (j in 1:2) {
    recs <- rbind(recs, data.frame(
      xmin = xr[i], xmax = xr[i + 1],
      ymin = yr[j], ymax = yr[j + 1],
      combo = paste0("Asma ", asL[i], " · Paraf ", pfL[j]),
      pct   = round(tab[i, j] / N * 100, 1),
      n     = tab[i, j],
      stringsAsFactors = FALSE))
  }
}
niveles <- c(paste0("Asma ", asL[1], " · Paraf ", pfL[1]),
             paste0("Asma ", asL[1], " · Paraf ", pfL[2]),
             paste0("Asma ", asL[2], " · Paraf ", pfL[1]),
             paste0("Asma ", asL[2], " · Paraf ", pfL[2]))
recs$combo <- factor(recs$combo, levels = niveles)

# Un color por combinación: azules para Asma Sí, rojos para Asma No
paleta <- c("#1F77B4", "#AEC7E8", "#D62728", "#FF9896")
names(paleta) <- niveles

centros <- (xr[-length(xr)] + xr[-1]) / 2
etiq_x  <- paste0("Asma: ", asL, "\n(n=", rsum, ")")

ggplot(recs) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = combo),
            colour = "white", linewidth = 1.2) +
  geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2,
                label = paste0(pct, "%\n(n=", n, ")")),
            size = 3.3, fontface = "bold") +
  scale_fill_manual(values = paleta, name = NULL,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(breaks = centros, labels = etiq_x, expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(title = "Asma vs. Parafunción oral",
       x = NULL, y = "Proporción de parafunción (dentro de cada grupo)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid       = element_blank(),
        axis.text.x      = element_text(face = "bold"))


## -----------------------------------------------------------------------------
#| label: c41-odds-ratio-fisher
# OR calculado
OR_res <- fisher.test(TablaAsOr)
cat("Odds Ratio:", round(OR_res$estimate, 4), "\n")
cat("IC 95%: [", round(OR_res$conf.int[1],3), ",", round(OR_res$conf.int[2],3), "]\n")
cat("p-value (Fisher exacta):", round(OR_res$p.value, 4), "\n")


## -----------------------------------------------------------------------------
#| label: phi-asthma
ch_phi <- chisq.test(TablaAsOr, correct = FALSE)
N_total <- sum(TablaAsOr)
phi <- sqrt(as.numeric(ch_phi$statistic) / N_total)
cat("Phi (φ) =", round(phi, 3), "\n")


## -----------------------------------------------------------------------------
#| label: poder-chi2
library(pwr)
pot <- pwr.chisq.test(w = phi, N = N_total, df = 1, sig.level = 0.05)
pot


## -----------------------------------------------------------------------------
#| label: poder-n-requerida
# Tamaño de muestra necesario para 80% de poder con el efecto observado
pwr.chisq.test(w = phi, power = 0.80, df = 1, sig.level = 0.05)

