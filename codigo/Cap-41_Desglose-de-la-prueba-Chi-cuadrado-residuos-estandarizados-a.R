## -----------------------------------------------------------------------------
#| label: dat-perio
library(gmodels)

dat <- read.table("Examples/datPerio.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

dat$Perio_Dx <- factor(dat$Perio_Dx,
                       levels = c("Ging","Low_P","Mod_P","Adv_P"))
str(dat[, c("Sex","Perio_Dx")])


## -----------------------------------------------------------------------------
#| label: tabla-perio
TableSP <- xtabs(~ Sex + Perio_Dx, data = dat)
knitr::kable(addmargins(TableSP),
             caption = "Tabla de contingencia: Sexo vs. Diagnóstico periodontal.")


## -----------------------------------------------------------------------------
#| label: c42-tabla1-perio
#| message: false
library(table1)
# Etiquetas descriptivas de los niveles para la tabla
dat_t1 <- dat
levels(dat_t1$Perio_Dx) <- c("Gingivitis", "Periodontitis leve",
                             "Periodontitis moderada", "Periodontitis avanzada")
table1(~ Perio_Dx | Sex, data = dat_t1)


## -----------------------------------------------------------------------------
#| label: crossTable-perio
CrossTable(TableSP,
           fisher    = TRUE,
           chisq     = TRUE,
           expected  = TRUE,
           asresid    = TRUE,
           format    = "SPSS")


## -----------------------------------------------------------------------------
#| label: cramer-v
#| warning: false
res_v <- chisq.test(TableSP)
N <- sum(TableSP)
k <- min(dim(TableSP))
V <- sqrt(as.numeric(res_v$statistic) / (N * (k - 1)))
cat("V de Cramér =", round(V, 3), "\n")


## -----------------------------------------------------------------------------
#| label: residuos-std
# Residuos estandarizados ajustados (ASR), vía chisq.test()
res <- chisq.test(TableSP)
ASR  <- res$stdres

cat("=== Residuos estandarizados ajustados ===\n")
print(round(ASR, 3))

cat("\n=== Celdas significativas (|ASR| > 1.96) ===\n")
sig <- which(abs(ASR) > 1.96, arr.ind = TRUE)
if (nrow(sig) == 0) {
  cat("Ninguna celda presenta ASR significativo.\n")
} else {
  print(sig)
}


## -----------------------------------------------------------------------------
#| label: fig-residuos-heatmap
#| fig-cap: "Mapa de calor de los residuos estandarizados ajustados por sexo y diagnóstico periodontal. Cada celda muestra la frecuencia observada (n), su porcentaje sobre el total y el residuo estandarizado ajustado (ASR). Los colores azules indican celdas con más observaciones de lo esperado; los rojos, menos. Celdas con |ASR| > 1.96 (equivalente a p < 0.05) indicarían asociación significativa en esa celda específica."
#| fig-width: 7
#| fig-height: 4
#| message: false

library(ggplot2)
ASR_df <- as.data.frame(as.table(ASR))
names(ASR_df) <- c("Sexo","Perio_Dx","ASR")

# Frecuencias observadas reales y su porcentaje
obs_ct <- xtabs(~ Sex + Perio_Dx, data = dat)
obs_df <- as.data.frame(as.table(obs_ct))
names(obs_df) <- c("Sexo","Perio_Dx","n")
ASR_df <- merge(ASR_df, obs_df, by = c("Sexo","Perio_Dx"))
ASR_df$pct <- round(ASR_df$n / sum(ASR_df$n) * 100, 1)

ggplot(ASR_df, aes(Perio_Dx, Sexo, fill = ASR)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0("n=", n, " (", pct, "%)\nASR=", round(ASR, 2))),
            size = 3.3, lineheight = 0.95) +
  scale_fill_gradient2(low = "#E74C3C", mid = "white", high = "#2E86C1",
                       midpoint = 0, name = "ASR") +
  geom_hline(yintercept = c(0.5, 1.5, 2.5), colour = "white") +
  labs(x = "Diagnóstico periodontal",
       y = "Sexo",
       title = "Residuos estandarizados — Sexo vs. Diagnóstico periodontal") +
  theme_bw(base_size = 12)

