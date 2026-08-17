## -----------------------------------------------------------------------------
#| label: fig-rm-anova-schema
#| fig-cap: "Descomposición de la varianza en ANOVA de medidas repetidas (izquierda) comparada con ANOVA independiente (derecha). La diferencia clave es que en medidas repetidas la varianza entre sujetos (SSB) se extrae explícitamente, reduciendo la varianza del error (denominador del estadístico F) y aumentando el poder estadístico."
#| fig-width: 9
#| fig-height: 4
#| echo: false

par(mfrow = c(1, 2), mar = c(0.5, 0.5, 2, 0.5))

# ANOVA Medidas Repetidas
plot(0, 0, type="n", xlim=c(0,10), ylim=c(0,6),
     xaxt="n", yaxt="n", bty="n", main="ANOVA Medidas Repetidas")
rect(1,4,9,5.5, col="#AED6F1", border="#2E86C1", lwd=2)
text(5,4.75,"SST (varianza total)", font=2, cex=0.85)
rect(1,2,4.5,3.5, col="#D5F5E3", border="#27AE60", lwd=2)
text(2.75,2.75,"SSW\n(within)", font=2, cex=0.8, col="#27AE60")
rect(5,2,9,3.5, col="#FAD7A0", border="#E67E22", lwd=2)
text(7,2.75,"SSB\n(between)", font=2, cex=0.8, col="#E67E22")
rect(1,0.3,2.7,1.5, col="#A9DFBF", border="#1E8449", lwd=2)
text(1.85,0.9,"SSM", font=2, cex=0.8)
rect(3,0.3,4.5,1.5, col="#FADBD8", border="#E74C3C", lwd=2)
text(3.75,0.9,"SSR", font=2, cex=0.8)
arrows(2.75,2,1.85,1.6, lwd=1.5, length=0.1)
arrows(2.75,2,3.75,1.6, lwd=1.5, length=0.1)

# ANOVA Independiente
plot(0, 0, type="n", xlim=c(0,10), ylim=c(0,6),
     xaxt="n", yaxt="n", bty="n", main="ANOVA Independiente")
rect(1,4,9,5.5, col="#AED6F1", border="#2E86C1", lwd=2)
text(5,4.75,"SST (varianza total)", font=2, cex=0.85)
rect(1,2,4.5,3.5, col="#A9DFBF", border="#1E8449", lwd=2)
text(2.75,2.75,"SSM\n(modelo)", font=2, cex=0.8)
rect(5,2,9,3.5, col="#FADBD8", border="#E74C3C", lwd=2)
text(7,2.75,"SSR\n(error)", font=2, cex=0.8)
arrows(5,4,2.75,3.6, lwd=1.5, length=0.1)
arrows(5,4,7,3.6, lwd=1.5, length=0.1)

par(mfrow=c(1,1))


## -----------------------------------------------------------------------------
#| label: prep-rm19
#| message: false
library(dplyr)
library(reshape2)

dat <- read.table("Examples/DXKTw.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)

dat <- within(dat, {Grupo <- as.factor(Grupo)})
G1  <- subset(dat, Grupo == "1")

# Seleccionar solo variables necesarias
G1 <- G1 %>% select(Px, Grupo, X6, X7, X8)

# Transformar a formato long
G1_long <- melt(G1, id.vars = c("Px","Grupo"),
                measure.vars = c("X6","X7","X8"),
                variable.name = "Times",
                value.name = "Response")
G1_long$Times <- factor(G1_long$Times, levels = c("X6","X7","X8"))

head(G1_long, 9)


## -----------------------------------------------------------------------------
#| label: calculo-ss-rm
# SST
grand_var <- var(G1_long$Response, na.rm = TRUE)
N         <- nrow(G1_long)
SS_T      <- grand_var * (N - 1)
cat("SST:", round(SS_T, 2), "| df =", N - 1, "\n")

# SSW (within-participant)
var_px <- G1_long %>%
  group_by(Px) %>%
  summarise(var_i = var(Response, na.rm = TRUE))
n_cond <- 3
SS_W <- sum(var_px$var_i * (n_cond - 1), na.rm = TRUE)
df_W <- nrow(var_px) * (n_cond - 1)
cat("SSW:", round(SS_W, 2), "| df =", df_W, "\n")

# SSM
grand_mean <- mean(G1_long$Response, na.rm = TRUE)
medias_t   <- G1_long %>%
  group_by(Times) %>%
  summarise(n = n(), media = mean(Response, na.rm = TRUE))
n_px <- length(unique(G1_long$Px))
SS_M <- sum(medias_t$n * (medias_t$media - grand_mean)^2)
df_M <- nrow(medias_t) - 1
cat("SSM:", round(SS_M, 2), "| df =", df_M, "\n")

# SSR
SS_R <- SS_W - SS_M
df_R <- df_W - df_M
cat("SSR:", round(SS_R, 2), "| df =", df_R, "\n")

# F-ratio
MS_M   <- SS_M / df_M
MS_R   <- SS_R / df_R
F_stat <- MS_M / MS_R
p_val  <- pf(F_stat, df_M, df_R, lower.tail = FALSE)
cat("F =", round(F_stat, 4), "| p =", round(p_val, 4), "\n")


## -----------------------------------------------------------------------------
#| label: ssb-rm19
# Suma de cuadrados entre participantes (SS_B)
media_px <- tapply(G1_long$Response, G1_long$Px, mean, na.rm = TRUE)
SS_B     <- SS_T - SS_W
SS_B_alt <- n_cond * sum((media_px - grand_mean)^2)
df_B     <- length(unique(G1_long$Px)) - 1

cat("SS_B = SS_T - SS_W =", round(SS_T, 2), "-", round(SS_W, 2),
    "=", round(SS_B, 2), "\n")
cat("SS_B (fórmula directa):", round(SS_B_alt, 2), "\n")
cat("df_B =", df_B, "\n")
cat("Verificación: SS_W + SS_B =", round(SS_W + SS_B, 2),
    "(debe ser igual a SS_T =", round(SS_T, 2), ")\n")

