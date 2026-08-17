## -----------------------------------------------------------------------------
#| label: prep-signed29
#| message: false
library(dplyr); library(reshape2)

dat <- read.table("sets de datos/DXKTw.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat$Grupo <- as.factor(dat$Grupo)
G2 <- subset(dat, Grupo == "2") %>%
  select(Px, Grupo, X0, X2)

# Calcular diferencia
G2$diferencia <- G2$X0 - G2$X2
head(G2)


## -----------------------------------------------------------------------------
#| label: c29-shapiro-diferencias
shapiro.test(G2$diferencia)


## -----------------------------------------------------------------------------
#| label: ranking-signo
# Paso previo: excluir las diferencias = 0 (no aportan información de signo)
G2_nozero <- G2[G2$diferencia != 0, ]

# Paso 1: ordenar por el VALOR ABSOLUTO de la diferencia, de menor a mayor
orden    <- order(abs(G2_nozero$diferencia))
sr       <- G2_nozero[orden, c("Px", "diferencia")]
sr$abs_d <- abs(sr$diferencia)

# Paso 2: rango potencial = posición en ese orden
sr$rango_potencial <- seq_len(nrow(sr))

# Paso 3: rango final = promedio de los rangos potenciales en caso de empate
sr$rango_final <- rank(sr$abs_d)

# Paso 4: devolver el signo original de la diferencia a cada rango
sr$rango_con_signo <- sign(sr$diferencia) * sr$rango_final

head(sr, 15)


## -----------------------------------------------------------------------------
#| label: suma-rangos-signo
W_pos  <- sum(sr$rango_con_signo[sr$rango_con_signo > 0])
W_neg  <- abs(sum(sr$rango_con_signo[sr$rango_con_signo < 0]))
T_stat <- min(W_pos, W_neg)

cat("W+ (suma de rangos positivos):", W_pos, "\n")
cat("W- (suma de rangos negativos):", W_neg, "\n")
cat("T (estadístico de prueba, el menor):", T_stat, "\n")


## -----------------------------------------------------------------------------
#| label: fig-signed-rank
#| fig-cap: "Distribución del dolor (NRS) inmediatamente después de la cirugía (X0) y a las 2 horas (X2) en el grupo control. Las líneas conectan las mediciones del mismo paciente. Las diferencias entre tiempos serán evaluadas con la prueba de Wilcoxon de rangos con signo."
#| fig-width: 6
#| fig-height: 5
#| message: false

library(ggplot2)
G2_long <- melt(G2[, c("Px","X0","X2")], id.vars = "Px",
                variable.name = "Tiempo", value.name = "Dolor")
G2_long$Tiempo <- factor(G2_long$Tiempo,
                         labels = c("Inmediato (X0)","2 horas (X2)"))

ggplot(G2_long, aes(Tiempo, Dolor)) +
  geom_line(aes(group = Px), alpha = 0.3, colour = "gray50") +
  geom_boxplot(aes(fill = Tiempo), alpha = 0.6, outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("#AED6F1","#FADBD8")) +
  stat_summary(fun = median, geom = "point", size = 3.5, colour = "black") +
  labs(x = "Tiempo de evaluación",
       y = "Dolor (NRS)") +
  theme_bw(base_size = 12) + theme(legend.position = "none")

