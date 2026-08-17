## -----------------------------------------------------------------------------
#| label: prep-friedman33
#| message: false
library(dplyr); library(reshape2)

dat <- read.table("Examples/DXKTw.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat$Grupo <- as.factor(dat$Grupo)
G1 <- subset(dat, Grupo == "1") %>%
  select(Px, Grupo, X0, X1, X2)

str(G1)


## -----------------------------------------------------------------------------
#| label: c33-varianzas-esfericidad
G1$dif01 <- G1$X0 - G1$X1
G1$dif02 <- G1$X0 - G1$X2
G1$dif12 <- G1$X1 - G1$X2

cat("Varianza diferencias X0-X1:", round(var(G1$dif01, na.rm=T), 3), "\n")
cat("Varianza diferencias X0-X2:", round(var(G1$dif02, na.rm=T), 3), "\n")
cat("Varianza diferencias X1-X2:", round(var(G1$dif12, na.rm=T), 3), "\n")


## -----------------------------------------------------------------------------
#| label: mauchly-friedman
#| message: false
library(ez)

# Formato largo para ezANOVA
G1_long_ez <- melt(G1[, c("Px","X0","X1","X2")], id.vars = "Px",
                   variable.name = "Tiempo", value.name = "Dolor")
G1_long_ez$Px <- factor(G1_long_ez$Px)

ez_res <- ezANOVA(data = G1_long_ez, dv = .(Dolor), wid = .(Px),
                  within = .(Tiempo), detailed = TRUE, type = 3)
ez_res$`Mauchly's Test for Sphericity`


## -----------------------------------------------------------------------------
#| label: ranking-friedman
# Rankear dentro de cada sujeto (por fila)
G1_num  <- as.matrix(G1[, c("X0","X1","X2")])
G1_rank <- t(apply(G1_num, 1, rank))
colnames(G1_rank) <- c("Rango_X0","Rango_X1","Rango_X2")

# Suma de rangos por condición
R <- colSums(G1_rank, na.rm = TRUE)
cat("Suma de rangos por condición:\n")
print(R)

N <- nrow(G1)
k <- 3

# Estadístico de Friedman SIN corrección por empates
chi2_F <- (12 / (N * k * (k + 1))) * sum(R^2) - 3 * N * (k + 1)

# Estadístico CON corrección por empates (el que usa friedman.test)
CF      <- (1/4) * N * k * (k + 1)^2
Fr_corr <- (N * (k - 1) * (sum(R^2) / N - CF)) / (sum(G1_rank^2) - CF)
p_corr  <- pchisq(Fr_corr, df = k - 1, lower.tail = FALSE)

cat("\nχ²F sin corrección por empates =", round(chi2_F, 4), "\n")
cat("χ²F corregido por empates       =", round(Fr_corr, 4),
    "| p =", round(p_corr, 4), "\n")


## -----------------------------------------------------------------------------
#| label: fig-friedman-viz
#| fig-cap: "Evolución del dolor postoperatorio (NRS) en el grupo Dexketoprofeno a las horas 0, 1 y 2. Las líneas conectan las mediciones del mismo paciente. La mediana del dolor tiende a aumentar a lo largo del tiempo (0, 1 y 5, respectivamente), con una variabilidad considerable entre pacientes; no obstante, la prueba de Friedman no detecta una diferencia significativa (p = 0.07)."
#| fig-width: 6
#| fig-height: 5
#| message: false

library(ggplot2)
G1_long <- melt(G1[, c("Px","X0","X1","X2")], id.vars = "Px",
                variable.name = "Tiempo", value.name = "Dolor")
G1_long$Tiempo <- factor(G1_long$Tiempo,
                         labels = c("0 horas","1 hora","2 horas"))

ggplot(G1_long, aes(Tiempo, Dolor)) +
  geom_line(aes(group = Px), alpha = 0.25, colour = "gray50") +
  geom_boxplot(aes(fill = Tiempo), alpha = 0.6, outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("#AED6F1","#A9DFBF","#FADBD8")) +
  stat_summary(fun = median, geom = "point", size = 3.5, colour = "black") +
  stat_summary(fun = median, geom = "line", aes(group = 1),
               colour = "black", linewidth = 1.2) +
  labs(x = "Tiempo postoperatorio", y = "Dolor (NRS)") +
  theme_bw(base_size = 12) + theme(legend.position = "none")

