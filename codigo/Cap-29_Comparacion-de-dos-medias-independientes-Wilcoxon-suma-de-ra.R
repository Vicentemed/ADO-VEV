## -----------------------------------------------------------------------------
#| label: carga-aucfil
dat_auc <- read.table("sets de datos/AUCFil.txt", header = TRUE, sep = "\t",
                      na.strings = "NA", dec = ".", strip.white = TRUE)
dat_auc$Group <- factor(dat_auc$Group)
str(dat_auc)


## -----------------------------------------------------------------------------
#| label: supuestos-wilcoxon27
library(car)
shapiro.test(dat_auc$AUC)
leveneTest(AUC ~ Group, data = dat_auc, center = median)


## -----------------------------------------------------------------------------
#| label: calculo-ranking
# Paso 1: ordenar TODAS las observaciones (de ambos grupos) de menor a mayor
orden   <- order(dat_auc$AUC)
auc_ord <- dat_auc[orden, c("Group", "AUC")]

# Paso 2: rango potencial = posición en el orden combinado
auc_ord$Rango_potencial <- seq_len(nrow(auc_ord))

# Paso 3: rango final = promedio de los rangos potenciales en caso de empate
auc_ord$Rango_final <- rank(auc_ord$AUC)

# Primeras observaciones del ordenamiento conjunto
head(auc_ord, 12)


## -----------------------------------------------------------------------------
#| label: suma-rangos-w
# Rango final de cada observación y suma de rangos por grupo
dat_auc$Rango <- rank(dat_auc$AUC)
sum_rangos <- tapply(dat_auc$Rango, dat_auc$Group, sum)
n_grupos   <- tapply(dat_auc$AUC,   dat_auc$Group, length)

# Estadístico W para cada grupo
W_por_grupo <- sum_rangos - n_grupos * (n_grupos + 1) / 2

cat("Suma de rangos por grupo:\n"); print(sum_rangos)
cat("\nEstadístico W por grupo:\n"); print(W_por_grupo)
cat("\nW (el menor se usa como estadístico de prueba):", min(W_por_grupo), "\n")


## -----------------------------------------------------------------------------
#| label: fig-wilcoxon-ranking
#| fig-cap: "Distribución del AUC (área bajo la curva de permeabilidad gaseosa) por tipo de composite dental. Los rangos asignados a cada observación se usan para calcular el estadístico W de Wilcoxon. El composite Filtek 3M muestra consistentemente valores de AUC más altos (peor sellado) que SonicFill KERR."
#| fig-width: 6
#| fig-height: 5
#| message: false

library(ggplot2)
ggplot(dat_auc, aes(Group, AUC, fill = Group)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.5, size = 2.5,
              aes(colour = Group)) +
  scale_fill_manual(values  = c("#2E86C1","#E74C3C")) +
  scale_colour_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Composite dental",
       y = "AUC (permeabilidad gaseosa)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

