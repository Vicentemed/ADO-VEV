## -----------------------------------------------------------------------------
#| label: carga-remat
dat <- read.table("Examples/ReMat.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat$Groups <- factor(dat$Groups)
str(dat)


## -----------------------------------------------------------------------------
#| label: fig-boxplot-kruskal
#| fig-cap: "Distribución de la resistencia mecánica por grupo de material restaurador. La distribución asimétrica y las varianzas heterogéneas entre grupos justifican el uso de la prueba de Kruskal-Wallis en lugar del ANOVA paramétrico."
#| fig-width: 6
#| fig-height: 5
#| message: false

library(ggplot2)
ggplot(dat, aes(Groups, Response, fill = Groups)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.4, size = 2) +
  labs(x = "Material restaurador",
       y = "Resistencia mecánica (respuesta)") +
  theme_bw(base_size = 12) + theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: c31-shapiro-levene
library(car)
shapiro.test(dat$Response)
leveneTest(Response ~ Groups, data = dat, center = median)


## -----------------------------------------------------------------------------
#| label: calculo-H
dat$Rango <- rank(dat$Response)

rangos_grupo <- tapply(dat$Rango, dat$Groups, sum)
n_grupo      <- tapply(dat$Response, dat$Groups, length)
N            <- nrow(dat)

H_manual <- (12 / (N * (N + 1))) * sum(rangos_grupo^2 / n_grupo) - 3 * (N + 1)
p_manual  <- pchisq(H_manual, df = length(unique(dat$Groups)) - 1, lower.tail = FALSE)

cat("Suma de rangos por grupo (R_k):\n"); print(rangos_grupo)
cat("Tamaño por grupo (n_k):\n");        print(n_grupo)
cat("N =", N, "\n\n")
cat("H (calculado manualmente):", round(H_manual, 4), "\n")
cat("p-value:", format.pval(p_manual), "\n")


## -----------------------------------------------------------------------------
#| label: c31-rangos-medios
# Medias de rangos por grupo
tapply(dat$Rango, dat$Groups, mean)

