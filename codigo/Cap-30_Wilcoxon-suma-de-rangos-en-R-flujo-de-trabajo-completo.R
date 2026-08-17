## -----------------------------------------------------------------------------
#| label: carga-wilcoxon28
library(ggplot2); library(dplyr); library(car)

dat <- read.table("Examples/AUCFil.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat$Group <- factor(dat$Group)
head(dat)


## -----------------------------------------------------------------------------
#| label: fig-boxplot-wilcoxon28
#| fig-cap: "Distribución del AUC de microfiltración por tipo de composite dental. La distribución asimétrica y la presencia de valores atípicos justifican el uso de la prueba de Wilcoxon en lugar de la prueba t."
#| fig-width: 5
#| fig-height: 5

ggplot(dat, aes(Group, AUC, fill = Group)) +
  geom_boxplot(alpha = 0.6, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 2) +
  scale_fill_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Composite dental", y = "AUC (permeabilidad gaseosa)") +
  theme_bw(base_size = 12) + theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: c28-descriptiva-mediana
dat %>%
  group_by(Group) %>%
  summarise(n       = n(),
            Mediana = median(AUC, na.rm = TRUE),
            IQR     = IQR(AUC, na.rm = TRUE),
            Min     = min(AUC, na.rm = TRUE),
            Max     = max(AUC, na.rm = TRUE)) |>
  knitr::kable(caption = "Estadística descriptiva del AUC por grupo (mediana e IQR para datos no normales).")


## -----------------------------------------------------------------------------
#| label: c28-table1-resumen
#| message: false
library(table1)
table1(~ AUC | Group, data = dat)


## -----------------------------------------------------------------------------
#| label: c28-shapiro-levene
sw <- shapiro.test(dat$AUC)
sw
lev <- leveneTest(AUC ~ Group, data = dat, center = median)
lev


## -----------------------------------------------------------------------------
#| label: wilcoxon-test28
WModel <- wilcox.test(AUC ~ Group, data = dat,
                      alternative = "two.sided",
                      exact = FALSE,           # aproximación normal (hay empates)
                      correct = TRUE)          # corrección de continuidad
WModel


## -----------------------------------------------------------------------------
#| label: efecto-wilcoxon28
# r a partir de Z (aproximación normal)
rbyW <- function(wilcoxMod, N) {
  z <- qnorm(wilcoxMod$p.value / 2)
  r <- abs(z) / sqrt(N)   # se reporta la magnitud del efecto
  cat("Efecto r =", round(r, 3), "\n")
  invisible(r)
}
r_efecto <- rbyW(WModel, nrow(dat))

# r a partir de la diferencia de rangos medios
dat$Rango <- rank(dat$AUC)
R_medios  <- tapply(dat$Rango, dat$Group, mean)
n_grupos  <- tapply(dat$AUC,   dat$Group, length)
r_rangos  <- (2 * abs(diff(R_medios))) / sum(n_grupos)
cat("Efecto r (por rangos medios) =", round(r_rangos, 3), "\n\n")

# Media y DE por grupo (conviene reportarlas además de mediana/IQR; ver @sec-pruebas-no-parametricas)
resumen_md <- rbind(Media = tapply(dat$AUC, dat$Group, mean),
                    DE    = tapply(dat$AUC, dat$Group, sd))
print(round(resumen_md, 2))

cat("\nDiferencia de medianas de AUC =",
    round(abs(diff(tapply(dat$AUC, dat$Group, median))), 2), "\n")
cat("Diferencia de medias de AUC   =",
    round(abs(diff(tapply(dat$AUC, dat$Group, mean))), 2), "\n")

