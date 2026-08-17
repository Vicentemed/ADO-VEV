## -----------------------------------------------------------------------------
#| label: carga-kw32
library(ggplot2); library(dplyr); library(car)

dat <- read.table("Examples/ReMat.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat$Groups <- factor(dat$Groups)
str(dat)


## -----------------------------------------------------------------------------
#| label: c32-descriptiva-tabla
dat %>%
  group_by(Groups) %>%
  summarise(n       = n(),
            Media   = round(mean(Response,   na.rm = TRUE), 2),
            DE      = round(sd(Response,     na.rm = TRUE), 2),
            Mediana = round(median(Response, na.rm = TRUE), 2),
            IQR     = round(IQR(Response,    na.rm = TRUE), 2)) |>
  knitr::kable(caption = "Estadística descriptiva de la resistencia mecánica por grupo (media, DE, mediana e IQR).")


## -----------------------------------------------------------------------------
#| label: c32-table1-resumen
#| message: false
library(table1)
table1(~ Response | Groups, data = dat)


## -----------------------------------------------------------------------------
#| label: c32-shapiro-levene
sh32 <- shapiro.test(dat$Response); sh32
lev32 <- leveneTest(Response ~ Groups, data = dat, center = median); lev32


## -----------------------------------------------------------------------------
#| label: kruskal-test32
KWModel <- kruskal.test(Response ~ Groups, data = dat)
KWModel


## -----------------------------------------------------------------------------
#| label: posthoc-kw32
#| message: false
if (!requireNamespace("pgirmess", quietly = TRUE)) install.packages("pgirmess")
library(pgirmess)

kruskalmc(Response ~ Groups, data = dat, probs = 0.05)


## -----------------------------------------------------------------------------
#| label: c32-posthoc-bonferroni
# Alternativa: pairwise Wilcoxon con corrección de Bonferroni
pairwise.wilcox.test(dat$Response, dat$Groups,
                     p.adjust.method = "bonferroni",
                     exact = FALSE)


## -----------------------------------------------------------------------------
#| label: fig-kw-results32
#| fig-cap: "Resistencia mecánica por grupo de material restaurador; los puntos negros representan la mediana de cada grupo. De acuerdo con el análisis post-hoc, CenN difiere significativamente de EQForteF y de RivaLC, mientras que estos dos últimos no difieren entre sí (α = 0.05)."
#| fig-width: 7
#| fig-height: 5

ggplot(dat, aes(Groups, Response, fill = Groups)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 2, colour = "gray30") +
  stat_summary(fun = median, geom = "point", size = 4, colour = "black") +
  labs(x = "Material restaurador",
       y = "Resistencia mecánica") +
  theme_bw(base_size = 12) + theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: efecto-kw
# eta cuadrado para Kruskal-Wallis
N   <- nrow(dat)
K   <- length(unique(dat$Groups))
eta2_kw <- (KWModel$statistic - K + 1) / (N - K)
cat("η² (eta cuadrado de Kruskal-Wallis) =", round(eta2_kw, 4), "\n")

