## -----------------------------------------------------------------------------
#| label: prep-friedman34
#| message: false
library(dplyr); library(reshape2); library(ggplot2)

dat <- read.table("Examples/DXKTw.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat$Grupo <- as.factor(dat$Grupo)
G1 <- subset(dat, Grupo == "1") %>%
  select(Px, Grupo, X0, X1, X2)

# Formato long para análisis
G1_long <- melt(G1[, c("Px","X0","X1","X2")], id.vars = "Px",
                variable.name = "Tiempo", value.name = "Dolor")
G1_long$Tiempo <- factor(G1_long$Tiempo, levels = c("X0","X1","X2"))
G1_long$Px     <- factor(G1_long$Px)
str(G1_long)


## -----------------------------------------------------------------------------
#| label: c34-descriptivos-por-tiempo
G1_long %>%
  group_by(Tiempo) %>%
  summarise(n       = n(),
            Media   = round(mean(Dolor,   na.rm = TRUE), 2),
            DE      = round(sd(Dolor,     na.rm = TRUE), 2),
            Mediana = round(median(Dolor, na.rm = TRUE), 2),
            IQR     = round(IQR(Dolor,    na.rm = TRUE), 2)) |>
  knitr::kable(caption = "Estadística descriptiva del dolor por tiempo de evaluación (grupo Dexketoprofeno).")


## -----------------------------------------------------------------------------
#| label: c34-tabla1-descriptivos
#| message: false
library(table1)
table1(~ Dolor | Tiempo, data = G1_long)


## -----------------------------------------------------------------------------
#| label: c34-formato-largo
wtol<-melt(G1, id = c("Px", "Grupo"), measured = c("X0","X1","X2"))
names(wtol)<-c("Px","Grupo","Times","Response")
wtol



## -----------------------------------------------------------------------------
#| label: friedman-test34
# Formato wide para friedman.test()
G1_wide <- as.matrix(G1[, c("X0","X1","X2")])
FriedModel <- friedman.test(G1_wide)
FriedModel


## -----------------------------------------------------------------------------
#| label: c34-posthoc-friedmanmc
# Alternativa con pgirmess
if (!requireNamespace("pgirmess", quietly=TRUE)) install.packages("pgirmess")
library(pgirmess)
friedmanmc(G1_wide)


## -----------------------------------------------------------------------------
#| label: efecto-friedman
N <- nrow(G1)
k <- 3
W_kendall <- FriedModel$statistic / (N * (k - 1))
cat("W de Kendall (tamaño de efecto) =", round(W_kendall, 4), "\n")
cat("Interpretación: 0-0.1 trivial | 0.1-0.3 pequeño | 0.3-0.5 mediano | >0.5 grande\n")

