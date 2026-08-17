## -----------------------------------------------------------------------------
#| label: carga-pgmax
#| message: false
library(irr)

dat <- read.table("sets de datos/PGMax.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
str(dat)
head(dat)


## -----------------------------------------------------------------------------
#| label: kappa-dos-eval
# Kappa no ponderado entre dos evaluadores (Obs2 y Obs3)
k_unw <- kappa2(dat[, 2:3], "unweighted")
k_unw


## -----------------------------------------------------------------------------
#| label: kappa-ponderado
# Kappa ponderado lineal
k_lin <- kappa2(dat[, 2:3], "equal")
k_lin

# Kappa ponderado cuadrático (penaliza más los desacuerdos grandes)
k_sq <- kappa2(dat[, 2:3], "squared")
k_sq


## -----------------------------------------------------------------------------
#| label: kappa-multiple
# Kappa de Fleiss para todos los evaluadores
kappam.fleiss(dat[, 1:ncol(dat)])


## -----------------------------------------------------------------------------
#| label: gwet-ac2
#| message: false
library(irrCAC)

# AC1 de Gwet (no ponderado): aísla la robustez a la paradoja de prevalencia
gwet.ac1.raw(dat, weights = "unweighted",
             categ.labels = c("AI", "AII", "BII", "BIII"))$est

# AC2 de Gwet con pesos cuadráticos (apropiado para la escala ordinal)
gwet.ac1.raw(dat, weights = "quadratic",
             categ.labels = c("AI", "AII", "BII", "BIII"))$est


## -----------------------------------------------------------------------------
#| label: fig-kappa-matrix
#| fig-cap: "Matriz de concordancia entre los evaluadores Obs2 y Obs3. Los colores más oscuros en la diagonal principal indican mayor acuerdo. Las celdas fuera de la diagonal representan desacuerdos entre evaluadores."
#| fig-width: 6
#| fig-height: 5
#| message: false

library(ggplot2)
# Tabla de contingencia entre dos evaluadores
tab_kappa <- table(dat[,2], dat[,3])
tab_df    <- as.data.frame(tab_kappa)
names(tab_df) <- c("Obs2","Obs3","Frecuencia")

ggplot(tab_df, aes(Obs3, Obs2, fill = Frecuencia)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = Frecuencia), size = 4, colour = "black") +
  scale_fill_gradient(low = "white", high = "#2E86C1") +
  labs(x = "Observador 3", y = "Observador 2",
       title = "Matriz de acuerdo — Clasificación de terceros molares entre dos observadores") +
  theme_bw(base_size = 12)

