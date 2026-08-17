## -----------------------------------------------------------------------------
#| label: tipo-I
dat_ex <- data.frame(
  Y   = c(3,5,2,7,4,6,8,1,9,3),
  A   = factor(rep(c("A1","A2"), 5)),
  B   = factor(rep(c("B1","B1","B2","B2","B1"), 2))
)
# Tipo I: el orden importa
anova(lm(Y ~ A + B, data = dat_ex))   # A primero
anova(lm(Y ~ B + A, data = dat_ex))   # B primero


## -----------------------------------------------------------------------------
#| label: comparacion-tipos-ss
#| message: false
library(car)

dat_vickers <- read.table("Examples/VickersF.txt", header = TRUE, sep = "\t",
                          na.strings = "NA", dec = ".", strip.white = TRUE)
dat_vickers$Group     <- factor(dat_vickers$Group)
dat_vickers$Cure.time <- factor(dat_vickers$Cure.time,
                                levels = c("Control","Dual","Light(20s)"))

# El diseño es perfectamente BALANCEADO: 15 observaciones en cada celda
table(dat_vickers$Group, dat_vickers$Cure.time)

mod <- lm(VH ~ Group * Cure.time, data = dat_vickers)

# Tipo I: secuencial -> se obtiene con anova() de R base
cat("=== Tipo I (secuencial) ===\n")
print(anova(mod))

# Tipo II y III: parciales -> con Anova() del paquete car
cat("\n=== Tipo II ===\n")
print(Anova(mod, type = "II"))

cat("\n=== Tipo III ===\n")
contrasts(dat_vickers$Group)     <- contr.sum
contrasts(dat_vickers$Cure.time) <- contr.sum
mod_iii <- lm(VH ~ Group * Cure.time, data = dat_vickers)
print(Anova(mod_iii, type = "III"))


## -----------------------------------------------------------------------------
#| label: comparacion-tipos-ss-desbal
#| message: false

# Desbalancear el mismo dataframe: quitar 5 obs de una celda y 8 de otra
quitar <- c(
  which(dat_vickers$Group == "Multilink" & dat_vickers$Cure.time == "Control")[1:5],
  which(dat_vickers$Group == "Ultimate"  & dat_vickers$Cure.time == "Light(20s)")[1:8]
)
dat_desbal <- dat_vickers[-quitar, ]
dat_desbal$Group     <- factor(dat_desbal$Group)
dat_desbal$Cure.time <- factor(dat_desbal$Cure.time,
                               levels = c("Control","Dual","Light(20s)"))

# Ahora el diseño está DESBALANCEADO (celdas con 7, 10 y 15 observaciones)
table(dat_desbal$Group, dat_desbal$Cure.time)


## -----------------------------------------------------------------------------
#| label: comparacion-tipos-ss-desbal-anova
#| message: false

# Tipo I con DOS órdenes distintos de los predictores
cat("=== Tipo I  ->  orden: Group + Cure.time ===\n")
print(anova(lm(VH ~ Group * Cure.time, data = dat_desbal)))

cat("\n=== Tipo I  ->  orden: Cure.time + Group ===\n")
print(anova(lm(VH ~ Cure.time * Group, data = dat_desbal)))

# Tipo II y III NO dependen del orden
mod_desbal <- lm(VH ~ Group * Cure.time, data = dat_desbal)
cat("\n=== Tipo II ===\n")
print(Anova(mod_desbal, type = "II"))

cat("\n=== Tipo III ===\n")
contrasts(dat_desbal$Group)     <- contr.sum
contrasts(dat_desbal$Cure.time) <- contr.sum
mod_desbal_iii <- lm(VH ~ Group * Cure.time, data = dat_desbal)
print(Anova(mod_desbal_iii, type = "III"))


## -----------------------------------------------------------------------------
#| label: tbl-tipos-ss
#| tbl-cap: "Guía de selección del tipo de suma de cuadrados según el diseño y los objetivos del análisis."
#| echo: false

tabla_ss <- data.frame(
  Tipo = c("Tipo I", "Tipo II", "Tipo III", "Tipo IV"),
  `Diseño balanceado` = c("✓ (si el orden es teórico)", "✓✓ (recomendado)", "✓✓", "✓✓"),
  `Diseño desbalanceado` = c("✗", "✓ (sin interacción)", "✓✓ (recomendado)", "✓✓"),
  `Con interacción` = c("✗", "✗", "✓✓", "✓✓"),
  `Datos faltantes` = c("✗", "✗", "✗", "✓✓"),
  check.names = FALSE
)
knitr::kable(tabla_ss, align = c("l","c","c","c","c"))

