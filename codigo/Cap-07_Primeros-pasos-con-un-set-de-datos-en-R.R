## -----------------------------------------------------------------------------
#| label: setup-patients
patientNames  <- c("Tony", "James", "Pedro", "Martin")
patientAge    <- c(45, 42, 38, 44)
patientWeight <- c(80, 75, 65, 79)
patientSize   <- c(1.72, 1.73, 1.69, 1.80)

Patients <- data.frame(
  Name   = patientNames,
  Age    = patientAge,
  Weight = patientWeight,
  Size   = patientSize
)
Patients$IMC   <- Patients$Weight / (Patients$Size^2)
Patients$Dx    <- as.Date(c("2015-02-18","2011-05-22","2009-01-02","2014-09-03"))
Patients$Event <- as.Date(c("2018-04-11","2017-06-29","2019-04-12","2018-01-23"))
Patients$DaysD <- as.numeric(Patients$Event - Patients$Dx)


## -----------------------------------------------------------------------------
#| label: c06b-datos-faltantes
# Set de datos con valores faltantes simulados
patientAge    <- c(45, NA, 38, 44)
patientWeight <- c(80, 75, 65, NA)
patientSize   <- c(1.72, 1.73, NA, 1.80)

PatientsNA <- data.frame(
  Name   = c("Tony","James","Pedro","Martin"),
  Age    = patientAge,
  Weight = patientWeight,
  Size   = patientSize
)
knitr::kable(PatientsNA, caption = "Set de datos con valores faltantes (NA).")


## -----------------------------------------------------------------------------
#| label: fig-missing-data
#| fig-cap: "Patrón de datos faltantes en el set de datos PatientsNA. El diagrama de barras (izquierda) muestra el porcentaje de NA por variable; el gráfico de patrón (derecha) muestra qué combinaciones de variables tienen datos faltantes simultáneamente."
#| fig-width: 7
#| fig-height: 4
#| message: false
#| warning: false

library(VIM)
aggr_plot <- aggr(PatientsNA,
                  col       = c("navyblue", "red"),
                  numbers   = TRUE,
                  sortVars  = TRUE,
                  labels    = names(PatientsNA),
                  cex.axis  = 0.8,
                  gap       = 3,
                  ylab      = c("Porcentaje de datos faltantes", "Patrón"))


## -----------------------------------------------------------------------------
#| label: c06b-resumen-na
summary(PatientsNA)


## -----------------------------------------------------------------------------
#| label: c06b-eliminar-na
PatientsNA_completo <- na.omit(PatientsNA)
knitr::kable(PatientsNA_completo, caption = "Set de datos tras eliminar renglones con NA.")


## -----------------------------------------------------------------------------
#| label: c06b-inspeccion-estructura
str(Patients)


## -----------------------------------------------------------------------------
#| label: c06b-seleccion-variables
Patients2 <- Patients[, c("Name", "IMC", "DaysD")]
str(Patients2)


## -----------------------------------------------------------------------------
#| label: c06b-anadir-variable
Patients2$Groups <- c("Exp", "Exp", "Control", "Control")


## -----------------------------------------------------------------------------
#| label: c06b-convertir-factor
Patients2$Groups <- factor(Patients2$Groups, levels = c("Control", "Exp"))
str(Patients2)


## -----------------------------------------------------------------------------
#| label: c06b-filtrado-grupo
PatientsControl <- Patients2[Patients2$Groups == "Control", ]
PatientsExp     <- subset(Patients2, Groups == "Exp")

knitr::kable(PatientsControl, caption = "Pacientes del grupo control.")
knitr::kable(PatientsExp,     caption = "Pacientes del grupo experimental.")


## -----------------------------------------------------------------------------
#| label: c06b-filtrado-sobrepeso
PatientsOW <- subset(Patients, IMC > 25)
knitr::kable(PatientsOW[, c("Name","Age","Weight","IMC")],
             caption = "Pacientes con IMC > 25 (sobrepeso).")


## -----------------------------------------------------------------------------
#| label: c06b-ifelse-categoria
Patients$Peso <- ifelse(Patients$IMC > 25, "Sobrepeso", "Normopeso")
knitr::kable(Patients[, c("Name","IMC","Peso")],
             caption = "Clasificación nutricional basada en IMC.")


## -----------------------------------------------------------------------------
#| label: c06b-cut-categorias
Patients$Peso3 <- cut(Patients$IMC,
                      breaks = c(18, 24.9, 29.9, 40),
                      labels = c("Normopeso", "Sobrepeso", "Obesidad"),
                      right  = TRUE)
knitr::kable(Patients[, c("Name","IMC","Peso3")],
             caption = "Clasificación nutricional en tres categorías usando cut().")


## -----------------------------------------------------------------------------
#| label: c06b-filtrado-multiple
# Pacientes mayores de 40 años Y con peso menor a 80 kg
PatFiltered <- subset(Patients, Age > 40 & Weight < 80)
knitr::kable(PatFiltered[, c("Name","Age","Weight")],
             caption = "Pacientes mayores de 40 años con peso < 80 kg.")


## -----------------------------------------------------------------------------
#| label: demo-wide-long
# Formato wide: 5 sujetos, 3 tiempos de medición
Wide <- data.frame(
  SubjectID = 1:5,
  Sex       = c("M","F","F","M","M"),
  T1        = c(40.1, 39.5, 38.2, 41.0, 42.1),
  T2        = c(70.3, 65.2, 65.9, 71.3, 72.5),
  T3        = c(50.3, 51.2, 48.6, 51.3, 47.2)
)

knitr::kable(Wide, caption = "Datos en formato wide: cada tiempo es una columna.")


## -----------------------------------------------------------------------------
#| label: c06b-formato-long
# Formato long: cada observación en un renglón
Long <- data.frame(
  SubjectID = rep(1:5, times = 3),
  Sex       = rep(c("M","F","F","M","M"), times = 3),
  Measures  = rep(c("T1","T2","T3"), each = 5),
  Response  = c(40.1,39.5,38.2,41.0,42.1,
                70.3,65.2,65.9,71.3,72.5,
                50.3,51.2,48.6,51.3,47.2)
)

knitr::kable(Long, caption = "Los mismos datos en formato long: cada fila es una observación.")


## -----------------------------------------------------------------------------
#| label: c06b-wide-a-long
library(reshape2)

# Wide → Long
wide_to_Long <- melt(Wide,
                     id.vars      = c("SubjectID","Sex"),
                     variable.name = "Measures",
                     value.name    = "Response")
wide_to_Long$Measures <- as.character(wide_to_Long$Measures)
knitr::kable(wide_to_Long, caption = "Transformación de wide a long con melt().")


## -----------------------------------------------------------------------------
#| label: c06b-long-a-wide
# Long → Wide
long_to_Wide <- dcast(Long, SubjectID + Sex ~ Measures, value.var = "Response")
knitr::kable(long_to_Wide, caption = "Transformación de long a wide con dcast().")

