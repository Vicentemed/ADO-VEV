## -----------------------------------------------------------------------------
#| label: c06-sintaxis-comando
#| eval: false
# objeto <- funcion(argumentos)


## -----------------------------------------------------------------------------
#| label: c06-crear-vector-nombres
patientNames <- c("Tony", "James", "Pedro", "Martin")
patientNames


## -----------------------------------------------------------------------------
#| label: c06-directorio-trabajo
#| eval: false
# # Windows
# setwd("C:/Users/MiUsuario/Desktop/MiProyecto")
# 
# # macOS / Linux
# setwd("/Users/MiUsuario/Desktop/MiProyecto")


## -----------------------------------------------------------------------------
#| label: c06-directorio-actual
#| eval: false
# getwd()


## -----------------------------------------------------------------------------
#| label: c06-listar-directorio
#| eval: false
# dir()


## -----------------------------------------------------------------------------
#| label: c06-instalar-paquete
#| eval: false
# install.packages("ggplot2")


## -----------------------------------------------------------------------------
#| label: c06-cargar-paquete
#| eval: false
# library(ggplot2)


## -----------------------------------------------------------------------------
#| label: c06-argumentos-funcion
args(rnorm)


## -----------------------------------------------------------------------------
#| label: c06-ayuda-rnorm
#| eval: false
# ?rnorm


## -----------------------------------------------------------------------------
#| label: c06-argumentos-nombre-posicion
# Por nombre (recomendado para claridad)
set.seed(42)
numeros <- rnorm(n = 30, mean = 30, sd = 2)

# Por posición (equivalente)
set.seed(42)
numeros <- rnorm(30, 30, 2)

# Ver estructura del objeto
str(numeros)
summary(numeros)


## -----------------------------------------------------------------------------
#| label: c06-crear-vectores
patientNames  <- c("Tony", "James", "Pedro", "Martin")
patientAge    <- c(45, 42, 38, 44)
patientWeight <- c(80, 75, 65, 79)
patientSize   <- c(1.72, 1.73, 1.69, 1.80)


## -----------------------------------------------------------------------------
#| label: c06-crear-dataframe
Patients <- data.frame(
  Name   = patientNames,
  Age    = patientAge,
  Weight = patientWeight,
  Size   = patientSize
)
knitr::kable(Patients, caption = "Set de datos de pacientes creado directamente en R.")


## -----------------------------------------------------------------------------
#| label: c06-calcular-imc
Patients$IMC <- Patients$Weight / (Patients$Size^2)
round(Patients$IMC, 2)


## -----------------------------------------------------------------------------
#| label: c06-fechas-tiempo
Patients$Dx    <- as.Date(c("2015-02-18","2011-05-22","2009-01-02","2014-09-03"))
Patients$Event <- as.Date(c("2018-04-11","2017-06-29","2019-04-12","2018-01-23"))
Patients$DaysD <- as.numeric(Patients$Event - Patients$Dx)

knitr::kable(Patients[, c("Name","Dx","Event","DaysD")],
             caption = "Fechas de diagnóstico y evento, con días transcurridos calculados.")


## -----------------------------------------------------------------------------
#| label: c06-exportar-datos
#| eval: false
# # Exportar como .csv
# write.csv(Patients, file = "Patients.csv", row.names = FALSE)
# 
# # Exportar como .txt (separado por tabulaciones)
# write.table(Patients, "Patients.txt", sep = "\t",
#             col.names = TRUE, row.names = FALSE, quote = TRUE, na = "NA")


## -----------------------------------------------------------------------------
#| label: c06-importar-datos
#| eval: false
# # Importar .csv
# datPatients <- read.csv("Patients.csv", header = TRUE, sep = ",",
#                         na.strings = "NA", dec = ".", strip.white = TRUE)
# 
# # Importar .txt
# datPatients <- read.table("Patients.txt", header = TRUE, sep = "\t",
#                           na.strings = "NA", dec = ".", strip.white = TRUE)

