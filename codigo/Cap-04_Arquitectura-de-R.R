## -----------------------------------------------------------------------------
#| label: tbl-objetos-r
#| tbl-cap: "Tipos de objetos principales en R y sus características."
#| echo: false

objetos <- data.frame(
  Objeto  = c("vector", "factor", "matrix", "data.frame", "list", "function"),
  Descripción = c(
    "Secuencia de elementos del mismo tipo (numérico, carácter, lógico)",
    "Vector con niveles categóricos etiquetados",
    "Arreglo bidimensional de elementos del mismo tipo",
    "Tabla de datos: columnas pueden ser de diferentes tipos",
    "Colección de objetos de cualquier tipo",
    "Conjunto de instrucciones que realiza una tarea específica"
  ),
  Ejemplo = c(
    "c(1, 2, 3, 4)",
    "factor(c('Control', 'Exp', 'Control'))",
    "matrix(1:9, nrow=3)",
    "data.frame(x=1:3, y=c('a','b','c'))",
    "list(a=1, b='texto', c=TRUE)",
    "function(x) x^2"
  )
)
knitr::kable(objetos, align = c("l","l","l"))


## -----------------------------------------------------------------------------
#| label: demo-objetos
# Ejemplos de creación de objetos básicos
vector_numerico <- c(10, 20, 30, 40, 50)
vector_texto    <- c("Control", "Experimental", "Control")
factor_grupos   <- factor(vector_texto)
matriz_datos    <- matrix(1:9, nrow = 3, ncol = 3)

# Verificar el tipo de cada objeto
class(vector_numerico)
class(factor_grupos)
class(matriz_datos)


## -----------------------------------------------------------------------------
#| label: demo-dataframe
# Crear un data.frame con datos clínicos simulados
pacientes <- data.frame(
  ID     = 1:6,
  Grupo  = factor(c("Control","Control","Control","Exp","Exp","Exp")),
  Edad   = c(45, 52, 38, 49, 41, 55),
  NRS    = c(3, 5, 2, 7, 6, 8),
  Sexo   = factor(c("M","F","M","F","M","F"))
)
pacientes


## -----------------------------------------------------------------------------
#| label: c04-resumen-dataframe
# Resumen rápido de un data.frame
str(pacientes)
summary(pacientes)


## -----------------------------------------------------------------------------
#| label: fig-arquitectura-r
#| fig-cap: "Arquitectura simplificada del ecosistema R. El núcleo base de R se extiende mediante paquetes de CRAN, Bioconductor y GitHub. RStudio provee un entorno de desarrollo integrado (IDE) que facilita el trabajo con el motor R."
#| fig-width: 8
#| fig-height: 4
#| echo: false

par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 5),
     xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

# Motor R (centro)
rect(3.5, 1.5, 6.5, 3.5, col = "#2E86C1", border = "#1A5276", lwd = 2)
text(5.0, 2.5, "Motor R\n(R Base)", col = "white", font = 2, cex = 1.1)

# Repositorios de paquetes
repos <- data.frame(
  x1  = c(0.2, 0.2, 0.2),
  y1  = c(3.5, 2.2, 0.9),
  x2  = c(2.8, 2.8, 2.8),
  y2  = c(4.5, 3.2, 1.9),
  lab = c("CRAN\n(>20,000 pkgs)", "Bioconductor\n(Genómica)", "GitHub\n(Desarrollo)"),
  col = c("#27AE60", "#8E44AD", "#E67E22")
)
for (i in 1:3) {
  rect(repos$x1[i], repos$y1[i], repos$x2[i], repos$y2[i],
       col = repos$col[i], border = "white", lwd = 1.5)
  text(1.5, (repos$y1[i] + repos$y2[i])/2, repos$lab[i],
       col = "white", font = 2, cex = 0.8)
  arrows(repos$x2[i], (repos$y1[i] + repos$y2[i])/2,
         3.4, 2.5, col = repos$col[i], lwd = 1.5, length = 0.1)
}

# RStudio
rect(7.2, 1.5, 9.8, 3.5, col = "#E8F8F5", border = "#1ABC9C", lwd = 2)
text(8.5, 2.5, "RStudio\n(IDE)", col = "#1ABC9C", font = 2, cex = 1.0)
arrows(6.6, 2.5, 7.1, 2.5, col = "#555555", lwd = 2, length = 0.12, code = 3)

# Usuario
text(5.0, 4.6, "Usuario / Investigador", col = "#555555", font = 3, cex = 0.9)
arrows(5.0, 4.4, 5.0, 3.6, col = "#555555", lwd = 1.5, length = 0.1)


## -----------------------------------------------------------------------------
#| label: c04-instalar-paquetes
#| eval: false
# install.packages("car")        # Para prueba de Levene y regresión
# install.packages("ggplot2")    # Para visualización avanzada
# install.packages("survival")   # Para análisis de supervivencia


## -----------------------------------------------------------------------------
#| label: c04-cargar-paquetes
#| eval: false
# library(car)
# library(ggplot2)
# library(survival)


## -----------------------------------------------------------------------------
#| label: c04-sintaxis-funcion
#| eval: false
# nombre_funcion(argumento1 = valor1, argumento2 = valor2)


## -----------------------------------------------------------------------------
#| label: c04-funciones-basicas
datos <- c(5, 3, 7, 2, 8, 4, 6)
mean(datos)
sd(datos)     # desviación estándar
median(datos) # mediana


## -----------------------------------------------------------------------------
#| label: c04-ayuda-funciones
#| eval: false
# ?mean
# help("sd")

