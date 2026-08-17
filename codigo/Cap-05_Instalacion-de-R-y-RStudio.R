## -----------------------------------------------------------------------------
#| label: fig-rstudio-paneles
#| fig-cap: "Distribución de los cuatro paneles principales de RStudio. El panel de scripts (superior izquierdo) es donde se escribe el código; la consola (inferior izquierdo) ejecuta los comandos; el entorno (superior derecho) muestra los objetos activos; el panel de visualización (inferior derecho) muestra gráficos, archivos y documentación."
#| fig-width: 8
#| fig-height: 5
#| echo: false

par(mar = c(0.5, 0.5, 1.5, 0.5))
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 6),
     xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "",
     main = "Interfaz de RStudio — cuatro paneles")

# Paneles
paneles <- list(
  list(x1=0.1, y1=3.1, x2=4.9, y2=5.9, col="#D6EAF8", bord="#2E86C1",
       tit="1. Editor de Scripts", txt="Escribe y guarda código R\n(.R, .Rmd, .qmd)"),
  list(x1=0.1, y1=0.1, x2=4.9, y2=3.0, col="#D5F5E3", bord="#27AE60",
       tit="2. Consola", txt="Ejecuta código interactivamente\nMuestra resultados y mensajes"),
  list(x1=5.1, y1=3.1, x2=9.9, y2=5.9, col="#FDEBD0", bord="#E67E22",
       tit="3. Entorno / Historial", txt="Objetos activos en memoria\nHistorial de comandos"),
  list(x1=5.1, y1=0.1, x2=9.9, y2=3.0, col="#F4ECF7", bord="#8E44AD",
       tit="4. Archivos / Gráficos / Ayuda", txt="Visualización de gráficos\nExplorador de archivos\nDocumentación de funciones")
)

for (p in paneles) {
  rect(p$x1, p$y1, p$x2, p$y2, col = p$col, border = p$bord, lwd = 2)
  text((p$x1+p$x2)/2, p$y2 - 0.25, p$tit, font = 2, cex = 0.85, col = p$bord)
  text((p$x1+p$x2)/2, (p$y1+p$y2)/2, p$txt, cex = 0.75, col = "#333333")
}


## -----------------------------------------------------------------------------
#| label: verificacion-instalacion
# Verificar versión de R
R.version.string

# Operaciones básicas
2 + 2
sqrt(144)
log(exp(1))  # debe devolver 1

# Crear un vector y calcular estadísticos básicos
x <- c(3, 7, 5, 2, 8, 4, 6)
cat("Media:", mean(x), "\n")
cat("Mediana:", median(x), "\n")
cat("DE:", round(sd(x), 2), "\n")


## -----------------------------------------------------------------------------
#| label: c05-instalar-paquetes-libro
#| eval: false
# paquetes_libro <- c(
#   "car",         # Levene, regresión avanzada
#   "ggplot2",     # Visualización
#   "dplyr",       # Manipulación de datos
#   "psych",       # Estadística descriptiva
#   "table1",      # Tablas descriptivas para publicación
#   "multcomp",    # Comparaciones múltiples post-hoc
#   "survival",    # Análisis de supervivencia
#   "survminer",   # Curvas de Kaplan-Meier
#   "VIM",         # Datos faltantes
#   "pROC",        # Curva ROC y AUC
#   "irr",         # Concordancia (ICC, kappa)
#   "epiR",        # Concordancia (CCC de Lin)
#   "nlme",        # Modelos mixtos
#   "effects"      # Medias ajustadas (ANCOVA)
# )
# 
# install.packages(paquetes_libro)

