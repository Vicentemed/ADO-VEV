## -----------------------------------------------------------------------------
#| label: kappa-manual
# Ejemplo con tres tablas para ilustrar el efecto de prevalencia
calcular_kappa <- function(a, b, cc, d) {
  N  <- a + b + cc + d
  P0 <- (a + d) / N
  Pe <- ((a+b)/N)*((a+cc)/N) + ((cc+d)/N)*((b+d)/N)
  k  <- (P0 - Pe) / (1 - Pe)
  cat(sprintf("Tabla: a=%d b=%d c=%d d=%d | P0=%.3f Pe=%.3f κ=%.3f\n",
              a, b, cc, d, P0, Pe, k))
}

cat("=== Baja prevalencia (P0=0.90) ===\n")
calcular_kappa(1, 4, 6, 89)   # κ bajo a pesar de alto acuerdo

cat("\n=== Alta prevalencia (P0=0.90) ===\n")
calcular_kappa(89, 2, 8, 1)   # κ bajo también

cat("\n=== Prevalencia simétrica (P0=0.90) ===\n")
calcular_kappa(55, 9, 1, 35)  # κ alto


## -----------------------------------------------------------------------------
#| label: tbl-kappa-interp
#| tbl-cap: "Escala de interpretación del Kappa de Cohen según Landis y Koch (1977)."
#| echo: false

interp_kappa <- data.frame(
  `Valor κ`       = c("< 0.00","0.00–0.20","0.21–0.40","0.41–0.60","0.61–0.80","0.81–1.00"),
  Interpretación  = c("Sin acuerdo","Ligero","Razonable","Moderado","Sustancial","Casi perfecto"),
  check.names = FALSE
)
knitr::kable(interp_kappa, align = c("c","l"))


## -----------------------------------------------------------------------------
#| label: tbl-icc-tipos
#| tbl-cap: "Modelos de ICC y cuándo usar cada uno (basado en Koo & Li, 2016)."
#| echo: false

icc_tipos <- data.frame(
  Modelo = c("ICC(1,1)", "ICC(2,1)", "ICC(3,1)",
             "ICC(1,k)", "ICC(2,k)", "ICC(3,k)"),
  Descripción = c(
    "Un factor, aleatorio. Cada sujeto es evaluado por un evaluador distinto, y los evaluadores se eligen al azar.",
    "Dos factores, aleatorio. Una muestra de k evaluadores, elegidos al azar, evalúa a cada sujeto.",
    "Dos factores, efectos mixtos. Los evaluadores seleccionados son los únicos evaluadores de interés.",
    "Un factor, aleatorio. Como ICC(1,1), pero la unidad es la media de k mediciones.",
    "Dos factores, aleatorio. Como ICC(2,1), pero la unidad es la media de k mediciones.",
    "Dos factores, efectos mixtos. Evaluadores fijos; la unidad es la media de k mediciones."
  ),
  `Cuándo usar` = c(
    "Rara vez, con la excepción de los estudios multicéntricos.",
    "Cuando interesa generalizar la fiabilidad a cualquier evaluador que posea las mismas características.",
    "Cuando solo se necesita la fiabilidad del experimento; no puede generalizarse.",
    "Igual que ICC(1,1), pero cuando interesa la media de múltiples observaciones.",
    "Igual que ICC(2,1), pero cuando interesa la media de múltiples observaciones.",
    "Igual que ICC(3,1), pero cuando interesa la media de múltiples observaciones."
  ),
  check.names = FALSE
)
knitr::kable(icc_tipos, align = c("l","l","l"))


## -----------------------------------------------------------------------------
#| label: tbl-icc-interp
#| tbl-cap: "Interpretación del ICC según Koo & Li (2016)."
#| echo: false

icc_interp <- data.frame(
  `Valor ICC` = c("< 0.50","0.50–0.75","0.75–0.90","> 0.90"),
  Interpretación = c("Pobre","Moderada","Buena","Excelente"),
  check.names = FALSE
)
knitr::kable(icc_interp, align = c("c","l"))

