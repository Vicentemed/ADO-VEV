## -----------------------------------------------------------------------------
#| label: tbl-np-equiv
#| tbl-cap: "Correspondencia entre pruebas paramétricas y sus alternativas no paramétricas."
#| echo: false

equiv <- data.frame(
  `Prueba paramétrica`    = c("t-test independiente", "t-test apareado",
                               "ANOVA de una vía", "ANOVA medidas repetidas",
                               "Correlación de Pearson"),
  `Alternativa no paramétrica` = c("Wilcoxon suma de rangos (Mann-Whitney)",
                               "Wilcoxon rangos con signo",
                               "Kruskal-Wallis",
                               "ANOVA de Friedman",
                               "Spearman / Kendall"),
  Capítulo = c("27–28", "29–30", "31–32", "33–34", "8"),
  check.names = FALSE
)
knitr::kable(equiv, align = c("l","l","c"))


## -----------------------------------------------------------------------------
#| label: demo-ranking
valores <- c(4.2, 8.1, 2.5, 8.1, 5.3, 1.0, 6.7, 3.8)

# Paso 1: ordenar los valores de menor a mayor
val_ord <- sort(valores)

# Paso 2: rango potencial = posición de cada valor en el orden
rango_potencial <- seq_along(val_ord)

# Paso 3: rango final = promedio de los rangos potenciales en caso de empate
#         (rank() usa ties.method = "average" por defecto)
rango_final <- rank(val_ord)

data.frame(
  `Valor ordenado`  = val_ord,
  `Rango potencial` = rango_potencial,
  `Rango final`     = rango_final,
  check.names = FALSE
) |>
  knitr::kable(
    caption = "Proceso de asignación de rangos. Tras ordenar los valores de menor a mayor, cada uno recibe un rango potencial según su posición. Los dos valores empatados en el valor 8.1 (posiciones 7 y 8) reciben como rango final el promedio de sus rangos potenciales: (7 + 8) / 2 = 7.5. A este evento se le denomina *ties* (empates).")


## -----------------------------------------------------------------------------
#| label: fig-dist-asimetrica
#| echo: false
#| fig-cap: "Comparación entre media (línea roja) y mediana (línea azul) en una distribución sesgada. La mediana es más representativa del valor típico cuando los datos tienen distribución asimétrica o valores atípicos."
#| fig-width: 7
#| fig-height: 4

set.seed(42)
datos_asim <- c(rexp(50, rate = 0.2))

hist(datos_asim, main = "Distribución asimétrica",
     xlab = "Valor", col = "#AED6F1", border = "white", breaks = 15)
abline(v = mean(datos_asim),   col = "#E74C3C", lwd = 2)
abline(v = median(datos_asim), col = "#2E86C1", lwd = 2, lty = 2)
legend("topright",
       legend = c(paste0("Media = ",   round(mean(datos_asim),1)),
                  paste0("Mediana = ", round(median(datos_asim),1))),
       col = c("#E74C3C","#2E86C1"), lty = c(1,2), lwd = 2, bty = "n")

