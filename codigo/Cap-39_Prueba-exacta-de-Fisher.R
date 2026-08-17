## -----------------------------------------------------------------------------
#| label: fisher-ejemplo-cap37
obs <- matrix(c(28, 10, 48, 114),
              nrow = 2, byrow = TRUE,
              dimnames = list(c("Experimental","Control"),
                              c("Evento: Sí","Evento: No")))

fisher.test(obs)


## -----------------------------------------------------------------------------
#| label: tbl-fisher-vs-pearson
#| tbl-cap: "Criterios para seleccionar entre la prueba chi-cuadrado de Pearson y la prueba exacta de Fisher."
#| echo: false

criterios <- data.frame(
  Criterio = c("Frecuencia esperada mínima ≥ 5",
               "Alguna celda con frecuencia esperada < 5",
               "Muestra total N < 20",
               "Tabla 2×2 con totales marginales fijos"),
  `Prueba recomendada` = c("Chi-cuadrado de Pearson",
                            "Fisher exacta",
                            "Fisher exacta",
                            "Fisher exacta"),
  check.names = FALSE
)
knitr::kable(criterios, align = c("l","l"))


## -----------------------------------------------------------------------------
#| label: fisher-ejemplo
# Tabla con una celda de frecuencia esperada < 5
tabla_small <- matrix(c(4, 2, 1, 10),
                      nrow = 2, byrow = TRUE,
                      dimnames = list(c("Tratamiento A","Tratamiento B"),
                                      c("Éxito","Fracaso")))

cat("=== Tabla observada ===\n"); print(tabla_small)

# Verificar frecuencias esperadas
esp_small <- chisq.test(tabla_small)$expected
cat("\n=== Frecuencias esperadas ===\n"); print(round(esp_small, 2))

cat("\n¿Alguna frecuencia esperada < 5?", any(esp_small < 5), "\n")


## -----------------------------------------------------------------------------
#| label: c38-chi-vs-fisher
# Chi-cuadrado (solo para comparación — NO usar con frecuencias bajas)
chisq.test(tabla_small, correct = TRUE)$p.value

# Fisher exacta (CORRECTO para este caso)
fisher.test(tabla_small)


## -----------------------------------------------------------------------------
#| label: fisher-rxc
# Tabla 3×2 con frecuencias esperadas bajas
tabla_3x2 <- matrix(c(3, 1, 5, 2, 1, 8),
                    nrow = 3, byrow = TRUE,
                    dimnames = list(c("G1","G2","G3"), c("Sí","No")))

fisher.test(tabla_3x2)

