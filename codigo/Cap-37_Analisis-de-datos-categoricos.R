## -----------------------------------------------------------------------------
#| label: tbl-contingencia-ejemplo
#| tbl-cap: "Ejemplo de tabla de contingencia 2×2 con las frecuencias observadas y sus totales marginales."

tabla_ej <- matrix(c(28, 10, 48, 114),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(
                     c("Experimental","Control"),
                     c("Evento: Sí","Evento: No")
                   ))
knitr::kable(addmargins(tabla_ej))


## -----------------------------------------------------------------------------
#| label: tbl-efectos-cat
#| tbl-cap: "Medidas de asociación para datos categóricos y su interpretación."
#| echo: false

efectos <- data.frame(
  Medida = c("Odds Ratio (OR)", "Riesgo Relativo (RR)",
             "Diferencia de proporciones",
             "Phi (φ)", "V de Cramér"),
  `Tipo de tabla` = c("2×2", "2×2 (estudios prospectivos)",
                      "2×2",
                      "2×2", "Tablas mayores"),
  Interpretación = c(
    "OR=1: sin asociación; OR>1: mayores momios del evento en expuestos",
    "RR=1: sin efecto; RR>1: mayor riesgo en expuestos",
    "0: sin diferencia; ±1: diferencia máxima",
    "0: sin asociación; 1: asociación perfecta",
    "0: sin asociación; 1: asociación perfecta"
  )
)
knitr::kable(efectos, check.names = FALSE, align = c("l","l","l"))


## -----------------------------------------------------------------------------
#| label: fig-barras-cat
#| echo: false
#| fig-cap: "Visualización de la distribución de un evento entre grupos experimental y control. Las barras apiladas muestran la proporción de eventos (sí/no) para cada grupo, facilitando la comparación visual antes del análisis estadístico."
#| fig-width: 6
#| fig-height: 4
#| message: false

library(ggplot2)
dat_cat <- data.frame(
  Grupo   = c(rep("Experimental",38), rep("Control",162)),
  Evento  = c(rep("Sí",28), rep("No",10), rep("Sí",48), rep("No",114))
)
dat_cat$Evento <- factor(dat_cat$Evento, levels = c("Sí","No"))

ggplot(dat_cat, aes(Grupo, fill = Evento)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Grupo", y = "Proporción", fill = "Evento") +
  theme_bw(base_size = 12)

