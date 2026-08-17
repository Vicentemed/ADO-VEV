## -----------------------------------------------------------------------------
#| label: c03-instalar-paquete
#| eval: false
# install.packages("nombre_del_paquete")


## -----------------------------------------------------------------------------
#| label: c03-cargar-paquete
#| eval: false
# library(nombre_del_paquete)


## -----------------------------------------------------------------------------
#| label: tbl-paquetes
#| tbl-cap: "Paquetes de R utilizados frecuentemente en este libro y sus funciones principales."
#| echo: false

paquetes <- data.frame(
  Paquete = c("car", "ggplot2", "dplyr", "psych", "table1",
              "multcomp", "survival", "survminer", "VIM", "pROC"),
  Función = c(
    "Pruebas de Levene, regresión avanzada",
    "Visualización de datos (gramática de gráficos)",
    "Manipulación de datos (filter, select, mutate)",
    "Estadística descriptiva y correlaciones",
    "Tablas descriptivas listas para publicación",
    "Comparaciones múltiples post-hoc (Tukey, Dunnett)",
    "Análisis de supervivencia (Kaplan-Meier, Cox)",
    "Visualización de curvas de supervivencia",
    "Visualización e imputación de datos faltantes",
    "Curva ROC y área bajo la curva (AUC)"
  )
)

knitr::kable(paquetes, align = c("l", "l"))


## -----------------------------------------------------------------------------
#| label: version-r
# Ver la versión de R instalada y los paquetes cargados
R.version.string

