## -----------------------------------------------------------------------------
#| label: tbl-apendice-paquetes
#| tbl-cap: "Paquetes de R utilizados en el libro, funciones principales y contexto de uso."
#| echo: false

paquetes <- data.frame(
  Paquete = c(
    "blandr", "car", "caret", "dcurves", "dplyr", "drc", "effects", "epiR",
    "ez", "gam", "ggm", "ggplot2", "gmodels", "googlesheets4", "irr", "irrCAC",
    "knitr", "leaps", "lsr", "MASS", "multcomp", "nlme", "pastecs", "patchwork",
    "pgirmess", "ppsr", "pROC", "psych", "pwr", "qcc", "reshape2",
    "ResourceSelection", "rms", "rstatix", "scales", "survival",
    "survminer", "table1", "VIM"
  ),
  `Funciones principales` = c(
    "blandr.statistics(), blandr.draw()",
    "leveneTest(), Anova(), vif(), durbinWatsonTest(), qqPlot()",
    "train(), trainControl(), confusionMatrix()",
    "dca()",
    "group_by(), summarise(), filter(), mutate()",
    "drm(), LL.2()",
    "effect(), allEffects()",
    "epi.ccc()",
    "ezANOVA()",
    "gam(), s()",
    "pcor(), pcor.test()",
    "ggplot(), geom_*()",
    "CrossTable()",
    "gs4_auth(), read_sheet()",
    "kappa2(), icc()",
    "gwet.ac1.raw()",
    "kable()",
    "regsubsets()",
    "etaSquared()",
    "stepAIC(), data(birthwt)",
    "glht(), mcp()",
    "lme()",
    "stat.desc()",
    "Operadores +, |, /",
    "kruskalmc(), friedmanmc()",
    "score(), visualize_pps()",
    "roc(), auc(), coords()",
    "describe(), describeBy(), ICC()",
    "pwr.t.test(), pwr.anova.test()",
    "qcc.overdispersion.test()",
    "melt(), dcast()",
    "hoslem.test()",
    "lrm(), cph(), nomogram(), datadist(), validate()",
    "games_howell_test()",
    "percent()",
    "Surv(), survfit(), survdiff(), coxph(), cox.zph()",
    "ggsurvplot(), surv_summary(), ggcoxzph()",
    "table1()",
    "aggr()"
  ),
  `Uso en el libro` = c(
    "Concordancia: gráfico y límites de Bland-Altman",
    "Supuestos (Levene, Q-Q), ANOVA tipo II/III, multicolinealidad",
    "Validación cruzada de la regresión múltiple",
    "Análisis de curva de decisión (DCA) en regresión logística",
    "Manipulación y resumen de datos",
    "Modelos de crecimiento log-logístico (viabilidad celular)",
    "Medias ajustadas en ANCOVA",
    "Coeficiente de concordancia de Lin (CCC)",
    "ANOVA de medidas repetidas",
    "Modelos aditivos generalizados (suavizado) en regresión múltiple",
    "Correlación parcial y semiparcial",
    "Toda la visualización de datos del libro",
    "Tablas de contingencia con residuos estandarizados",
    "Carga de datos desde Google Sheets (demostración)",
    "Kappa de Cohen e ICC de concordancia",
    "AC1/AC2 de Gwet: concordancia robusta y ponderada",
    "Tablas con formato en todos los capítulos",
    "Selección de todos los subconjuntos (all-subsets) en regresión",
    "Tamaño de efecto eta cuadrada",
    "Selección de modelos y datos birthwt",
    "Comparaciones múltiples (Tukey, Dunnett)",
    "Enfoque multinivel de medidas repetidas",
    "Descriptivos con sesgo y curtosis estandarizados",
    "Combinar varios gráficos ggplot2 en una figura",
    "Post-hoc de Kruskal-Wallis y de Friedman",
    "Predictive Power Score (PPS) en el análisis de correlación",
    "Curva ROC y AUC en regresión logística",
    "Estadística descriptiva e ICC",
    "Cálculo de potencia y tamaño de muestra",
    "Prueba de sobredispersión en regresión logística",
    "Transformación de datos wide a long y viceversa",
    "Prueba de bondad de ajuste de Hosmer-Lemeshow",
    "Regresión logística/Cox, nomograma y validación por bootstrap",
    "Post-hoc de Games-Howell en ANOVA",
    "Formato de porcentajes en ejes y etiquetas",
    "Análisis de supervivencia (Kaplan-Meier y Cox)",
    "Curvas de supervivencia y diagnóstico de riesgos proporcionales (Cox)",
    "Tablas descriptivas listas para publicación",
    "Visualización del patrón de datos faltantes"
  ),
  check.names = FALSE
)

knitr::kable(paquetes, align = c("l", "l", "l"))


## -----------------------------------------------------------------------------
#| label: c58-instalar-paquetes
#| eval: false
# install.packages(c(
#   "blandr", "car", "caret", "dcurves", "dplyr", "drc", "effects", "epiR",
#   "ez", "gam", "ggm", "ggplot2", "gmodels", "googlesheets4", "irr", "irrCAC",
#   "knitr", "leaps", "lsr", "MASS", "multcomp", "nlme", "pastecs", "patchwork",
#   "pgirmess", "ppsr", "pROC", "psych", "pwr", "qcc", "reshape2",
#   "ResourceSelection", "rms", "rstatix", "scales", "survival",
#   "survminer", "table1", "VIM"
# ))
# 
# # Nota: el paquete 'ggm' (correlación parcial) depende del paquete 'graph',
# # disponible en Bioconductor y no en CRAN. Si su instalación falla, ejecutar:
# #   install.packages("BiocManager"); BiocManager::install("graph")


## -----------------------------------------------------------------------------
#| label: tbl-apendice-alternativas
#| tbl-cap: "Paquetes con alternativas más recientes y su estado actual."
#| echo: false

alternativas <- data.frame(
  Paquete = c("reshape2", "ez", "lsr", "caret", "survminer",
              "pgirmess", "pastecs", "gmodels"),
  Estado = c(
    "Retirado (superado por el autor)",
    "Sin mantenimiento reciente",
    "Antiguo (material docente)",
    "En mantenimiento",
    "Mantenimiento esporádico",
    "Nicho, activo",
    "Activo, clásico",
    "Activo, clásico"
  ),
  `Alternativa actualizada` = c(
    "tidyr (pivot_longer / pivot_wider) o data.table (melt / dcast)",
    "afex (aov_ez / aov_car) o rstatix",
    "effectsize (eta_squared, cohens_d), con intervalos de confianza",
    "tidymodels (parsnip, rsample, yardstick)",
    "ggsurvfit, integrado de forma nativa con ggplot2",
    "rstatix (dunn_test), FSA (dunnTest) o PMCMRplus",
    "rstatix o psych::describe() para descriptivos",
    "janitor (tabyl) para tablas de contingencia"
  ),
  check.names = FALSE
)

knitr::kable(alternativas, align = c("l", "l", "l"))


## -----------------------------------------------------------------------------
#| label: tbl-apendice-datos
#| tbl-cap: "Sets de datos utilizados en los ejemplos del libro."
#| echo: false

datos <- data.frame(
  `Set de datos` = c(
    "VickersF.txt", "DXKTw.txt", "Carbuse.txt", "datPerio.txt", "datcron.txt",
    "ReMat.txt", "datBF.txt", "AUCFil.txt", "viabSBF.txt", "datAsthma.txt",
    "datMusicT.txt", "PGMax.txt", "CRIT1.txt", "birthwt", "anxiety"
  ),
  Origen = c(
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Archivo de texto (Examples/)",
    "Paquete MASS",
    "Paquete irr"
  ),
  `Tema / análisis` = c(
    "Dureza de Vickers (2 materiales × 3 protocolos de endurecimiento): t-test, ANOVA, ANOVA de dos vías",
    "Dolor postoperatorio (NRS) en cirugía de terceros molares: supuestos, medidas repetidas, no paramétricas",
    "Rugosidad superficial (AFM) de fresas de carburo antes/después de uso: t-test dependiente",
    "Diagnóstico periodontal por profundidad de sondeo y factores asociados: chi-cuadrado y residuos estandarizados",
    "Edad dental, edad cronológica y maduración ósea vertebral: correlación",
    "Resistencia mecánica de ionómeros de vidrio reforzados con resina: Kruskal-Wallis",
    "Agentes remineralizantes en lesiones de mancha blanca (fluorescencia por DIAGNOdent): ANCOVA",
    "Microfiltración en composites dentales (Filtek 3M vs. SonicFill KERR; AUC de permeabilidad gaseosa): Wilcoxon de suma de rangos",
    "Viabilidad celular de suero fetal bovino comercial vs. de industria cárnica: modelos de crecimiento (drc)",
    "Parafunciones orales y asma en pacientes pediátricos: análisis de datos categóricos",
    "Musicoterapia y comportamiento en odontopediatría (diseño cruzado): prueba de McNemar",
    "Concordancia entre observadores en la clasificación de terceros molares: kappa",
    "Supervivencia de coronas de acero inoxidable (CRIT): Kaplan-Meier y regresión de Cox",
    "Predicción del peso al nacer a partir de factores maternos: regresión múltiple y logística",
    "Ansiedad evaluada por varios observadores: ICC entre evaluadores"
  ),
  Referencia = c(
    "Proyecto propio",
    "[@esparza2016]",
    "Proyecto propio",
    "Proyecto propio",
    "[@reverte2019]",
    "Proyecto propio",
    "[@esparza2021]",
    "Proyecto propio",
    "[@preciado2023]",
    "[@castaneda2022]",
    "Proyecto propio",
    "[@rivera2020]",
    "[@martinez2017]",
    "Paquete `MASS`",
    "Paquete `irr`"
  ),
  check.names = FALSE
)

knitr::kable(datos, align = c("l", "l", "l", "l"))


## -----------------------------------------------------------------------------
#| label: c58-info-sesion
#| eval: false
# sessionInfo()

