## -----------------------------------------------------------------------------
#| label: calculo-chi2-manual
# Tabla de contingencia observada
obs <- matrix(c(28, 10, 48, 114),
              nrow = 2, byrow = TRUE,
              dimnames = list(c("Experimental","Control"),
                              c("Evento: Sí","Evento: No")))

N <- sum(obs)
totales_fila <- rowSums(obs)
totales_col  <- colSums(obs)

# Frecuencias esperadas
esp <- outer(totales_fila, totales_col) / N

cat("=== Frecuencias OBSERVADAS ===\n"); print(obs)
cat("\n=== Frecuencias ESPERADAS ===\n"); print(round(esp, 2))

# Chi-cuadrado manual
chi2_manual <- sum((obs - esp)^2 / esp)
df_chi2     <- (nrow(obs) - 1) * (ncol(obs) - 1)
p_chi2      <- pchisq(chi2_manual, df = df_chi2, lower.tail = FALSE)

cat("\nchi2 =", round(chi2_manual, 3), "| df =", df_chi2, "| p =", round(p_chi2, 4), "\n")


## -----------------------------------------------------------------------------
#| label: c37-verificacion-chisq-test
# Verificación con chisq.test()
chisq.test(obs, correct = FALSE)


## -----------------------------------------------------------------------------
#| label: or-chi2
A <- obs["Experimental","Evento: Sí"]
B <- obs["Experimental","Evento: No"]
C <- obs["Control","Evento: Sí"]
D <- obs["Control","Evento: No"]

OR <- (A/B) / (C/D)
cat("Odds Ratio =", round(OR, 2), "\n")
cat("Los momios (odds) de presentar el evento en el grupo experimental son",
    round(OR, 2), "veces los del grupo control.\n")


## -----------------------------------------------------------------------------
#| label: c37-chisq-yates
# Con corrección de Yates (default para tablas 2×2)
chisq.test(obs, correct = TRUE)


## -----------------------------------------------------------------------------
#| label: likelihood-ratio
LX2 <- 2 * sum(obs * log(obs / esp))
p_LX2 <- pchisq(LX2, df = df_chi2, lower.tail = FALSE)
cat("L chi2 (razón de verosimilitud) =", round(LX2, 3),
    "| p =", round(p_LX2, 4), "\n")


## -----------------------------------------------------------------------------
#| label: lr-en-glm
#| message: false
# La razón de verosimilitud también se obtiene al comparar modelos GLM
dat_ex <- data.frame(
  grupo  = factor(c(rep("Exp",38), rep("Con",162))),
  evento = c(rep(1,28), rep(0,10), rep(1,48), rep(0,114))
)

mod_full <- glm(evento ~ grupo, data = dat_ex, family = binomial)
mod_null <- glm(evento ~ 1,    data = dat_ex, family = binomial)

# LR test = comparación de deviance entre modelo completo y nulo
anova(mod_null, mod_full, test = "LRT")

