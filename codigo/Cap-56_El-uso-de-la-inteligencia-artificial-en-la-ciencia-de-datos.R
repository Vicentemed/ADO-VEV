## -----------------------------------------------------------------------------
#| label: llm-funciones
logistica <- function(z) 1 / (1 + exp(-z))          # dos categorías
softmax   <- function(z) exp(z) / sum(exp(z))        # K categorías (vocabulario)


## -----------------------------------------------------------------------------
#| label: llm-siguiente-token
tokens <- c("restauración", "corona", "extracción", "endodoncia", "limpieza")
logits <- c(2.1, 1.7, 0.4, 1.2, -0.3)

prob <- softmax(logits)
names(prob) <- tokens
round(prob, 3)

cat("Token más probable:", tokens[which.max(prob)], "\n")


## -----------------------------------------------------------------------------
#| label: llm-softmax-logistica
z <- 1.4  # diferencia de puntaje entre dos tokens
c(softmax = softmax(c(z, 0))[1], logistica = logistica(z))


## -----------------------------------------------------------------------------
#| label: llm-temperatura
softmax_T <- function(z, temp = 1) softmax(z / temp)

rbind(
  "T = 0.5 (concentrada)" = round(softmax_T(logits, 0.5), 3),
  "T = 1.0 (base)"        = round(softmax_T(logits, 1.0), 3),
  "T = 2.0 (plana)"       = round(softmax_T(logits, 2.0), 3)
)


## -----------------------------------------------------------------------------
#| label: llm-pesos
# Representación (embedding) de la oración: 4 señales clínicas y su intensidad
contexto <- c(caries = 1.0, urgencia = 0.8, movilidad = -0.5, dolor = 0.2)

# Pesos: cada FILA es un token; cada COLUMNA, un rasgo del contexto
pesos <- matrix(
  c( 0.9,  0.3, -0.2,  0.1,    # restauración
     0.7,  0.6, -0.1,  0.0,    # corona
    -0.3,  0.2,  1.0,  0.3,    # extracción
     0.2,  0.4, -0.1,  0.9,    # endodoncia
    -0.4, -0.5, -0.2, -0.3),   # limpieza
  nrow = 5, byrow = TRUE,
  dimnames = list(tokens, names(contexto)))
pesos


## -----------------------------------------------------------------------------
#| label: llm-pesos-logit
# logit de "corona" = suma de (peso x rasgo)
sum(pesos["corona", ] * contexto)

# El mismo cálculo, a la vez, para todos los tokens (matriz de pesos x contexto)
logits_ctx <- as.vector(pesos %*% contexto)
names(logits_ctx) <- tokens
round(logits_ctx, 2)


## -----------------------------------------------------------------------------
#| label: llm-pesos-softmax
round(softmax(logits_ctx), 3)

