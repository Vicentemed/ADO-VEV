## -----------------------------------------------------------------------------
#| label: fig-covariable-independencia
#| fig-cap: "Esquema de la independencia de la covariable. Izquierda (experimento verdadero): la covariable se superpone con la varianza del error pero no con la del experimento — ANCOVA correcto. Derecha (cuasiexperimento): la covariable solapa la varianza experimental — ANCOVA incorrecto, producirá sesgos."
#| fig-width: 9
#| fig-height: 3.5
#| echo: false

par(mfrow = c(1, 2), mar = c(1, 1, 2, 1))

draw_circles <- function(title, overlap_exp = FALSE) {
  plot(0, 0, type = "n", xlim = c(-2, 5), ylim = c(-2, 3),
       xaxt = "n", yaxt = "n", bty = "n", main = title, asp = 1)
  # Respuesta (grande, centro)
  symbols(1, 0.5, circles = 1.5, add = TRUE, inches = FALSE,
          fg = "#2E86C1", bg = rgb(0.18, 0.54, 0.76, 0.2), lwd = 2)
  text(1, 0.5, "Respuesta\n(Y)", cex = 0.75, col = "#2E86C1")
  # Experimento
  symbols(-0.5, 0.5, circles = 0.9, add = TRUE, inches = FALSE,
          fg = "#27AE60", bg = rgb(0.15, 0.68, 0.38, 0.3), lwd = 2)
  text(-0.5, 0.5, "Exp (Grupos)", cex = 0.7, col = "#27AE60")
  # Covariable (posición depende de overlap_exp)
  cx <- if (overlap_exp) 0.2 else 2.5
  symbols(cx, -0.8, circles = 0.9, add = TRUE, inches = FALSE,
          fg = "#E74C3C", bg = rgb(0.91, 0.30, 0.24, 0.3), lwd = 2)
  text(cx, -0.8, "Cov.", cex = 0.7, col = "#E74C3C")
}

draw_circles("Experimento verdadero\n(ANCOVA correcto)", overlap_exp = FALSE)
draw_circles("Cuasiexperimento\n(ANCOVA incorrecto)", overlap_exp = TRUE)

par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
#| label: carga-ancova
dat <- read.table("sets de datos/datBF.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
str(dat)


## -----------------------------------------------------------------------------
#| label: fig-pendientes-ancova
#| fig-cap: "Relación entre la fluorescencia basal (Basal) y la fluorescencia final (Final) de las lesiones de mancha blanca por grupo de tratamiento. Las líneas de regresión paralelas entre grupos indican que el supuesto de homogeneidad de pendientes de regresión se cumple — la relación entre covariable y respuesta es similar en los tres grupos."
#| fig-width: 6
#| fig-height: 5
#| message: false

library(ggplot2)
dat$Group <- factor(dat$Group, levels = c("G2","G1","G3"))

ggplot(dat, aes(Basal, Final, colour = Group)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.10) +
  scale_colour_manual(values = c("#555555","#2E86C1","#E74C3C")) +
  labs(x = "Fluorescencia basal (UF)",
       y = "Fluorescencia final (UF)",
       colour = "Grupo") +
  theme_bw(base_size = 12)

