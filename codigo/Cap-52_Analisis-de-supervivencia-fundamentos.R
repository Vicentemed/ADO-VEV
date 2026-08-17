## -----------------------------------------------------------------------------
#| label: fig-censura-demo
#| fig-cap: "Esquema de censura y eventos de los ocho pacientes tratados con una corona de acero inoxidable. El eje horizontal indica el tiempo desde el inicio de la medición. El tramo punteado gris representa el tiempo en riesgo previo al inicio de la medición (entrada tardía o truncamiento por la izquierda) de los sujetos 1, 3, 4, 5, 7 y 8; los sujetos 2 y 6 entran justo al inicio. Las cruces (×) marcan el evento —observado en los sujetos 2, 6, 7, 1 y 4 en los tiempos t1 a t5— y los círculos abiertos (○) la censura por la derecha de los sujetos 3, 5 y 8 al final de la medición."
#| fig-width: 8
#| fig-height: 4.4
#| echo: false

sujeto  <- 1:8
tiempo  <- c(4, 1, 6, 5, 6, 2, 3, 6)   # tiempo del evento o de la censura
estatus <- c(1, 1, 0, 1, 0, 1, 1, 0)   # 1 = evento; 0 = censura (sujetos 3, 5 y 8)
pre     <- c(-2, NA, -3, -1.5, -2.5, NA, -3.5, -1)  # riesgo previo (truncam. izq.); NA = sin truncamiento
ypos    <- 9 - sujeto                   # Sujeto 1 en la parte superior
evento  <- estatus == 1

par(mar = c(4.5, 5, 3, 1))
plot(0, 0, type = "n", xlim = c(-4, 8), ylim = c(0.5, 8.5),
     xlab = "Tiempo desde el inicio de la medición (meses)", ylab = "",
     yaxt = "n", main = "Esquema de censura y eventos")
axis(2, at = ypos, labels = paste("Sujeto", sujeto), las = 1, cex.axis = 0.85)
axis(1, at = seq(-4, 8, 2))
axis(3, at = 1:5, labels = paste0("t", 1:5), col.axis = "#E74C3C",
     cex.axis = 0.85, tcl = -0.3, mgp = c(3, 0.3, 0))
abline(v = c(0, 6), lty = 2, col = "gray45")

for (i in sujeto) {
  yy <- ypos[i]
  # Tiempo en riesgo previo al inicio de la medición (truncamiento por la izquierda)
  if (!is.na(pre[i])) {
    lines(c(pre[i], 0), c(yy, yy), lwd = 2, lty = 3, col = "gray60")
    points(pre[i], yy, pch = "|", cex = 1, col = "gray45")
  }
  # Seguimiento observado
  lines(c(0, tiempo[i]), c(yy, yy), lwd = 2, col = "#2E86C1")
  # Desenlace
  if (evento[i]) {
    points(tiempo[i], yy, pch = 4, cex = 1.6, lwd = 2.5, col = "#E74C3C")  # evento
  } else {
    points(tiempo[i], yy, pch = 1, cex = 1.6, lwd = 2.5, col = "#27AE60")  # censura
  }
}
text(0, 8.5, "Inicio de la medición", pos = 4, offset = 0.2, cex = 0.7, col = "gray30")
text(6, 8.5, "Fin", pos = 4, offset = 0.2, cex = 0.7, col = "gray30")
legend(-4, 3.4,
       legend = c("Evento (×)", "Censura (○)", "Riesgo previo (truncam. izq.)"),
       pch = c(4, 1, NA), lty = c(NA, NA, 3),
       col = c("#E74C3C", "#27AE60", "gray60"),
       lwd = 2, bg = "white", box.col = "gray80", cex = 0.78)


## -----------------------------------------------------------------------------
#| label: tbl-datos-supervivencia
datos_km <- data.frame(
  sujeto  = 1:8,
  tiempo  = c(4, 1, 6, 5, 6, 2, 3, 6),
  estatus = c(1, 1, 0, 1, 0, 1, 1, 0)   # 1 = evento; 0 = censura (sujetos 3, 5 y 8)
)
knitr::kable(datos_km,
             col.names = c("Sujeto", "Tiempo", "Estatus"),
             caption = "Datos del esquema de supervivencia: tiempo hasta el evento o la censura y estatus (1 = evento; 0 = censura) de cada sujeto.")

