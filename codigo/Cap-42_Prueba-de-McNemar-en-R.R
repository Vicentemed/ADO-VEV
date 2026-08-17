## -----------------------------------------------------------------------------
#| label: carga-musict
library(gmodels)

dat <- read.table("sets de datos/datMusicT.txt", header = TRUE, sep = "\t",
                  na.strings = "NA", dec = ".", strip.white = TRUE)
dat$Behavior <- as.factor(dat$Behavior)
dat$Group    <- factor(dat$Group, levels = c("Music","Control"))
str(dat)


## -----------------------------------------------------------------------------
#| label: reshape-pareado
musica  <- dat[dat$Group == "Music",   c("Px", "Behavior")]
control <- dat[dat$Group == "Control", c("Px", "Behavior")]
names(musica)[2]  <- "Music"
names(control)[2] <- "Control"

ancho <- merge(musica, control, by = "Px")   # un niño por fila
head(ancho)


## -----------------------------------------------------------------------------
#| label: tabla-pareada
ancho$Music   <- factor(ancho$Music,   levels = c(1, 2), labels = c("Colaborador", "No colaborador"))
ancho$Control <- factor(ancho$Control, levels = c(1, 2), labels = c("Colaborador", "No colaborador"))

TablaPar <- table(Musicoterapia = ancho$Music, Control = ancho$Control)

# Para mostrar la tabla con etiquetas explícitas de fila (Musicoterapia) y columna (Control):
tab_df <- as.data.frame.matrix(addmargins(TablaPar))
tab_df <- cbind(Musicoterapia = rownames(tab_df), tab_df)
tab_df$Musicoterapia[tab_df$Musicoterapia == "Sum"] <- "Total"
rownames(tab_df) <- NULL
knitr::kable(tab_df,
             col.names = c("Musicoterapia", "Control: Colaborador", "Control: No colaborador", "Total"),
             align     = c("l", "c", "c", "c"),
             caption   = "Tabla de contingencia apareada: comportamiento de cada niño bajo musicoterapia (filas) frente a control (columnas).")


## -----------------------------------------------------------------------------
#| label: mcnemar-test
mc <- mcnemar.test(TablaPar, correct = FALSE)
mc


## -----------------------------------------------------------------------------
#| label: or-mcnemar
solo_musica  <- TablaPar["Colaborador", "No colaborador"]   # b
solo_control <- TablaPar["No colaborador", "Colaborador"]   # c
cat("Pares discordantes -> b =", solo_musica, "| c =", solo_control, "\n")
cat("OR de McNemar (b/c) =", solo_musica / solo_control, "\n")


## -----------------------------------------------------------------------------
#| label: or-mcnemar-haldane
(solo_musica + 0.5) / (solo_control + 0.5)


## -----------------------------------------------------------------------------
#| label: fig-mcnemar-barras
#| fig-cap: "Distribución del comportamiento colaborador en niños con musicoterapia y con tratamiento control. La musicoterapia está asociada con una mayor proporción de comportamiento colaborador en comparación con el control."
#| fig-width: 6
#| fig-height: 4
#| message: false

library(ggplot2)
dat$Behavior_label <- ifelse(dat$Behavior == 1, "Colaborador","No colaborador")

ggplot(dat, aes(Group, fill = Behavior_label)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#2E86C1","#E74C3C")) +
  labs(x = "Grupo", y = "Proporción", fill = "Comportamiento") +
  theme_bw(base_size = 12)

