# Código y sets de datos — *Ciencia de Datos en Odontología*

Material complementario del libro **_Ciencia de Datos en Odontología. Un enfoque pragmático con R_** (Vicente Esparza Villalpando, Universidad Autónoma de Aguascalientes).

Esta carpeta reúne **todo el código** y **todos los sets de datos** empleados en el libro, para que el lector pueda reproducir cualquier análisis en su propio equipo.

## Contenido

- **`codigo/`** — un archivo `.R` por capítulo con todo el código de R utilizado. Cada bloque conserva su etiqueta original como separador (`## ---- cNN-nombre ----`), lo que permite localizarlo dentro del capítulo correspondiente.
- **`sets de datos/`** — los conjuntos de datos (archivos de texto delimitados por tabulaciones) que usan los ejemplos.
- **`codigo-y-datos.Rproj`** — proyecto de RStudio que fija automáticamente el directorio de trabajo en esta carpeta.

## Cómo usarlo

1. Abrir el proyecto **`codigo-y-datos.Rproj`** en RStudio. Al abrirlo, el directorio de trabajo queda fijado en esta carpeta.
2. Abrir el archivo `.R` del capítulo deseado (dentro de `codigo/`) y ejecutarlo. Las instrucciones `read.table("sets de datos/...")` encontrarán los datos automáticamente.
3. Algunos ejemplos emplean conjuntos de datos incluidos en paquetes de R (`birthwt` del paquete **MASS** y `anxiety` del paquete **irr**), que no requieren un archivo externo.

## Requisitos

- **R 4.5 o superior** y **RStudio**.
- Los paquetes de R empleados a lo largo del libro se enumeran en el **apéndice**; pueden instalarse siguiendo el capítulo de instalación.

## Nota

El código se distribuye tal como se ejecuta en el libro, con un estilo deliberadamente basado en **objetos y funciones** (R base). El contenido, los análisis y las decisiones metodológicas son responsabilidad del autor.
