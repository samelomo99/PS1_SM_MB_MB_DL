#-------------------------------------------------------------------#
## --------------- Problem Set 1: Predicting Income -------------- ##
## - Santiago Melo - Miguel Blanco - María Bernal - Diana Lopera - ##
#-------------------------------------------------------------------#

# Instalación / Librerías
install.packages("chromote")
install.packages("skimr")


library(rvest)
library(dplyr)
library(chromote)
library(readr)
library(skimr)
library(tidyverse)
library(stargazer)

# ------------------------------------------------------------- #
## ------------------------- PUNTO 2 ------------------------- ##
# ------------------------------------------------------------- #

# ------------------------------ #
# ------- Data Scrapping ------- #
# ------------------------------ #

# Data Scrapping
## Descarga de datos 

# Crear la carpeta "data" si no existe, para guardar los archivos CSV
if (!dir.exists("data")) {
  dir.create("data")
}

# Lista para almacenar cada data.frame extraído
lista_tablas <- list()

# Tiempo máximo de espera para que se cargue la tabla (en segundos)
max_wait <- 90

# Iterar sobre las 10 páginas
for(i in 1:10) {
  # Construir la URL de la página
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page", i, ".html")
  cat("Procesando URL:", url, "\n")
  
  # Inicia una sesión de Chromote para la página
  b <- ChromoteSession$new()
  b$Page$navigate(url)
  
  # Espera activa hasta que la tabla esté presente
  start_time <- Sys.time()
  table_found <- FALSE
  while(as.numeric(difftime(Sys.time(), start_time, units = "secs")) < max_wait && !table_found){
    # Verifica si existe un elemento <table> en el DOM
    result <- b$Runtime$evaluate("document.querySelector('table') !== null;")$result$value
    if (isTRUE(result)) {
      table_found <- TRUE
      cat("Se encontró la tabla en la página.\n")
    } else {
      Sys.sleep(2)  # esperar 2 segundos antes de volver a chequear
    }
  }
  
  # Si no se encuentra la tabla en el tiempo máximo, se avisa y se continúa con la siguiente página
  if (!table_found) {
    cat("No se encontró la tabla en", url, "dentro de", max_wait, "segundos. Continuando...\n")
    b$close()
    next
  }
  
  # Extraer el HTML de la tabla usando JavaScript
  js_code <- "document.querySelector('table').outerHTML;"
  table_html <- b$Runtime$evaluate(js_code)$result$value
  
  # Cerrar la sesión de Chromote para esta página
  b$close()
  
  # Convertir el HTML extraído en un objeto rvest
  page_parsed <- read_html(table_html)
  
  # Extraer la tabla y convertirla en data.frame
  tabla_df <- page_parsed %>% html_table(fill = TRUE)
  
  # Verificar que se extrajo la tabla
  if (is.null(tabla_df)) {
    cat("No se pudo extraer la tabla de", url, "\n")
    next
  }
  
  # Mostrar por consola las primeras filas de la tabla extraída
  cat("Tabla extraída de", url, ":\n")
  print(head(tabla_df))
  
  # Guardar la tabla en un archivo CSV individual (sin row.names)
  csv_filename <- sprintf("data/page%d.csv", i)
  write.csv(tabla_df, csv_filename, row.names = FALSE)
  cat("Guardado CSV:", csv_filename, "\n\n")
  
  # Almacenar la tabla en la lista
  lista_tablas[[i]] <- tabla_df
}

# Unir todas las tablas en un único data.frame
base_completa <- bind_rows(lista_tablas)
cat("Número total de observaciones en la base completa:", nrow(base_completa), "\n")

# Guardar la base completa en un archivo CSV
write.csv(base_completa, "data/GEIH_2018_sample_all.csv", row.names = FALSE)
cat("Base completa guardada en data/GEIH_2018_sample_all.csv\n")


# ------------------------------ #
# ----- Manejo de los datos -----#
# ------------------------------ #

# Usamos la base de datos scrapeada y subida al repositorio en GitHub
datos <- read_csv("https://raw.githubusercontent.com/samelomo99/PS1_SM_MB_MB_DL/main/stores/GEIH_2018_sample_all.csv")

# Filtramos por mayores (o iguales) a 18 y por personas ocupadas. 
datos <- datos %>% filter(age >= 18, ocu == 1)
summary(datos$age) # Comprobamos que el mínimo es 18 años.

# Revisión rápida de los datos
skim(datos)


# ------------------------------------------------------------- #
## ------------------------- PUNTO 3 ------------------------- ##
# ------------------------------------------------------------- #


## Creamos las variables

log_w <- log(datos$y_total_m)

## La regresión 

reg_p3 <- lm(log_w ~ age + I(age^2), data = datos)

#Generacion de la tabla 
stargazer(reg_p3, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3, type = "latex", title = "Logaritmo del salario en funcion de la edad")


# Sacando los coeficientes

reg_p3$coefficients


## Revisando el ajuste intramodelo

y_hat <- predict(reg_p3, newdata = datos)

MSE_p3 <- mean((log_w - y_hat)^2)

MSE_p3


