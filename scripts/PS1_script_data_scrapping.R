#-------------------------------------------------------------------#
## --------------- Problem Set 1: Predicting Income -------------- ##
## - Santiago Melo - Miguel Blanco - María Bernal - Diana Lopera - ##
#-------------------------------------------------------------------#

# Instalación / Librerías

# Instalar y cargar pacman si aún no lo tienes
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Instalar y cargar todos los paquetes necesarios en una sola llamada
pacman::p_load(
  readr,        # Importar datos (ya incluido en tidyverse)
  labelled,     # Manejo de etiquetas
  naniar,       # Visualizar datos faltantes
  DataExplorer, # Gráficos de missing values
  psych,        # Estadísticas descriptivas
  rvest,        # Web scraping
  rio,          # Importar/exportar datos
  tidyverse,    # Conjunto de paquetes para tidy data (incluye dplyr, ggplot2, etc.)
  skimr,        # Resumen de datos
  visdat,       # Visualizar datos faltantes
  corrplot,     # Gráficos de correlación
  gridExtra,    # Organización de gráficos
  MASS,         # Funciones estadísticas diversas
  stargazer,    # Tablas para salida a TEX
  chromote,     # Automatización de navegador (útil para scraping avanzado)
  ggplot2,      # Gráficos (ya incluido en tidyverse)
  boot,         # Funciones de bootstrap
  patchwork,    # Combinación de gráficos
  dplyr
)

# ------------------------------------------------------------- #
## ---------------------- DATA SCRAPPING --------------------- ##
# ------------------------------------------------------------- #

# Data Scrapping
## Descarga de datos 

#Nota: es importante que para que la carpeta stores se cree en el lugar correcto
##estemos ubicados en ruta donde queremos que esta sea creada
## Descarga de datos 

# Crear la carpeta "stores" si no existe (dentro del repositorio)
if (!dir.exists("stores")) {
  dir.create("stores")
  cat("Carpeta 'stores' creada.\n")
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