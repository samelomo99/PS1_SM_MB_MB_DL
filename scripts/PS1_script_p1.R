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
library(ggplot2)
library(boot)

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

#GEIH_2018_sample_all <- datos
datos <- read_csv("https://raw.githubusercontent.com/samelomo99/PS1_SM_MB_MB_DL/main/stores/GEIH_2018_sample_all.csv")

# Filtramos por mayores (o iguales) a 18 y por personas ocupadas. 
datos <- datos %>% filter(age >= 18, ocu == 1)
summary(datos$age) # Comprobamos que el mínimo es 18 años.


# -- NA / Missing Values - 2 aproximaciones -- #
is.na(datos$y_total_m)

# 1. Eliminamos NA
datos1 <- datos %>% filter(!is.na(y_total_m))

# 2. Reemplazamos NA por el valor medio
m_y_total_m <- mean(datos$y_total_m, na.rm = TRUE)
datos2 <- datos %>%  mutate(y_total_m = replace_na(y_total_m, m_y_total_m))

# Revisión rápida de los datos
skim(datos1)
skim(datos2)

## Estadísticas descriptivas


# ------------------------------------------------------------- #
## ------------------------- PUNTO 3 ------------------------- ##
# ------------------------------------------------------------- #


## Creamos las variables

log_w1 <- log(datos1$y_total_m)
log_w2 <- log(datos2$y_total_m)

## La regresión 

reg_p3_1 <- lm(log_w1 ~ age + I(age^2), data = datos1)
reg_p3_2 <- lm(log_w2 ~ age + I(age^2), data = datos2)

#Generacion de la tabla 
stargazer(reg_p3_1, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3_1, type = "latex", title = "Logaritmo del salario en funcion de la edad")

stargazer(reg_p3_2, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3_2, type = "latex", title = "Logaritmo del salario en funcion de la edad")


# Sacando los coeficientes

reg_p3_1$coefficients
reg_p3_2$coefficients


## Revisando el ajuste intramodelo

log_w_hat_1 <- predict(reg_p3_1, newdata = datos1)
log_w_hat_2  <- predict(reg_p3_2, newdata = datos2)

MSE_1 <- mean((log_w1 - log_w_hat_1)^2)
MSE_2 <- mean((log_w2 - log_w_hat_2)^2)

MSE_1
MSE_2

## En este caso el MSE 2 se ajusta mejor y nos va mejor en el modelo de medias. 
## Nos casamos con el modelo de medias (datos2)


## Ahora desarrollamos una grafica para ver cual es el valor del punto pico del ingreso estimado en terminos de la edad.


ggplot(datos2, aes(x = age, y = log_w_hat_2)) +
  geom_point(color = "blue", alpha = 0.6) +  # Puntos en azul con transparencia
 # geom_smooth(method = "lm", color = "red", se = TRUE) +  # Línea de tendencia con intervalo de confianza
  labs(title = "Relación entre Edad y el Logarimo de los Ingresos",
       x = "Edad",
       y = "Log(Ingresos)") +
  theme_minimal()

## Finalmente calculamos con bootstrap el valor maximo de los ingresos.
  
  ##Primero creamos la funcion 

peak_age_f1<-function(datos2,index){
  
  reg_p3_2 <- lm(log_w2 ~ age + I(age^2), data = datos2, subset = index)
  
  
  b2 <- coef(reg_p3_2)[2]
  b3 <- coef(reg_p3_2)[3]
  
age_max <- -b2/(2*b3)  #Esto sale de derivar la ecuacion e igualar a 0 en base a los coeficientes estimados

return(age_max)
}

peak_age_f1(datos2,1:nrow(datos2))  #Probando la funcion 

  ##Finalmente hacemos la simulación
set.seed(1234)
boot_p3 <- boot(data = datos2, peak_age_f1, R = 1000)
boot_p3

boot.ci(boot_p3, type = c("perc", "bca")) #Esta funcion me saca los intervalos de confianza al 95% bajo dos metodologias

