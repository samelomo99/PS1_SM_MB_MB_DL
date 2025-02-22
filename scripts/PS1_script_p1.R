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

# install pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)
# require/install packages on this session

p_load(rvest,
       dplyr,
       chromote,
       readr,
       skimr,
       tidyverse,
       stargazer,
       ggplot2,
       boot)
       

# ------------------------------------------------------------- #
## ------------------------- PUNTO 2 ------------------------- ##
# ------------------------------------------------------------- #

# ------------------------------ #
# ------- Data Scrapping ------- #
# ------------------------------ #

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


# ------------------------------ #
# ----- Manejo de los datos -----#
# ------------------------------ #

# Usamos la base de datos scrapeada y subida al repositorio en GitHub

#GEI

H_2018_sample_all <- datos
datos <- read_csv("https://raw.githubusercontent.com/samelomo99/PS1_SM_MB_MB_DL/main/stores/GEIH_2018_sample_all.csv")

# Filtramos por mayores (o iguales) a 18 y por personas ocupadas. 
datos <- datos %>% filter(age >= 18, ocu == 1)
summary(datos$age) # Comprobamos que el mínimo es 18 años.
skim(datos)

# -- NA / Missing Values - 2 aproximaciones -- #
is.na(datos$y_total_m)

# 1. Eliminamos NA
datos1 <- datos %>% filter(!is.na(y_total_m))
datos3 <- datos %>% filter(!is.na(y_ingLab_m_ha))
datos5 <- datos %>% filter(!is.na(y_total_m_ha))

# 2. Reemplazamos NA por el valor medio
m_y_total_m <- mean(datos$y_total_m, na.rm = TRUE)
m_y_ingLab_m_ha <- mean(datos$y_ingLab_m_ha, na.rm = TRUE)
m_y_total_m_ha <- mean(datos$y_total_m_ha, na.rm = TRUE)
datos2 <- datos %>%  mutate(y_total_m = replace_na(y_total_m, m_y_total_m), y_total_m_ha = replace_na(y_total_m_ha, m_y_total_m_ha))
datos4 <- datos %>%  mutate(y_ingLab_m_ha = replace_na(y_ingLab_m_ha, m_y_ingLab_m_ha)) 

# Revisión rápida de los datos
skim(datos1)
skim(datos2)
skim(datos3)
skim(datos4)
skim(datos5)

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

CF <- boot.ci(boot_p3, type = "perc")$percent[4:5] #Esto me saca el percentil
edad_max_boot_p3 <- boot_p3$t #Aqui sacamos los valores estimados de cada una de las iteraciones del bootstrap

ggplot(data.frame(edad_max_boot_p3), aes(x = edad_max_boot_p3)) +
  geom_histogram(aes(y =after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +  # Agregar densidad
  geom_vline(aes(xintercept = mean(edad_max_boot_p3)), color = "red", linetype = "dashed", linewidth = 1) +  # Media
  geom_vline(aes(xintercept = CF[1]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite inferior IC
  geom_vline(aes(xintercept = CF[2]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite superior IC
  labs(title = "Distribución Bootstrap de la Edad con Ingresos Máximos",
       x = "Edad máxima estimada",
       y = "Densidad") +
  theme_minimal()

#--------------------------------------------------------------------------------------------
##Ahora con los ingresos totales por cada hora trabajada
#--------------------------------------------------------------------------------------------

## Creamos las variables

log_w1_ha <- log(datos1$y_total_m_ha)
log_w2_ha <- log(datos2$y_total_m_ha)

## La regresión 

reg_p3_1_ha <- lm(log_w1_ha ~ age + I(age^2), data = datos1)
reg_p3_2_ha <- lm(log_w2_ha ~ age + I(age^2), data = datos2)

#Generacion de la tabla 
stargazer(reg_p3_1_ha, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3_1_ha, type = "latex", title = "Logaritmo del salario en funcion de la edad")

stargazer(reg_p3_2_ha, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3_2_ha, type = "latex", title = "Logaritmo del salario en funcion de la edad")


# Sacando los coeficientes

reg_p3_1_ha$coefficients
reg_p3_2_ha$coefficients


## Revisando el ajuste intramodelo

log_w_hat_1_ha <- predict(reg_p3_1_ha, newdata = datos1)
log_w_hat_2_ha  <- predict(reg_p3_2_ha, newdata = datos2)

MSE_1_ha <- mean((log_w1_ha - log_w_hat_1_ha)^2)
MSE_2_ha <- mean((log_w2_ha - log_w_hat_2_ha)^2)

MSE_1_ha
MSE_2_ha

## En este caso el MSE 2_ha se ajusta mejor y nos va mejor en el modelo de medias. 
## Nos casamos con el modelo de medias (datos2)


## Ahora desarrollamos una grafica para ver cual es el valor del punto pico del ingreso estimado en terminos de la edad.


ggplot(datos2, aes(x = age, y = log_w_hat_2_ha)) +
  geom_point(color = "blue", alpha = 0.6) +  # Puntos en azul con transparencia
  # geom_smooth(method = "lm", color = "red", se = TRUE) +  # Línea de tendencia con intervalo de confianza
  labs(title = "Relación entre Edad y el Logarimo de los Ingresos",
       x = "Edad",
       y = "Log(Ingresos)") +
  theme_minimal()

## Finalmente calculamos con bootstrap el valor maximo de los ingresos.

##Primero creamos la funcion 

peak_age_f1_ha<-function(datos2,index){
  
  reg_p3_2_ha <- lm(log_w2_ha ~ age + I(age^2), data = datos2, subset = index)
  
  
  b2_ha <- coef(reg_p3_2_ha)[2]
  b3_ha <- coef(reg_p3_2_ha)[3]
  
  age_max_ha <- -b2_ha/(2*b3_ha)  #Esto sale de derivar la ecuacion e igualar a 0 en base a los coeficientes estimados
  
  return(age_max_ha)
}

peak_age_f1_ha(datos2,1:nrow(datos2))  #Probando la funcion 

##Finalmente hacemos la simulación
set.seed(1234)
boot_p3_ha <- boot(data = datos2, peak_age_f1_ha, R = 1000)
boot_p3_ha

boot.ci(boot_p3_ha, type = c("perc", "bca")) #Esta funcion me saca los intervalos de confianza al 95% bajo dos metodologias

CF_ha <- boot.ci(boot_p3_ha, type = "perc")$percent[4:5] #Esto me saca el percentil
edad_max_boot_p3_ha <- boot_p3_ha$t #Aqui sacamos los valores estimados de cada una de las iteraciones del bootstrap

ggplot(data.frame(edad_max_boot_p3_ha), aes(x = edad_max_boot_p3_ha)) +
  geom_histogram(aes(y =after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +  # Agregar densidad
  geom_vline(aes(xintercept = mean(edad_max_boot_p3_ha)), color = "red", linetype = "dashed", linewidth = 1) +  # Media
  geom_vline(aes(xintercept = CF_ha[1]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite inferior IC
  geom_vline(aes(xintercept = CF_ha[2]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite superior IC
  labs(title = "Distribución Bootstrap de la Edad con Ingresos Máximos",
       x = "Edad máxima estimada",
       y = "Densidad") +
  theme_minimal()

#-----------------------------------------------
## Ahora la version con datos de horas laboradas
#-----------------------------------------------

## Creamos las variables

log_s1 <- log(datos3$y_ingLab_m_ha)
log_s2 <- log(datos4$y_ingLab_m_ha)

## La regresión 

reg_p3_1s <- lm(log_s1 ~ age + I(age^2), data = datos3)
reg_p3_2s <- lm(log_s2 ~ age + I(age^2), data = datos4)

#Generacion de la tabla 
stargazer(reg_p3_1s, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3_1s, type = "latex", title = "Logaritmo del salario en funcion de la edad")

stargazer(reg_p3_2s, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3_2s, type = "latex", title = "Logaritmo del salario en funcion de la edad")


# Sacando los coeficientes

reg_p3_1s$coefficients
reg_p3_2s$coefficients


## Revisando el ajuste intramodelo

log_s_hat_1 <- predict(reg_p3_1s, newdata = datos3)
log_s_hat_2  <- predict(reg_p3_2s, newdata = datos4)

MSE_1s <- mean((log_s1 - log_s_hat_1)^2)
MSE_2s <- mean((log_s2 - log_s_hat_2)^2)

MSE_1s
MSE_2s

## En este caso el MSE 2s se ajusta mejor y nos va mejor en el modelo de medias. 
## Nos casamos con el modelo de medias (datos4)


## Ahora desarrollamos una grafica para ver cual es el valor del punto pico del ingreso estimado en terminos de la edad.


ggplot(datos4, aes(x = age, y = log_s_hat_2)) +
  geom_point(color = "blue", alpha = 0.6) +  # Puntos en azul con transparencia
  # geom_smooth(method = "lm", color = "red", se = TRUE) +  # Línea de tendencia con intervalo de confianza
  labs(title = "Relación entre Edad y el Logarimo de los Ingresos",
       x = "Edad",
       y = "Log(Ingresos)") +
  theme_minimal()

ggplot(datos4, aes(x = age, y = log_s_hat_2)) +
  geom_point(color = "blue", alpha = 0.6) +  # Puntos en azul con transparencia
  geom_vline(xintercept = 50, color = "red", linetype = "dashed", size = 1) +  # Línea vertical roja en x = 50
  # geom_smooth(method = "lm", color = "red", se = TRUE) +  # Línea de tendencia opcional
  labs(title = "Relación entre Edad y el Logaritmo de los Ingresos",
       x = "Edad",
       y = "Log(Ingresos)") +
  theme_minimal()

## Finalmente calculamos con bootstrap el valor maximo de los ingresos.

##Primero creamos la funcion 

peak_age_f2<-function(datos2,index){
  
  reg_p3_2s <- lm(log_s2 ~ age + I(age^2), data = datos2, subset = index)
  
  
  b2s <- coef(reg_p3_2s)[2]
  b3s <- coef(reg_p3_2s)[3]
  
  age_max_s <- -b2s/(2*b3s)  #Esto sale de derivar la ecuacion e igualar a 0 en base a los coeficientes estimados
  
  return(age_max_s)
}

peak_age_f2(datos2,1:nrow(datos4))  #Probando la funcion 

##Finalmente hacemos la simulación
set.seed(1234)
boot_p3_s <- boot(data = datos4, peak_age_f2, R = 1000)
boot_p3_s

boot.ci(boot_p3_s, type = c("perc", "bca")) #Esta funcion me saca los intervalos de confianza al 95% bajo dos metodologias

CF_S <- boot.ci(boot_p3_s, type = "perc")$percent[4:5] #Esto me saca el percentil
edad_max_boot_p3_s <- boot_p3_s$t #Aqui sacamos los valores estimados de cada una de las iteraciones del bootstrap

ggplot(data.frame(edad_max_boot_p3_s), aes(x = edad_max_boot_p3_s)) +
  geom_histogram(aes(y =after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +  # Agregar densidad
  geom_vline(aes(xintercept = mean(edad_max_boot_p3_s)), color = "red", linetype = "dashed", linewidth = 1) +  # Media
  geom_vline(aes(xintercept = CF_S[1]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite inferior IC
  geom_vline(aes(xintercept = CF_S[2]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite superior IC
  labs(title = "Distribución Bootstrap de la Edad con Ingresos Máximos",
       x = "Edad máxima estimada",
       y = "Densidad") +
  theme_minimal()


# ------------------------------------------------------------- #
## ------------------------- PUNTO 4 ------------------------- ##
# ------------------------------------------------------------- #

## Hacemos la regresion

datos4 <- datos4 %>%
  mutate(female = ifelse(sex == 0, 1, 0))

reg_p4 <- lm(log_s2~female, data=datos4)

stargazer(reg_p4, type = "text", title = "Logaritmo del salario en funcion del genero")
 
## Teorema FWL

# x1 es la variable female
# x2 son los controles que corresponden a: edad y estrato

reg_p4_controles <- lm(log_s2 ~ female+age+estrato1, data = datos4)
stargazer(reg_p4_controles, type = "text", title = "Logaritmo del salario en funcion del genero")


regaux_p4_x1 <- lm(female~age+estrato1, data=datos4) # Regresion de x2 sobre x1

regaux_p4_y <- lm(log_s2~age+estrato1, data=datos4) # Regresion de x2 sobre y

# Despues de purgar x2 de x1 y de y, se corre la regresion

x1_resid <- regaux_p4_x1$residuals
x1_resid

y_resid <- regaux_p4_y$residuals
y_resid

reg_p4_fwl <- lm(y_resid~x1_resid, data=datos4)
stargazer(reg_p4_controles,reg_p4_fwl, type = "text", title = "Logaritmo del salario en funcion del genero")



# Teorema FWL con bootstrap

fwl_function<-function(datos4,index){
  
  regaux_p4_x1 <- lm(female~age+estrato1, data=datos4, subset=index) # Regresion de x2 sobre x1
  regaux_p4_y <- lm(log_s2~age+estrato1, data=datos4, subset=index)
  
  
  x1_resid <- regaux_p4_x1$residuals
  
  y_resid <- regaux_p4_y$residuals

  
  reg_p4_fwl <- lm(y_resid~x1_resid, data=datos4)

  beta_female <- reg_p4_fwl$coefficients[2]  

  return(beta_female)
}

fwl_function(datos4,1:nrow(datos4))  #Probando la funcion 

##Finalmente hacemos la simulación
set.seed(1234)
boot_p4_ha <- boot(data = datos4, fwl_function, R = 1000)
boot_p4_ha

boot.ci(boot_p4_ha, type = "perc") #Esta funcion me saca los intervalos de confianza al 95% bajo dos metodologias

## ----------

peak_age_female <-function(datos4,index){
  
  datos_muestra <- datos4[index, ]  # Tomar solo las filas seleccionadas por bootstrap
  datos_female <- datos_muestra[datos_muestra$female == 1, ]  # Filtrar solo mujeres
  
  log_salario <- log(datos_female$y_ingLab_m_ha)
  
  reg_p4_peak_female <- lm(log_salario ~ age + I(age^2), data = datos_female)
  
  
  b2_f <- coef(reg_p4_peak_female)[2]
  b3_f <- coef(reg_p4_peak_female)[3]
  
  age_max_f <- -b2_f/(2*b3_f)  #Esto sale de derivar la ecuacion e igualar a 0 en base a los coeficientes estimados
  
  return(age_max_f)
}

peak_age_female(datos4,1:nrow(datos4))  #Probando la funcion 

##Finalmente hacemos la simulación
set.seed(1234)
boot_p4_f <- boot(data = datos4, peak_age_female, R = 1000)
boot_p4_f

boot.ci(boot_p4_f, type = "perc") #Esta funcion me saca los intervalos de confianza al 95% bajo dos metodologias

CF_female <- boot.ci(boot_p4_f, type = "perc")$percent[4:5] #Esto me saca el percentil
edad_max_female <- boot_p4_f$t #Aqui sacamos los valores estimados de cada una de las iteraciones del bootstrap

ggplot(data.frame(edad_max_female), aes(x = edad_max_female)) +
  geom_histogram(aes(y =after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +  # Agregar densidad
  geom_vline(aes(xintercept = mean(edad_max_female)), color = "red", linetype = "dashed", linewidth = 1) +  # Media
  geom_vline(aes(xintercept = CF_female[1]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite inferior IC
  geom_vline(aes(xintercept = CF_female[2]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite superior IC
  labs(title = "Distribución Bootstrap de la Edad con Ingresos Máximos",
       x = "Edad máxima estimada",
       y = "Densidad") +
  theme_minimal()


## ----

peak_age_male <-function(datos4,index){
  
  datos_muestra <- datos4[index, ]  # Tomar solo las filas seleccionadas por bootstrap
  datos_male <- datos_muestra[datos_muestra$female == 0, ]  # Filtrar solo mujeres
  
  log_salario <- log(datos_male$y_ingLab_m_ha)
  
  reg_p4_peak_male <- lm(log_salario ~ age + I(age^2), data = datos_male)
  
  
  b2_m <- coef(reg_p4_peak_male)[2]
  b3_m <- coef(reg_p4_peak_male)[3]
  
  age_max_m <- -b2_m/(2*b3_m)  #Esto sale de derivar la ecuacion e igualar a 0 en base a los coeficientes estimados
  
  return(age_max_m)
}

peak_age_male(datos4,1:nrow(datos4))  #Probando la funcion 

##Finalmente hacemos la simulación
set.seed(1234)
boot_p4_m <- boot(data = datos4, peak_age_male, R = 1000)
boot_p4_m

boot.ci(boot_p4_m, type = "perc") #Esta funcion me saca los intervalos de confianza al 95% bajo dos metodologias

CF_male <- boot.ci(boot_p4_m, type = "perc")$percent[4:5] #Esto me saca el percentil
edad_max_male <- boot_p4_m$t #Aqui sacamos los valores estimados de cada una de las iteraciones del bootstrap

ggplot(data.frame(edad_max_male), aes(x = edad_max_male)) +
  geom_histogram(aes(y =after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +  # Agregar densidad
  geom_vline(aes(xintercept = mean(edad_max_male)), color = "red", linetype = "dashed", linewidth = 1) +  # Media
  geom_vline(aes(xintercept = CF_male[1]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite inferior IC
  geom_vline(aes(xintercept = CF_male[2]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite superior IC
  labs(title = "Distribución Bootstrap de la Edad con Ingresos Máximos",
       x = "Edad máxima estimada",
       y = "Densidad") +
  theme_minimal()
