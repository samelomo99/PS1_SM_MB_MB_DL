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
  patchwork    # Combinación de gráficos
 )


# ------------------------------------------------------------- #
## ------------------------- PUNTO 2 ------------------------- ##
# ------------------------------------------------------------- #

# --------------------------------------------------------------- #
#   ----- MANEJO DE DATOS Y ELECCIóN DE VARIABLES DE INTERÉS -----#
# --------------------------------------------------------------- #

# Usamos la base de datos scrapeada y subida al repositorio en GitHub

#GEIH

datos <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS1_SM_MB_MB_DL/main/stores/GEIH_2018_sample_all.csv"
  )

## filtramos solo las variables de interes y construimos variables necesarias
      ### El problema establece que el modelo debe predecir el salario por hora de 
        # los individuos, por tanto, incluiremos en la sub-base variables de salario por hora
        # en diferentes especificaciones para analisis exploratorio.
        # La elección de las potenciales variables predictores en un modelo de predicción de 
        # salarios por hora se fundamenta en la teoría del capital humano 
        # (Mincer, 1974) y en estudios previos que han analizado los determinantes 
        # del ingreso laboral.
            
            # 1. Variables clave del capital humano: El modelo minceriano sugiere que 
            #    la educación y la experiencia son los principales determinantes del salario.
            # 2. Características sociodemográficas: Las diferencias salariales pueden 
            #    explicarse parcialmente por atributos personales y sociales
            # 3. Variables relacionadas con el empleo y la firma: Las características 
            #    de la empresa y del empleo afectan significativamente la estructura 
            #    salarial.


# 1. Seleccionar las variables de interés, incluyendo las variables llave:
#    - directorio: Llave de vivienda
#    - orden: Llave de persona
#    - secuencia_p: Llave de hogar

# Nota: la variable   clase (1 urabano 0 rural), no se incluye porque todo es urbano
#la variable dominio y departamento, no se incluye porque todo es Bogota

datos <- datos %>% 
  select(
    directorio,    # Llave de vivienda
    orden,         # Llave de persona
    secuencia_p,   # Llave de hogar
    y_total_m,
    y_ingLab_m_ha,
    y_total_m_ha,
    y_salary_m_hu,
    sex,
    age,
    maxEducLevel,
    p6050,
    estrato1,
    oficio,
    relab,
    formal,
    sizeFirm,
    cotPension,
    p6426,
    ocu
  )


# Visualizar la estructura de la nueva base de datos
str(datos)

#2. Creamos algunas variables de interes antes filtrar por los mayores de 18    

#2.1. NUMERO DE MENORES EN EL HOGAR
##Crear variable indicador de menores de 18 años
datos <- datos %>%
  mutate(flag = ifelse(age <= 6, 1, 0))

#Calcular el número total de menores por hogar (identificado por 'directorio' y 'secuencia_p')
datos <- datos %>%
  group_by(directorio, secuencia_p) %>%
  mutate(nmenores = sum(flag)) %>%
  select(-flag) %>% 
  ungroup()

# Verificar el resultado mostrando las últimas filas
datos %>% 
  dplyr::select(directorio, secuencia_p, age, nmenores) %>% 
  head(10) %>%
  View()

#2.2 JEFE DE HOGAR (1= jefe 0=otro caso)
datos <- datos %>%
  mutate(H_Head = ifelse( p6050== 1, 1, 0))

table(datos$H_Head)

#2.3 JEFE DE HOGAR MUJER

#renombrando variable sex=gender (nota: gender= 0=mujer 1=hombre)
datos <- as_tibble(datos) %>% rename(gender=sex)

# creando variable jefe hogar mujer 
datos <- datos %>%
  mutate(Head_Female = H_Head*(1-gender))

table(datos$Head_Female)
table(datos$gender)  


# Filtramos por mayores (o iguales) a 18 y por personas ocupadas. 
datos <- datos %>% filter(age >= 18, ocu == 1)
summary(datos$age) # Comprobamos que el mínimo es 18 años.


##########################
#INSPECCIÓN DE LOS DATOS
##########################

datos <- as_tibble(datos) ## de dataframe to tibble

## print data
head(datos)
tail(datos)

## Inspección básica de la estructura y resumen de datos
skim(datos)

###Se revisa la estructura del dataframe (con glimpse y resumen estadístico básico (summary).
glimpse(datos)
summary(datos)

##Otra opcion de exploracion es usar el paquete summarytool que genera muy completo que incluye las frecuencias y el porcentaje de NA para cada variable
install.packages("summarytools")  # Si no lo tienes instalado
library(summarytools)
resumen <- dfSummary(datos, style = "grid", plain.ascii = FALSE) # resumen completo de la base
print(resumen, method = "browser") # Imprime el resumen en el navegador


#Visualización de datos faltantes

# a) Con naniar: gráfico de missing values
vis_miss(datos)

# b) Con DataExplorer: gráfico de missing values
plot_missing(datos)


##################################

# Filtramos por mayores (o iguales) a 18 y por personas ocupadas. 
datos <- datos %>% filter(age >= 18, ocu == 1)
summary(datos$age) # Comprobamos que el mínimo es 18 años.
skim(datos)

# -- NA / Missing Values - 2 aproximaciones -- #
is.na(datos$y_ingLab_m_ha)

# 1. Eliminamos NA
datos1 <- datos %>% filter(!is.na(y_ingLab_m_ha))


# 2. Reemplazamos NA por el valor medio

m_y_ingLab_m_ha <- mean(datos$y_ingLab_m_ha, na.rm = TRUE)
datos2 <- datos %>%  mutate(y_ingLab_m_ha = replace_na(y_ingLab_m_ha, m_y_ingLab_m_ha)) 

# Revisión rápida de los datos
skim(datos1)
skim(datos2)

## Estadísticas descriptivas


# ------------------------------------------------------------- #
## ------------------------- PUNTO 3 ------------------------- ##
# ------------------------------------------------------------- #

## Creamos las variables

log_s1 <- log(datos1$y_ingLab_m_ha)
log_s2 <- log(datos2$y_ingLab_m_ha)

## La regresión 

reg_p3_1s <- lm(log_s1 ~ age + I(age^2), data = datos1)
reg_p3_2s <- lm(log_s2 ~ age + I(age^2), data = datos2)

#Generacion de la tabla 
stargazer(reg_p3_1s, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3_1s, type = "latex", title = "Logaritmo del salario en funcion de la edad")

stargazer(reg_p3_2s, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3_2s, type = "latex", title = "Logaritmo del salario en funcion de la edad")


# Sacando los coeficientes

reg_p3_1s$coefficients
reg_p3_2s$coefficients


## Revisando el ajuste intramodelo

log_s_hat_1 <- predict(reg_p3_1s, newdata = datos1)
log_s_hat_2  <- predict(reg_p3_2s, newdata = datos2)

MSE_1s <- mean((log_s1 - log_s_hat_1)^2)
MSE_2s <- mean((log_s2 - log_s_hat_2)^2)

MSE_1s
MSE_2s

## En este caso el MSE 2s se ajusta mejor y nos va mejor en el modelo de medias. 
## Nos casamos con el modelo de medias (datos4)


## Ahora desarrollamos una grafica para ver cual es el valor del punto pico del ingreso estimado en terminos de la edad.


ggplot(datos2, aes(x = age, y = log_s_hat_2)) +
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

peak_age_f2(datos2,1:nrow(datos2))  #Probando la funcion 

##Finalmente hacemos la simulación
set.seed(1234)
boot_p3_s <- boot(data = datos2, peak_age_f2, R = 1000)
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

datos2 <- datos2 %>%
  mutate(female = ifelse(sex == 0, 1, 0))

reg_p4 <- lm(log_s2~female, data=datos2)

stargazer(reg_p4, type = "text", title = "Logaritmo del salario en funcion del genero")

## Teorema FWL

# x1 es la variable female
# x2 son los controles que corresponden a: edad y estrato

reg_p4_controles <- lm(log_s2 ~ female+age+estrato1, data = datos2)
stargazer(reg_p4_controles, type = "text", title = "Logaritmo del salario en funcion del genero")


regaux_p4_x1 <- lm(female~age+estrato1, data=datos2) # Regresion de x2 sobre x1

regaux_p4_y <- lm(log_s2~age+estrato1, data=datos2) # Regresion de x2 sobre y

# Despues de purgar x2 de x1 y de y, se corre la regresion

x1_resid <- regaux_p4_x1$residuals
x1_resid

y_resid <- regaux_p4_y$residuals
y_resid

reg_p4_fwl <- lm(y_resid~x1_resid, data=datos2)
stargazer(reg_p4_controles,reg_p4_fwl, type = "text", title = "Logaritmo del salario en funcion del genero")



# Teorema FWL con bootstrap

fwl_function<-function(datos2,index){
  
  regaux_p4_x1 <- lm(female~age+estrato1, data=datos2, subset=index) # Regresion de x2 sobre x1
  regaux_p4_y <- lm(log_s2~age+estrato1, data=datos2, subset=index)
  
  
  x1_resid <- regaux_p4_x1$residuals
  
  y_resid <- regaux_p4_y$residuals
  
  
  reg_p4_fwl <- lm(y_resid~x1_resid, data=datos2)
  
  beta_female <- reg_p4_fwl$coefficients[2]  
  
  return(beta_female)
}

fwl_function(datos2,1:nrow(datos2))  #Probando la funcion 

##Finalmente hacemos la simulación
set.seed(1234)
boot_p4_ha <- boot(data = datos2, fwl_function, R = 1000)
boot_p4_ha

boot.ci(boot_p4_ha, type = "perc") #Esta funcion me saca los intervalos de confianza al 95% bajo dos metodologias

## ----------

peak_age_female <-function(datos2,index){
  
  datos_muestra <- datos2[index, ]  # Tomar solo las filas seleccionadas por bootstrap
  datos_female <- datos_muestra[datos_muestra$female == 1, ]  # Filtrar solo mujeres
  
  log_salario <- log(datos_female$y_ingLab_m_ha)
  
  reg_p4_peak_female <- lm(log_salario ~ age + I(age^2), data = datos_female)
  
  
  b2_f <- coef(reg_p4_peak_female)[2]
  b3_f <- coef(reg_p4_peak_female)[3]
  
  age_max_f <- -b2_f/(2*b3_f)  #Esto sale de derivar la ecuacion e igualar a 0 en base a los coeficientes estimados
  
  return(age_max_f)
}

peak_age_female(datos2,1:nrow(datos2))  #Probando la funcion 

##Finalmente hacemos la simulación
set.seed(1234)
boot_p4_f <- boot(data = datos2, peak_age_female, R = 1000)
boot_p4_f

boot.ci(boot_p4_f, type = "perc") #Esta funcion me saca los intervalos de confianza al 95% bajo dos metodologias

CF_female <- boot.ci(boot_p4_f, type = "perc")$percent[4:5] #Esto me saca el percentil
edad_max_female <- boot_p4_f$t #Aqui sacamos los valores estimados de cada una de las iteraciones del bootstrap

graph_female <- ggplot(data.frame(edad_max_female), aes(x = edad_max_female)) +
  geom_histogram(aes(y =after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +  # Agregar densidad
  geom_vline(aes(xintercept = mean(edad_max_female)), color = "red", linetype = "dashed", linewidth = 1) +  # Media
  geom_vline(aes(xintercept = CF_female[1]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite inferior IC
  geom_vline(aes(xintercept = CF_female[2]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite superior IC
  labs(title = "Distribución Bootstrap de la Edad de las mujeres con ingresos máximos",
       x = "Edad máxima estimada",
       y = "Densidad") +
  theme_minimal()


## ----

peak_age_male <-function(datos2,index){
  
  datos_muestra <- datos2[index, ]  # Tomar solo las filas seleccionadas por bootstrap
  datos_male <- datos_muestra[datos_muestra$female == 0, ]  # Filtrar solo mujeres
  
  log_salario <- log(datos_male$y_ingLab_m_ha)
  
  reg_p4_peak_male <- lm(log_salario ~ age + I(age^2), data = datos_male)
  
  
  b2_m <- coef(reg_p4_peak_male)[2]
  b3_m <- coef(reg_p4_peak_male)[3]
  
  age_max_m <- -b2_m/(2*b3_m)  #Esto sale de derivar la ecuacion e igualar a 0 en base a los coeficientes estimados
  
  return(age_max_m)
}

peak_age_male(datos2,1:nrow(datos2))  #Probando la funcion 

##Finalmente hacemos la simulación
set.seed(1234)
boot_p4_m <- boot(data = datos2, peak_age_male, R = 1000)
boot_p4_m

boot.ci(boot_p4_m, type = "perc") #Esta funcion me saca los intervalos de confianza al 95% bajo dos metodologias

CF_male <- boot.ci(boot_p4_m, type = "perc")$percent[4:5] #Esto me saca el percentil
edad_max_male <- boot_p4_m$t #Aqui sacamos los valores estimados de cada una de las iteraciones del bootstrap

graph_male <- ggplot(data.frame(edad_max_male), aes(x = edad_max_male)) +
  geom_histogram(aes(y =after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +  # Agregar densidad
  geom_vline(aes(xintercept = mean(edad_max_male)), color = "red", linetype = "dashed", linewidth = 1) +  # Media
  geom_vline(aes(xintercept = CF_male[1]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite inferior IC
  geom_vline(aes(xintercept = CF_male[2]), color = "black", linetype = "dotted", linewidth = 1.2) +  # Límite superior IC
  labs(title = "Distribución Bootstrap de la Edad de los hombres con ingresos máximos",
       x = "Edad máxima estimada",
       y = "Densidad") +
  theme_minimal()


### Grafico combinado


parametros <- theme_minimal(base_size = 12) + 
  theme(
    axis.text = element_text(size = 10),  
    axis.title = element_text(size = 12),  
    panel.grid = element_blank(),  
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10))
  )

graph_female <- ggplot(data.frame(edad_max_female), aes(x = edad_max_female)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = color_fill, color = "white", alpha = 0.8) +
  geom_density(color = color_density, linewidth = 1) +
  geom_vline(aes(xintercept = mean(edad_max_female)), 
             color = color_mean, linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = CF_female[1]), 
             color = color_ci, linetype = "dotted", linewidth = 1.2) +
  geom_vline(aes(xintercept = CF_female[2]), 
             color = color_ci, linetype = "dotted", linewidth = 1.2) +
  labs(title = "A. Mujeres", x = "Edad máxima estimada", y = "Densidad") +
  parametros

graph_male <- ggplot(data.frame(edad_max_male), aes(x = edad_max_male)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = color_fill, color = "white", alpha = 0.8) +
  geom_density(color = color_density, linewidth = 1) +
  geom_vline(aes(xintercept = mean(edad_max_male)), 
             color = color_mean, linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = CF_male[1]), 
             color = color_ci, linetype = "dotted", linewidth = 1.2) +
  geom_vline(aes(xintercept = CF_male[2]), 
             color = color_ci, linetype = "dotted", linewidth = 1.2) +
  labs(title = "B. Hombres", x = "Edad máxima estimada", y = "Densidad") +
  parametros

(graph_female | graph_male) + 
  plot_annotation(
    title = "Distribución Bootstrap de la edad con ingresos máximos",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 15)),
      plot.tag = element_text(size = 12, face = "bold"),
      plot.title.position = "plot"
    )
  )


# ------------------------------------------------------------- #
## ------------------------- PUNTO 5 ------------------------- ##
# ------------------------------------------------------------- #
