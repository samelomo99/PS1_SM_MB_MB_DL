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
  caret,         # For predictive model assessment
  purrr
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
  dplyr::select(
    directorio,    # Llave de vivienda
    orden,         # Llave de persona
    secuencia_p,   # Llave de hogar
    y_ingLab_m_ha,
    y_salary_m_hu,
    sex,
    age,
    maxEducLevel,
    p6050,
    estrato1,
    oficio,
    relab,
    formal,
    informal,
    microEmpresa,
    cuentaPropia,
    sizeFirm,
    cotPension,
    ocu,
    pea,
    regSalud
  )



# Visualizar la estructura de la nueva base de datos
str(datos)

#2. Creamos algunas variables de interes antes filtrar por los mayores de 18    

#2.1. NUMERO DE MENORES EN EL HOGAR
##Crear variable indicador de menores de 18 años
library(dplyr)
datos <- datos %>%
  mutate(flag = ifelse(age <= 6, 1, 0))

#Calcular el número total de menores por hogar (identificado por 'directorio' y 'secuencia_p')
datos <- datos %>%
  group_by(directorio, secuencia_p) %>%
  mutate(nmenores = sum(flag)) %>%
  dplyr::select(-flag) %>% 
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
library(skimr)
skim_data <- skim(datos)
print(skim_data, n = Inf) # Imprimir todas las filas en la consola
View(skim_data) # abrirlo en el visor de datos 

###Se revisa la estructura del dataframe (con glimpse y resumen estadístico básico (summary).
glimpse(datos)
summary(datos)

##Otra opcion de exploracion es usar el paquete summarytool que genera muy completo que incluye las frecuencias y el porcentaje de NA para cada variable
install.packages("summarytools")  # Si no lo tienes instalado
library(summarytools)
resumen <- dfSummary(datos, style = "grid", plain.ascii = FALSE) # resumen completo de la base
print(resumen, method = "browser") # Imprime el resumen en el navegador


##Visualización de datos faltantes, utilizando diferentes metricas aprendidas 
#en clase

skim_data <- as_tibble(skim(datos))  # Forzar a tibble
db_miss <- skim_data %>% dplyr::select(skim_variable, n_missing)


# a) ggplot
library(ggplot2)
ggplot(db_miss, aes(x = reorder(skim_variable, +n_missing), y = n_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings") + 
  theme(axis.text = element_text(size = 5))

# b) Con naniar: gráfico de missing values
vis_miss(datos)

# c) Con DataExplorer: gráfico de missing values
plot_missing(datos)

# d) grafico de correlaciones: creamos a dataset con todas ls variables== 1 if missing
db2 <- datos %>% mutate_all(~ifelse(!is.na(.), 1, 0))
## descartar variables con ningun missing o con todas missing.
db2 <- datos %>% mutate_all(~ifelse(!is.na(.), 1, 0)) %>%
  select_if(~sd(.) > 0)
## Usar el paquete corrplot para visualizar una matriz de correlación 
#de cómo se correlacionan los valores faltantes entre diferentes variables.
M <- cor(db2)
corrplot(M) 


###Nota:para el grupo de variables seleccionado, se encuentra que hay missing en la
#          variables de ingresos y en la de maxeduclevel. Procedemos a dar manejo a
#          los datos faltantes. 

###A continuación se proceder a dar manejo a datos missing,por el momento hacemos 
#   procesamiento de datos a las diferentes formas de la variable de salario/ingreso
#   para definir la mejor opcion 

### la tabla de resumen tambien indica que hay valores extremos en las variables relacionadas
#   con el ingreso, antes de proceder a seleccion nuestra variable de resultado,
#   haremos algunas exploraciones de outliers y su respectivo manejo 


##########################
#PREPARACIÓN DE LOS DATOS
##########################

### 1) maxEducLevel         

ggplot(datos, aes(maxEducLevel)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Max Edu  Distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))   

# como es una variable categórica se reemplaza con el valor mas común (moda)

# se calcula el valor mas comun maxEducLevel. 
mode_edu <- as.numeric(names(sort(table(datos$maxEducLevel), decreasing = TRUE)[1]))

# Imputing the missing value. 
datos <- datos  %>%
  mutate(maxEducLevel_im = ifelse(is.na(maxEducLevel) == TRUE, mode_edu , maxEducLevel))

###  2) regSalud

# Corrigiendo missing en regSalud
#regSalud 1 r. contributivo
#regSalud 2 r. especial
#regSalud 3 r. subsidiado
#regSalud 9 N/A

# numero de missing 
datos %>% 
  summarise(total_na = sum(is.na(regSalud)))

#tabla cruzada para verificar que caracteristiscas tienen aquellos con missing en RegSalud
datos <- datos %>%
  mutate(regSalud_status = ifelse(is.na(regSalud), "Missing", "Not Missing"))

# a) Para la variable formal (=1 if formal (social security); =0 otherwise)
table_formal <- table(datos$regSalud_status, datos$formal)
print("Tabla cruzada: regSalud_status vs. formal")
print(table_formal) #aquellos con missing en Reg Salud es porque no son formales

# b) Para la variable relab 
#relab 1 "Obrero o empleado de empresa particular";
#relab 2 "Obrero o empleado del gobierno";
#relab 3 "Empleado doméstico";
#relab 4 "Trabajador por cuenta propia";
#relab 5 "Patron o empleador";
#relab 6 "Trabajador familiar sin remuneracion";
#relab 7 "Trabajador sin remuneracin en empresas o negocios de otros hogares";
#relab 8 "Jornalero o peon";
#relab 9 "Otro";

table_relab <- table(datos$regSalud_status, datos$relab)
print("Tabla cruzada: regSalud_status vs. relab")
print(table_relab)

table_sizefirm <- table(datos$regSalud_status, datos$sizeFirm)
print("Tabla cruzada: regSalud_status vs. sizefirm")
print(table_sizefirm)

#se observa que los valores missingen RegSalud, estan mas asociados a una 
# condicion de informalidad o personas que son cuenta propia, por tanto
# esto corresponde mas un no aplica, que aun missing real, no seria 
# adecuado reemplazar con la moda, en el directorio de variables, estos
# corresponderian a la categoria No Aplica=9 
datos <- datos %>%
  mutate(regSalud_im = ifelse(is.na(regSalud), 9, regSalud))

table(datos$regSalud)

### 3)CotPension

##revisamos la variable cotPension
table(datos$cotPension)

#observamos que 380 aparecen como pensionados, en teoria ya no tendrian salario
# verificamos que no sean personas por debajo de edad pension mujer<57 y hombre<62

datos <- datos %>%
  mutate(pension_status = case_when(
    cotPension == 3 & ((gender == 0 & age <= 56) | (gender == 1 & age <= 61)) ~ 1,
    cotPension == 3 & ((gender == 0 & age >= 57) | (gender == 1 & age >= 62)) ~ 2,
    cotPension %in% c(1, 2) ~ 3,
    TRUE ~ 3
  ))

table(datos$pension_status)

# crosstab entre pensionados validos (2) y no valido(1) y relab
tabla_cruzada <- datos %>%
  filter(pension_status %in% c(1, 2)) %>%
  with(table(relab, pension_status))
print(tabla_cruzada)

# crosstab entre ingreso y pension status
resumen <- datos %>%
  filter(pension_status %in% c(1, 2)) %>%
  group_by(pension_status) %>%
  summarise(
    n_con_info = sum(!is.na(y_ingLab_m_ha)),
    n_sin_info = sum(is.na(y_ingLab_m_ha))
  )
print(resumen)

#crosstab entre pension_status y relab
tabla_cruzada <- with(datos[datos$pension_status %in% c(1, 2), ], table(pension_status, relab))
print(tabla_cruzada)

resumen <- datos %>%
  filter(pension_status %in% c(1, 2)) %>%
  mutate(info_ingreso = ifelse(is.na(y_ingLab_m_ha), "Sin info", "Con info")) %>%
  group_by(pension_status, info_ingreso) %>%
  summarise(cantidad = n(), .groups = "drop")
print(resumen)

##corrigiendo datos mal clasificados

table(datos$cotPension)
#recodificamos cotPension=1 cuando pension_status=1
datos <- datos %>%
  mutate(cotPension_im = ifelse(pension_status == 1, 1, cotPension))
table(datos$cotPension)


### 4) relab 
# la variable relab me indica el tipo de relacion laboral,
# se considera eliminar registros cuya categoria es 6 y 7, y no tiene
# informacion de ingreso ya que por definición no generan 
#remuneración que se pueda modelar.

table(datos$relab)

tabla_resumen <- datos %>%
  filter(relab %in% c(6, 7)) %>%
  group_by(relab, missing = is.na(y_ingLab_m_ha)) %>%
  summarise(cantidad = n(), .groups = "drop")

print(tabla_resumen)

#se eliminan los casos donde relab 6 o 7 (no remuerado) y no tiene informacion deingresos
datos <- datos %>% 
  filter(!(relab %in% c(6, 7) & is.na(y_ingLab_m_ha)))          

### 5) y_ingLab_m_ha

    # Numero de missing de la variable 
    is.na(datos$y_ingLab_m_ha) %>% table()
    
    #distribucion de la variable ingreso 
    
    ggplot(datos, aes(y_ingLab_m_ha)) +
      geom_histogram(color = "#000000", fill = "#0099F8") +
      geom_vline(xintercept = median(datos$y_ingLab_m_ha, na.rm = TRUE), linetype = "dashed", color = "red") +
      geom_vline(xintercept = mean(datos$y_ingLab_m_ha, na.rm = TRUE), linetype = "dashed", color = "blue") +  
      ggtitle("labor income salaried - nomial hourly - all occ+tip+comis") +
      theme_classic() +
      theme(plot.title = element_text(size = 18))
    
    #dado que la distribucion de la variable tiene una cola larga a la derecha es mas adecuado usar la mediana
    # como metodo para imputar observaciones faltantes
    datos <- datos  %>%
      mutate(y_ingLab_m_ha_im = ifelse(is.na(y_ingLab_m_ha) == TRUE, median(datos$y_ingLab_m_ha, na.rm = TRUE) , y_ingLab_m_ha))
    
    ggplot(datos, aes(y_ingLab_m_ha_im)) +
      geom_histogram(color = "#000000", fill = "#0099F8") +
      geom_vline(xintercept = median(datos$y_ingLab_m_ha_im, na.rm = TRUE), linetype = "dashed", color = "red") +
      geom_vline(xintercept = mean(datos$y_ingLab_m_ha_im, na.rm = TRUE), linetype = "dashed", color = "blue") +  
      ggtitle("labor income salaried - nomial hourly - all occ+tip+comis") +
      theme_classic() +
      theme(plot.title = element_text(size = 18))
    
    summary(datos[, c("y_ingLab_m_ha_im", "y_ingLab_m_ha")])
    
    # gráfico de missing values
    plot_missing(datos)
    
    ###manejo de outliers variable ingreso por hora
    
    # Librerías necesarias
    library(dplyr)
    library(ggplot2)
    library(gridExtra)
    
    # 1. Exploración inicial de la variable de ingreso
    summary(datos$y_ingLab_m_ha_im)
    boxplot(datos$y_ingLab_m_ha_im, main = "Boxplot de y_ingLab_m_ha_im (imputada)")
    
    # 2. Definimos los umbrales para marcar outliers (1% y 99%)
    p_inferior <- 0.01
    p_superior <- 0.99
    
    umbral_inferior <- quantile(datos$y_ingLab_m_ha_im, probs = p_inferior, na.rm = TRUE)
    umbral_superior <- quantile(datos$y_ingLab_m_ha_im, probs = p_superior, na.rm = TRUE)
    
    # Filtrar registros que están por debajo o por encima de dichos umbrales
    extremos <- datos %>%
      filter(y_ingLab_m_ha_im < umbral_inferior | y_ingLab_m_ha_im > umbral_superior)
    
    n_extremos <- nrow(extremos)
    cat("Número de registros outliers (1% - 99%):", n_extremos, "\n")
    summary(extremos$y_ingLab_m_ha_im)
    
    # 2.1. Revisión específica del límite inferior
    extremos_inferior <- datos %>%
      filter(y_ingLab_m_ha_im < umbral_inferior)
    
    n_inferior <- nrow(extremos_inferior)
    cat("Número de observaciones por debajo del límite inferior (1%):", n_inferior, "\n")
    summary(extremos_inferior$y_ingLab_m_ha_im)
    
    # 2.2. Resumen de outliers por oficio
    tabla_extremos_oficio <- table(extremos$oficio)
    print(tabla_extremos_oficio)
    
    resumen_extremos_oficio <- extremos %>%
      group_by(oficio) %>%
      summarise(
        count        = n(),
        mean_outlier = mean(y_ingLab_m_ha_im, na.rm = TRUE),
        max_outlier  = max(y_ingLab_m_ha_im, na.rm = TRUE)
      )
    print(resumen_extremos_oficio)
    
    # 3. Visualización en escala original
    
    # 3.1. Boxplot con líneas de percentil 1% y 99%
    b <- ggplot(data = datos, aes(x = "", y = y_ingLab_m_ha_im)) +
      geom_boxplot() +
      theme_bw() +
      ggtitle("Boxplot de y_ingLab_m_ha_im (Escala original)") +
      ylab("Ingreso por hora (original)") +
      xlab("") +
      geom_hline(yintercept = umbral_inferior, linetype = "solid", color = "blue", linewidth = 0.7) +
      geom_hline(yintercept = umbral_superior, linetype = "solid", color = "blue", linewidth = 0.7)
    
    # 3.2. Boxplot con líneas de media ± 2*sd
    mean_val <- mean(datos$y_ingLab_m_ha_im, na.rm = TRUE)
    sd_val   <- sd(datos$y_ingLab_m_ha_im, na.rm = TRUE)
    low_2sd  <- mean_val - 2 * sd_val
    up_2sd   <- mean_val + 2 * sd_val
    
    c <- ggplot(data = datos, aes(x = "", y = y_ingLab_m_ha_im)) +
      geom_boxplot() +
      theme_bw() +
      ggtitle("Boxplot (Media ± 2*SD) - Escala original") +
      ylab("Ingreso por hora (original)") +
      xlab("") +
      geom_hline(yintercept = low_2sd, linetype = "solid", color = "red", size = 0.7) +
      geom_hline(yintercept = up_2sd,  linetype = "solid", color = "red", size = 0.7)
    
    grid.arrange(b, c, ncol = 2)
    
    # 4. Marcar outliers en la base de datos (según media ± 2*sd, por ejemplo)
    datos <- datos %>% 
      mutate(out_y_ingLab_m_ha_im = ifelse(y_ingLab_m_ha_im < low_2sd | y_ingLab_m_ha_im > up_2sd, 1, 0))
    
    # 5. Visualización en escala logarítmica
    
    # La variable ingreso por hora presenta una gran asimetría 
    # (la mediana es 5.055 mientras que el máximo es 350.583), 
    # la mayoría de los datos se concentran en un rango bajo y 
    # unos pocos valores extremos elevan la media. Esto sugiere 
    # que para visualizar adecuadamente la distribución, sería 
    # recomendable usar una transformación logarítmica en el eje y.
    # la transformación logarítmica ayuda a "comprimir" la escala de los 
    # valores extremos y a visualizar mejor la dispersión de la mayoría de 
    # los datos.
    
    # 5.1. Boxplot con percentiles 1% y 99% (escala log)
    plot1 <- ggplot(datos, aes(x = "", y = y_ingLab_m_ha_im)) +
      geom_boxplot() +
      scale_y_log10() +
      theme_bw() +
      ggtitle("Boxplot (Percentiles 1% y 99%) - Escala log") +
      ylab("Salario por hora (log10)") +
      xlab("") +
      geom_hline(yintercept = umbral_inferior, linetype = "solid", color = "blue", size = 0.7) +
      geom_hline(yintercept = umbral_superior, linetype = "solid", color = "blue", size = 0.7)
    
    # 5.2. Boxplot con media ± 2*sd calculados sobre los datos log
    log_data <- log10(datos$y_ingLab_m_ha_im)
    mean_log <- mean(log_data, na.rm = TRUE)
    sd_log   <- sd(log_data, na.rm = TRUE)
    low_log  <- 10^(mean_log - 2 * sd_log)
    up_log   <- 10^(mean_log + 2 * sd_log)
    
    plot2 <- ggplot(datos, aes(x = "", y = y_ingLab_m_ha_im)) +
      geom_boxplot() +
      scale_y_log10() +
      theme_bw() +
      ggtitle("Boxplot (Media ± 2*SD) - Escala log") +
      ylab("Salario por hora (log10)") +
      xlab("") +
      geom_hline(yintercept = low_log, linetype = "solid", color = "red", size = 0.7) +
      geom_hline(yintercept = up_log,  linetype = "solid", color = "red", size = 0.7)
    
    grid.arrange(plot1, plot2, ncol = 2)  
    
    
    
    #### Despues de revisar se observa que los valores altos de salario 
    ##   por son viables, sin embargo, hay valore extremadamente bajo
    #    si bien, es posible que se deba a errores de digitación(falta un cero)
    #    es dificil comprobar con la informacion disponible, lo conveniente seria
    #    excluir aquellas observaciones con valores muy pequeños 
    
    extremos_inferior$y_ingLab_m_ha_im
    
    ## 6. winsorizing 
    
    # Calcular el valor del percentil 5
    p5 <- quantile(datos$y_ingLab_m_ha_im, 0.05, na.rm = TRUE)
    
    # Filtrar las observaciones que estén en o por debajo de ese valor
    ingreso_p5 <- datos %>%
      filter(y_ingLab_m_ha_im <= p5)
    
    # Ver cuántos registros hay y revisar sus valores
    nrow(ingreso_p5)
    summary(ingreso_p5$y_ingLab_m_ha_im)
    
    #Crear la variable winsorizada solo para la cola inferior
    datos <- datos %>%
      mutate(y_ingLab_m_ha_wins = ifelse(
        y_ingLab_m_ha_im < p5, p5, y_ingLab_m_ha_im
      ))
    
    # 3. Comparar estadísticas entre la variable original y la variable recortada
    summary(datos$y_ingLab_m_ha_im)
    summary(datos$y_ingLab_m_ha_wins)
    
    
    
    ############################################
    #ANALISIS DESCRIPTIVO DE LOS DATOS-INFORME
    ############################################       
    
    library(dplyr)       # Manipulación de datos
    library(ggplot2)     # Gráficos
    library(gridExtra)   # Disposición de gráficos en grilla
    library(knitr)       # kable() para generar tablas en LaTeX
    library(kableExtra)  # Opciones adicionales para kable()
    library(summarytools)
    
    
    #Generamos summary general de las variables modificadas y de interes
    
    variables_seleccionadas <- c("y_ingLab_m_ha_wins", "nmenores", "age", 
                                 "gender", "estrato1", "formal", 
                                 "sizeFirm", "H_Head", "Head_Female", 
                                 "maxEducLevel_im", "regSalud_im", "cotPension_im","relab","oficio")
    datos_sub <- datos[, variables_seleccionadas]
    resumen <- dfSummary(datos_sub, style = "grid", plain.ascii = FALSE)
    print(resumen, method = "browser")
    
    #Analisis descriptivos adicionales 
    
    # Variables numéricas
    numeric_vars <- c("y_ingLab_m_ha_wins", "nmenores", "age")    
    
    # Variables categóricas
    categorical_vars <- c("gender", "estrato1", "relab", "formal", "sizeFirm",
                          "H_Head", "Head_Female", "maxEducLevel_im", 
                          "regSalud_im", "cotPension_im")
    

##################################
#esto ya se hizo en la linea 151-152
# Filtramos por mayores (o iguales) a 18 y por personas ocupadas. 
#datos <- datos %>% filter(age >= 18, ocu == 1)
#summary(datos$age) # Comprobamos que el mínimo es 18 años.
#skim(datos)
###################


## --- Tratamniento Missing Values --- ##

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



# ------------------------------------------------------------- #
## ------------------------- PUNTO 3 ------------------------- ##
# ------------------------------------------------------------- #

## Creamos las variables

datos1 <- datos1 %>% mutate(log_s1 = log(y_ingLab_m_ha))
datos2 <- datos2 %>% mutate(log_s2 = log(y_ingLab_m_ha))


## La regresión 

reg_p3_1s <- log_s1 ~ age + I(age^2)
modelo_p3_1s <- lm(reg_p3_1s, data = datos1)

reg_p3_2s <- log_s2 ~ age + I(age^2)
modelo_p3_2s <- lm(reg_p3_2s, data = datos2)

#Generacion de la tabla 
stargazer(modelo_p3_s1, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3_1s, type = "latex", title = "Logaritmo del salario en funcion de la edad")

stargazer(modelo_p3_2s, type = "text", title = "Logaritmo del salario en funcion de la edad")
##stargazer(reg_p3_2s, type = "latex", title = "Logaritmo del salario en funcion de la edad")


# Sacando los coeficientes

modelo_p3_1s$coefficients
modelo_p3_2s$coefficients


## Revisando el ajuste intramodelo

log_s_hat_1 <- predict(modelo_p3_1s, newdata = datos1)
log_s_hat_2  <- predict(modelo_p3_2s, newdata = datos2)

MSE_1s <- mean((log_s1 - log_s_hat_1)^2)
MSE_2s <- mean((log_s2 - log_s_hat_2)^2)

MSE_1s
MSE_2s

## En este caso el MSE2s se ajusta mejor y nos va mejor en el modelo de medias. 
## Nos casamos con el modelo de medias (datos2)


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
  mutate(female = ifelse(gender == 0, 1, 0))

reg_p4 <- log_s2~female
modelo_p4_1 <- lm(log_s2~female, data=datos2)

stargazer(modelo_p4_1, type = "text", title = "Logaritmo del salario en funcion del genero")

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

reg_p4_fwl <- y_resid~x1_resid
modelo_p4_fwl <- lm(reg_p4_fwl, data=datos2)
stargazer(reg_p4_controles, modelo_p4_fwl, type = "text", title = "Logaritmo del salario en funcion del genero")


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
                 fill = "lightblue", color = "white", alpha = 0.8) +
  geom_density(color = "blue", linewidth = 1) +
  geom_vline(aes(xintercept = mean(edad_max_female)), 
             color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = CF_female[1]), 
             color = "black", linetype = "dotted", linewidth = 1.2) +
  geom_vline(aes(xintercept = CF_female[2]), 
             color = "black", linetype = "dotted", linewidth = 1.2) +
  labs(title = "A. Mujeres", x = "Edad máxima estimada", y = "Densidad") +
  parametros

graph_male <- ggplot(data.frame(edad_max_male), aes(x = edad_max_male)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = "lightblue", color = "white", alpha = 0.8) +
  geom_density(color = "blue", linewidth = 1) +
  geom_vline(aes(xintercept = mean(edad_max_male)), 
             color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = CF_male[1]), 
             color = "black", linetype = "dotted", linewidth = 1.2) +
  geom_vline(aes(xintercept = CF_male[2]), 
             color = "black", linetype = "dotted", linewidth = 1.2) +
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

set.seed(10101)
## ------------------- ##
## ---- Punto 5.1 ---- ## 
## ------------------- ##

# Partimos nuestros datos en training y test

inTrain <- createDataPartition(
  y = datos2$log_s2,  ## the outcome data are needed
  p = .70, ## The percentage of training data
  list = FALSE
)

# Definimos el subset de test y training
train <- datos2 %>%  filter(row_number() %in% inTrain)
test <- datos2 %>% filter(!row_number() %in% inTrain)

# Podemos visualizar los datos
split_data <- data.frame(
  Split = factor(c("Training", "Testing")),
  Count = c(nrow(train), nrow(test)),
  Percentage = c(nrow(train)/nrow(datos2)*100, nrow(test)/nrow(datos2)*100)
)

ggplot(split_data, aes(x = Split, y = Count)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%\n(n=", Count, ")")), 
            vjust = -0.5, color = "black", size = 4) +
  labs(title = "Train-Test Split Distribution",
       y = "Number of Observations",
       x = "") +
  theme_bw() +
  ylim(0, max(split_data$Count) * 1.2)

## ------------------- ##
## ---- Punto 5.2 ---- ## 
## ------------------- ##

# Performance modelos anteriores y modelos adicionales
# MODELOS
modelo1 <- lm(reg_p3_2s, data = train)
modelo2 <- lm(reg_p4, data = train)
modelo3 <- lm(reg_p4_fwl, data = train)

# Performance (fuera de muestra)
predictions <- list(
  pred1 = predict(modelo1, test),
  pred2 = predict(modelo2, test),
  pred3 = predict(modelo3, test)
)

# Calcular RMSE sin volver a aplicar `predict()`
performance <- tibble(
  modelo = names(predictions),
  RMSE = map_dbl(predictions, ~ RMSE(.x, test$log_s2))
)

performance
