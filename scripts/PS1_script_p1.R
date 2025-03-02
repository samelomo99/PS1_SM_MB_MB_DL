#-------------------------------------------------------------------# ----
## --------------- Problem Set 1: Predicting Income -------------- ##
## - Santiago Melo - Miguel Blanco - María Bernal - Diana Lopera - ##
#-------------------------------------------------------------------#

# Instalación / Librerías

# Instalar y cargar pacman si aún no lo tienes
if (!require("pacman")) install.packages("pacman")
library(pacman)

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
  purrr,
  kableExtra,   # Opciones adicionales para kable()
  dplyr,          # Manipulación de datos
  summarytools,
  knitr,          # kable() para generar tablas en LaTeX
  xtable,
  tidyr,
  gmodels
)


# ------------------------------------------------------------- # ----
## ------------------------- PUNTO 2 ------------------------- ##
# ------------------------------------------------------------- #

# --------------------------------------------------------------- #
#   ----- MANEJO DE DATOS Y ELECCIÓN DE VARIABLES DE INTERÉS -----#
# --------------------------------------------------------------- #

# Usamos la base de datos scrapeada y subida al repositorio en GitHub

datos <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS1_SM_MB_MB_DL/main/stores/GEIH_2018_sample_all.csv"
  )

## filtramos solo las variables de interes y construimos variables necesarias

datos <- datos %>%
  dplyr::select(
    directorio,    # Llave de vivienda
    orden,         # Llave de persona
    secuencia_p,   # Llave de hogar
    y_ingLab_m_ha,
    sex,
    age,
    maxEducLevel,
    p6050,
    estrato1,
    oficio,
    relab,
    formal,
    informal,
    sizeFirm,
    cotPension,
    ocu,
  )

# Visualizar la estructura de la nueva base de datos
str(datos)


# Creamos algunas variables de interés antes filtrar por los mayores o iguales a 18    

# NÚMERO DE MENORES EN EL HOGAR

# Crear variable indicador de menores de 18 años

datos <- datos %>%
  mutate(flag = ifelse(age <= 6, 1, 0))

# Calcular el número total de menores por hogar (identificado por 'directorio' y 'secuencia_p')
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


# JEFE DEL HOGAR
datos <- datos %>%
  mutate(H_Head = ifelse( p6050== 1, 1, 0))

table(datos$H_Head)

# JEFE DE HOGAR MUJER 

# Renombramos la variable sex=gender (Nota. gender: 0=mujer 1=hombre)
datos <- as_tibble(datos) %>% rename(gender=sex)

# Creamos la variable de jefe de hogar mujer 
datos <- datos %>%
  mutate(Head_Female = H_Head*(1-gender))

table(datos$Head_Female)
table(datos$gender)  


# Filtramos por mayores (o iguales) a 18 y por personas ocupadas. 
datos <- datos %>% filter(age >= 18, ocu == 1)
summary(datos$age) # Comprobamos que el mínimo es 18 años.


# ------------------------------------ # ----
# ------ INSPECCIÓN DE LOS DATOS ----- #
# ------------------------------------ #

# Pasamos el data.frame a tibble
datos <- as_tibble(datos)
head(datos)

# Inspección básica de la estructura y resumen de datos

skim_data <- skim(datos)
View(skim_data) # abrirlo en el visor de datos 

# Se revisa la estructura del dataframe (con glimpse y resumen estadístico básico (summary).
glimpse(datos)
summary(datos)

# Resumen de los datos - Summary Tools
resumen <- dfSummary(datos, style = "grid", plain.ascii = FALSE)
print(resumen, method = "browser") 


# Tratamiento NA / Missing Values
skim_data <- as_tibble(skim(datos))  # Forzar a tibble
db_miss <- skim_data %>% dplyr::select(skim_variable, n_missing)

# Visualización
# a) ggplot
library(ggplot2)
db_miss %>%
  filter(n_missing > 0) %>%  # Filtra solo variables con valores perdidos
  ggplot(aes(x = reorder(skim_variable, +n_missing), y = n_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings") + 
  theme(axis.text = element_text(size = 5))

# b) Con DataExplorer: gráfico de missing values
datos_filtrados <- datos %>% select_if(~ sum(is.na(.)) > 0) # Filtra solo variables con NA
plot_missing(datos_filtrados)
 
pdf("plot_missing_val.pdf", width = 8, height = 6)  
plot_missing(datos_filtrados)  
dev.off()  # Cierra el dispositivo PDF

# ------------------------------------ #----
# ----- PREPARACIÓN DE LOS DATOS ----- #
# ------------------------------------ #

###################
## maxEducLevel         
###################

ggplot(datos, aes(maxEducLevel)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Max Edu  Distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))   

# Como es una variable categórica se reemplaza con el valor mas común (moda)

mode_edu <- as.numeric(names(sort(table(datos$maxEducLevel), decreasing = TRUE)[1]))

# Corregimos el Missing Value 
datos <- datos  %>%
  mutate(maxEducLevel_im = ifelse(is.na(maxEducLevel) == TRUE, mode_edu , maxEducLevel))

###################
##cotPension
###################

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
table(datos$cotPension_im)

###################
# relab 
###################
  # la variable relab me indica el tipo de relacion laboral,
  # se considera eliminar registros cuya categoria es 6 y 7, y no tiene
  # informacion de ingreso ya que por definición no generan 
  #remuneración que se pueda modelar.



tabla_resumen <- datos %>%
  filter(relab %in% c(6, 7)) %>%
  group_by(relab, missing = is.na(y_ingLab_m_ha)) %>%
  summarise(cantidad = n(), .groups = "drop")

print(tabla_resumen)

#se eliminan los casos donde relab 6 o 7 (no remuerado) y no tiene informacion deingresos
datos <- datos %>% 
  filter(!(relab %in% c(6, 7) & is.na(y_ingLab_m_ha)))          

###################
###y_ingLab_m_ha
###################

# Numero de missing de la variable 
is.na(datos$y_ingLab_m_ha) %>% table()

#distribución de la variable ingreso 

    
ggplot(datos, aes(y_ingLab_m_ha)) +
      geom_histogram(color = "#000000", fill = "#0099F8") +
      geom_vline(xintercept = median(datos$y_ingLab_m_ha, na.rm = TRUE), linetype = "dashed", color = "red") +
      geom_vline(xintercept = mean(datos$y_ingLab_m_ha, na.rm = TRUE), linetype = "dashed", color = "blue") +  
      ggtitle("labor income salaried - nomial hourly - all occ+tip+comis") +
      theme_classic() +
      theme(plot.title = element_text(size = 18))
    
##(aseguramos que los missing no sean porque son trabajadores no remunerados
    
trabj_noremuner <- subset(datos, relab == 6 & is.na(y_ingLab_m_ha))
cantidad <- nrow(trabj_noremuner)
print(paste("La cantidad de personas con relab=6 y y_ingLab_m_ha missing es:", cantidad))
    
    
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
    


#########################################################
###manejo de outliers 
#########################################################
    
# Librerías necesarias
library(dplyr)
library(ggplot2)
library(gridExtra)
  
# Exploración inicial de la variable de ingreso

# Definimos los umbrales para marcar outliers (1% y 99%)
p_inferior <- 0.01
p_superior <- 0.99

    # No obstante, creamos otra variable con el uso de media como reemplazo de NA
    datos <- datos  %>%
      mutate(y_ingLab_m_ha_mean = ifelse(is.na(y_ingLab_m_ha) == TRUE, mean(datos$y_ingLab_m_ha, na.rm = TRUE) , y_ingLab_m_ha))
    
    ggplot(datos, aes(y_ingLab_m_ha_mean)) +
      geom_histogram(color = "#000000", fill = "#0099F8") +
      geom_vline(xintercept = median(datos$y_ingLab_m_ha_im, na.rm = TRUE), linetype = "dashed", color = "red") +
      geom_vline(xintercept = mean(datos$y_ingLab_m_ha_im, na.rm = TRUE), linetype = "dashed", color = "blue") +  
      ggtitle("labor income salaried - nomial hourly - all occ+tip+comis") +
      theme_classic() +
      theme(plot.title = element_text(size = 18))
    
    summary(datos[, c("y_ingLab_m_ha_mean", "y_ingLab_m_ha")])
    
    # gráfico de missing values
  
    #########################################################
    ###manejo de outliers 
    #########################################################

    
umbral_inferior <- quantile(datos$y_ingLab_m_ha_im, probs = p_inferior, na.rm = TRUE)
umbral_superior <- quantile(datos$y_ingLab_m_ha_im, probs = p_superior, na.rm = TRUE)
    

# Filtrar registros que están por debajo o por encima de dichos umbrales
extremos <- datos %>%
    filter(y_ingLab_m_ha_im < umbral_inferior | y_ingLab_m_ha_im > umbral_superior)

    # Exploración inicial de la variable de ingreso

    
n_extremos <- nrow(extremos)
cat("Número de registros outliers (1% - 99%):", n_extremos, "\n")
summary(extremos$y_ingLab_m_ha_im)
    
# Revisión específica del límite inferior
extremos_inferior <- datos %>%
    filter(y_ingLab_m_ha_im < umbral_inferior)
    
n_inferior <- nrow(extremos_inferior)
cat("Número de observaciones por debajo del límite inferior (1%):", n_inferior, "\n")
summary(extremos_inferior$y_ingLab_m_ha_im)
    
# Resumen de outliers por oficio
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
    
# Visualización en escala original
    
# Boxplot con líneas de percentil 1% y 99%
b <- ggplot(data = datos, aes(x = "", y = y_ingLab_m_ha_im)) +
      geom_boxplot() +
      theme_bw() +
      ggtitle("y_ingLab_m_ha_im (Escala original:
              percentil 1% y 99%)") +
      ylab("Ingreso por hora (original)") +
      xlab("") +
      geom_hline(yintercept = umbral_inferior, linetype = "solid", color = "blue", linewidth = 0.7) +
      geom_hline(yintercept = umbral_superior, linetype = "solid", color = "blue", linewidth = 0.7)
    
# Boxplot con líneas de media ± 2*sd
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
    
# Marcar outliers en la base de datos (según media ± 2*sd, por ejemplo)
datos <- datos %>% 
mutate(out_y_ingLab_m_ha_im = ifelse(y_ingLab_m_ha_im < low_2sd | y_ingLab_m_ha_im > up_2sd, 1, 0))
    
# Visualización en escala logarítmica
    
# Boxplot con percentiles 1% y 99% (escala log)
plot1 <- ggplot(datos, aes(x = "", y = y_ingLab_m_ha_im)) +
      geom_boxplot() +
      scale_y_log10() +
      theme_bw() +
      ggtitle("y_ingLab_m_ha_im(Escala log):
              Percentiles 1% y 99%") +
      ylab("Salario por hora (log10)") +
      xlab("") +
      geom_hline(yintercept = umbral_inferior, linetype = "solid", color = "blue", size = 0.7) +
      geom_hline(yintercept = umbral_superior, linetype = "solid", color = "blue", size = 0.7)
    
# Boxplot con media ± 2*sd calculados sobre los datos log
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
    
    
##winsorizing 
  
extremos_inferior$y_ingLab_m_ha_im
    
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
    
#Comparar estadísticas entre la variable original y la variable recortada
summary(datos$y_ingLab_m_ha_im)
summary(datos$y_ingLab_m_ha_wins)
    
#visualizando el boxplot de nuestra variable de resulatado: ingreso windzorizada 
ggplot(datos_sub, aes(x = y_ingLab_m_ha_wins)) +
    geom_histogram(aes(y = ..density..),
                   bins = 30,
                   fill = "skyblue",
                   color = "black",
                   alpha = 0.7) +
    geom_density(color = "red", size = 1) +
    labs(title = "Distribución del Ingreso Laboral (y_ingLab_m_ha_wins)",
         x = "Ingreso laboral por hora",
         y = "Densidad") +
      theme_minimal() +
      scale_x_continuous(trans = "log10")  # Escala logarítmica en eje x, opcional
    
##bloxplot ingreso 
umbral_inferior <- quantile(datos_sub$y_ingLab_m_ha_wins, probs = 0.01, na.rm = TRUE)
umbral_superior <- quantile(datos_sub$y_ingLab_m_ha_wins, probs = 0.99, na.rm = TRUE)
    
# escala original (percentiles 1% y 99%)
e <- ggplot(datos_sub, aes(x = "", y = y_ingLab_m_ha_wins)) +
      geom_boxplot() +
      theme_bw() +
      ggtitle("y_ingLab_m_ha_wins Escala original): 
              Percentil 1% y 99%") +
      ylab("Ingreso por hora (original)") +
      xlab("") +
      geom_hline(yintercept = umbral_inferior, linetype = "solid",
                 color = "blue", linewidth = 0.7) +
      geom_hline(yintercept = umbral_superior, linetype = "solid",
                 color = "blue", linewidth = 0.7)
    
# Boxplot en escala logarítmica (percentiles 1% y 99%)
    
e_log <- ggplot(datos_sub, aes(x = "", y = y_ingLab_m_ha_wins)) +
    geom_boxplot() +
    scale_y_log10() +  # <-- Escala logarítmica para manejar asimetrías
    theme_bw() +
    ggtitle("y_ingLab_m_ha_wins(Escala log): 
              Percentil 1% y 99%") +
    ylab("Ingreso por hora (log)") +
    xlab("") +
    geom_hline(yintercept = umbral_inferior, linetype = "solid",
                 color = "blue", linewidth = 0.7) +
    geom_hline(yintercept = umbral_superior, linetype = "solid",
                 color = "blue", linewidth = 0.7)
    
    combined_plot <- grid.arrange(e, e_log,ncol = 2)
ggsave("boxplots_ing.pdf",plot   = combined_plot,device = "pdf",width  = 10,height = 8)

##combinando todas las graficas     
combined_plot_all <-grid.arrange(b,plot1, e, e_log, ncol = 2)  
ggsave("boxplots_ing_all.pdf",plot   = combined_plot_all,device = "pdf",width  = 10,height = 8)
    
    
##Identificando dependencia entre variable categoricas relacionadas con la firma y mercado laboral
  
###sizefirm vs. formal 
CrossTable(
    datos_sub$sizeFirm,
    datos_sub$formal, 
    prop.r    = TRUE,   # proporción por fila
    prop.c    = FALSE,  # no proporción por columna
    prop.t    = FALSE,   # proporción sobre total
    prop.chisq= FALSE    # incluir prueba de chi-cuadrado
    )
    
# Realizar la prueba chi-cuadrado y capturar los resultados
chi_test <- chisq.test(table(datos_sub$formal, datos_sub$sizeFirm))
    
# Imprimir los resultados en la consola
cat("\nChi-Square Test Results:\n")
cat(paste("Chi-squared:", chi_test$statistic, "\n"))
cat(paste("Degrees of freedom:", chi_test$parameter, "\n"))
cat(paste("P-value:", chi_test$p.value, "\n"))
    
tabla1 <- as.data.frame(tabla$t)
# Guardar tabla de contingencia en LaTeX
stargazer(tabla_df, type = "latex", summary = FALSE, out = "tabla_contingencia1.tex",
              title = "Tabla de Contingencia entre Tamaño de Firma y Formalidad",
              label = "tab:contingencia")
    
### relab vs. formal           

    
library(dplyr)       # Manipulación de datos
library(ggplot2)     # Gráficos
library(gridExtra)   # Disposición de gráficos en grilla
library(knitr)       # kable() para generar tablas en LaTeX
library(kableExtra)  # Opciones adicionales para kable()
library(summarytools)
    
CrossTable(
      datos_sub$relab,
      datos_sub$formal, 
      prop.r    = TRUE,   # proporción por fila
      prop.c    = FALSE,  # no proporción por columna
      prop.t    = FALSE,   # proporción sobre total
      prop.chisq= FALSE    # incluir prueba de chi-cuadrado
    )
    
# Realizar la prueba chi-cuadrado y capturar los resultados
chi_test <- chisq.test(table(datos_sub$formal, datos_sub$relab))
    
# Imprimir los resultados en la consola
cat("\nChi-Square Test Results:\n")
cat(paste("Chi-squared:", chi_test$statistic, "\n"))
cat(paste("Degrees of freedom:", chi_test$parameter, "\n"))
cat(paste("P-value:", chi_test$p.value, "\n"))
    
tabla2 <- as.data.frame(tabla$t)
# Guardar tabla de contingencia en LaTeX
stargazer(tabla_df, type = "latex", summary = FALSE, out = "tabla_contingencia2.tex",
              title = "Tabla de Contingencia entre Tipo de ocupacion y Formalidad",
              label = "tab:contingencia")
    
### formal vs. cotPension_im          
    
CrossTable(
      datos_sub$cotPension_im,
      datos_sub$formal, 
      prop.r    = TRUE,   # proporción por fila
      prop.c    = FALSE,  # no proporción por columna
      prop.t    = FALSE,   # proporción sobre total
      prop.chisq= FALSE    # incluir prueba de chi-cuadrado
    )
    
# Realizar la prueba chi-cuadrado y capturar los resultados
chi_test <- chisq.test(table(datos_subt$formal, datos_sub$cotPension_im))
    
# Imprimir los resultados en la consola
cat("\nChi-Square Test Results:\n")
cat(paste("Chi-squared:", chi_test$statistic, "\n"))
cat(paste("Degrees of freedom:", chi_test$parameter, "\n"))
cat(paste("P-value:", chi_test$p.value, "\n"))
    
tabla3 <- as.data.frame(tabla$t)
# Guardar tabla de contingencia en LaTeX
stargazer(tabla_df, type = "latex", summary = FALSE, out = "tabla_contingencia3.tex",
          title = "Tabla de Contingencia entre Cotiza pension y Formalidad",
          label = "tab:contingencia")    
    
    
    
############################################
#ANALISIS DESCRIPTIVO DE LOS DATOS-INFORME
############################################       

    
variables_seleccionadas <- c("y_ingLab_m_ha_wins", "nmenores", "age", 
                                 "gender", "estrato1", "formal", 
                                 "sizeFirm", "H_Head", "Head_Female", 
                                 "maxEducLevel_im", "cotPension_im","relab","oficio")
datos_sub <- datos[, variables_seleccionadas]
  
    
# Variables numéricas
numeric_vars <- c("y_ingLab_m_ha_wins", "nmenores", "age")    
    
# Variables categóricas
categorical_vars <- c("gender", "estrato1", "relab", "formal", "sizeFirm",
                          "H_Head", "Head_Female", "maxEducLevel_im", 
                           "cotPension_im")
    
# Etiquetas de las variables
    
    
datos_sub$gender <- factor(datos_sub$gender,
                               levels = c(0, 1),
                               labels = c("Mujer", "Hombre"))
    
# H_Head: 1=Jefe de hogar, 0=Otro
datos_sub$H_Head <- factor(datos_sub$H_Head,
                               levels = c(0, 1),
                               labels = c("Otro", "Jefe de hogar"))
    
# estrato1: 1=Estrato 1, 2=Estrato 2, 3=Estrato 3, 4=Estrato 4, 5=Estrato 5, 6=Estrato 6
datos_sub$estrato1 <- factor(datos_sub$estrato1,
                                 levels = c(1, 2, 3, 4, 5, 6),
                                 labels = c("Estrato 1", "Estrato 2", "Estrato 3",
                                            "Estrato 4", "Estrato 5", "Estrato 6"))
    

# relab: 1=Obrero..., 2=Obrero..., etc.
datos_sub$relab <- factor(datos_sub$relab,
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),

    # relab: 1=Obrero..., 2=Obrero..., etc.
    datos_sub$relab <- factor(datos_sub$relab,
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                              labels = c("Obrero/emp. empresa particular",
                                         "Obrero/emp. gobierno",
                                         "Empleado doméstico",
                                         "Trabajador cuenta propia",
                                         "Patrón o empleador",
                                         "Trabajador familiar sin remuneración",
                                         "Trabajador sin remuneracionempresas/negocios de otros hogares",
                                         "Otro"))
    
# formal: 1=formal (seguridad social), 0=otro
datos_sub$formal <- factor(datos_sub$formal,
                               levels = c(0, 1),
                               labels = c("Otro", "Formal (seguridad social)"))
    
# sizeFirm: 1= self-employed, 2= 2-5 trabajadores, etc.
datos_sub$sizeFirm <- factor(datos_sub$sizeFirm,
                                 levels = c(1, 2, 3, 4, 5),
                                 labels = c("Self-employed",
                                            "2-5 trabajadores",
                                            "6-10 trabajadores",
                                            "11-50 trabajadores",
                                            ">50 trabajadores"))
    
# Head_Female: 1=Jefe de hogar mujer, 0=Otro
datos_sub$Head_Female <- factor(datos_sub$Head_Female,
                                    levels = c(0, 1),
                                    labels = c("Otro", "Jefe de hogar mujer"))
    
# maxEducLevel_im: 1=Ninguno, 2=Preescolar, etc.
datos_sub$maxEducLevel_im <- factor(datos_sub$maxEducLevel_im,
                                        levels = c(1, 2, 3, 4, 5, 6, 7, 9),
                                        labels = c("Ninguno",
                                                   "Preescolar",
                                                   "Primaria incompleta (1-4)",
                                                   "Primaria completa (5)",
                                                   "Secundaria incompleta (6-10)",
                                                   "Secundaria completa (11)",
                                                   "Terciario",
                                                   "N/A"))
    
# cotPension_im: 1=cotiza pension, 2=no cotiza, etc.
datos_sub$cotPension_im <- factor(datos_sub$cotPension_im,
                                      levels = c(1, 2, 3, 9),
                                      labels = c("Cotiza pensión",
                                                 "No cotiza pensión",
                                                 "Pensionado",
                                                 "N/A"))
    
##grafico edad 
    
library(ggplot2)
    
p <- ggplot(datos_sub, aes(x = "", y = age)) +
      geom_boxplot(fill = "skyblue", outlier.shape = 16, outlier.alpha = 0.6) +
      scale_y_continuous(limits = c(min(datos_sub$age, na.rm = TRUE), 
                                    max(datos_sub$age, na.rm = TRUE))) +
      labs(
        title = "Boxplot de Edad",
        x = "",
        y = "Edad"
      ) +
      theme_minimal()
    
print(p)
    
ggsave("boxplot_edad.pdf", plot = p, device = "pdf", width = 6, height = 4)
    
    
#edad y sexo 
boxplot(age ~ factor(gender, levels = c(1, 0), labels = c("Hombre", "Mujer")),
            data = datos,
            xlab = "Género",
            ylab = "Edad (años)")
    
    
#ingreso por sexo 
boxplot(y_ingLab_m_ha_wins ~ factor(gender, levels = c(1, 0), labels = c("Hombre", "Mujer")),
            data = datos,
            xlab = "Género",
            ylab = "Ingreso laboral por hora(log)",
            log = "y")
    
    
#ingreso, edad, sexo 
    
# Definir los cortes: de 18 a 22, 23 a 27, … hasta 93-94 (recordar que la edad máxima es 94)
breaks <- c(18, seq(23, 93, by = 5), 95)
labels_age <- paste(breaks[-length(breaks)], breaks[-1] - 1, sep = "-")
  
# Crear la variable categórica para edad
datos$age_cat <- cut(datos$age,
                         breaks = breaks,
                         right = FALSE,
                         labels = labels_age)
    
# Gráfico: Boxplot del ingreso laboral por hora (escala log10) por categoría de edad y género
ggplot(datos, aes(x = age_cat, y = y_ingLab_m_ha_wins, 
                      fill = factor(gender, levels = c(1, 0), labels = c("Hombre", "Mujer")))) +
      geom_boxplot() +
      scale_y_continuous(trans = "log10") +
      labs(x = "Edad (quinquenios)",
           y = "Ingreso laboral por hora (log10)",
           fill = "Género") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank())
    
#----------------------------------------------------  
##sexo y sizefirm  REVISARR!!!!!!!!!!
    
# Agrupar y calcular el ingreso laboral promedio por sexo y formalidad
tabla_sizefirm <- datos %>%
      group_by(gender, sizeFirm) %>%
      summarise(ingreso_mean = mean(y_ingLab_m_ha_wins, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        gender_label = factor(gender, levels = c(1, 0), labels = c("Hombre", "Mujer")),
        formal_label = factor(sizeFirm, levels = c(1, 0), labels = c("Formal", "Informal"))
      )
    
# Gráfico de barras agrupado
ggplot(tabla_formalidad, aes(x = formal_label, y = ingreso_mean, fill = gender_label)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Formalidad",
           y = "Ingreso laboral promedio",
           title = "Ingreso laboral promedio según Sexo y Formalidad",
           fill = "Género") +
      theme_minimal()
#----------------------------------------------------  
    
# Tabla por gender
tabla_gender <- datos_sub %>%
    group_by(gender) %>%
    summarise(
    edad_mean = mean(age, na.rm = TRUE),
    edad_median = median(age, na.rm = TRUE),
    ingreso_lab_hora_mean = mean(y_ingLab_m_ha_wins, na.rm = TRUE),
    ingreso_lab_hora_mediana = median(y_ingLab_m_ha_wins, na.rm = TRUE)
      )
print(tabla_gender)
print(xtable(tabla_gender), file = "tabla_gender.tex", type = "latex")
  
    
# Tabla por nivel educativo
tabla_nivel_educativo <- datos_sub %>%
    group_by(maxEducLevel_im) %>%
    summarise(
    ingreso_lab_hora_mean = mean(y_ingLab_m_ha_wins, na.rm = TRUE),
    ingreso_lab_hora_mediana = median(y_ingLab_m_ha_wins, na.rm = TRUE)
      )
print(tabla_nivel_educativo)
print(xtable(tabla_nivel_educativo), file = "tabla_nivel_educativo.tex", type = "latex")
    
# Tabla por estrato1
tabla_estrato <- datos_sub %>%
    group_by(estrato1) %>%
    summarise(
    ingreso_lab_hora_mean = mean(y_ingLab_m_ha_wins, na.rm = TRUE),
    ingreso_lab_hora_mediana = median(y_ingLab_m_ha_wins, na.rm = TRUE)
    )
print(tabla_estrato)
print(xtable(tabla_estrato), file = "tabla_estrato.tex", type = "latex")
  
    
#edad vs sexo 
    
boxplot(edad~sexo,
            datos,
            xlab="Sexo",
            ylab="Edad (anos)", 
            xaxt="n")
axis(1, at=1:2,
         labels=c("Hombre","Mujer"))
    
#tabla ingreso, edad, sexo 
    
# Definir cortes y etiquetas para los grupos de edad a partir de 18 años
breaks <- c(18, seq(23, 95, by = 5), 95)
labels_age <- paste(breaks[-length(breaks)], breaks[-1] - 1, sep = "-")
    
# Crear las variables de grupos de edad y de sexo con etiquetas
datos <- datos %>%
mutate(age_cat = cut(age, 
                           breaks = breaks, 
                           right = FALSE, 
                           labels = labels_age),
             gender_label = factor(gender, levels = c(1, 0), 
                                   labels = c("Hombre", "Mujer")))
    
# Generar la tabla de ingreso laboral (promedio y mediana) según grupo de edad por sexo
tabla_ingreso <- datos %>%
      group_by(age_cat, gender_label) %>%
      summarise(
      ingreso_lab_hora_mean = mean(y_ingLab_m_ha_wins, na.rm = TRUE),
      ingreso_lab_hora_mediana = median(y_ingLab_m_ha_wins, na.rm = TRUE)
      ) %>%
      ungroup()
    
# Imprimir la tabla en la consola
print(tabla_ingreso)
    
# Exportar la tabla a un archivo LaTeX para Overleaf
print(xtable(tabla_ingreso), file = "tabla_ingreso.tex", type = "latex")
    
    
#########################################################    
    ###Resumen descriptivo de variables 
#########################################################
  
    
# estadísticas de cada variable numérica
numeric_summary <- datos_sub %>%
    select_at(numeric_vars) %>%     # Seleccionar las columnas definidas en numeric_vars
    pivot_longer(cols = everything(), 
                   names_to = "Variable", 
                   values_to = "Valor") %>% 
      
#etiquetas de las variables 
mutate(Variable = dplyr::recode(Variable,
                                      "age" = "Edad (años)",
                                      "nmenores" = "Número de menores",
                                      "y_ingLab_m_ha_wins" = "Salario por Hora(todas las ocupaciones)"
      )) %>%
      group_by(Variable) %>% 
      summarise(
        n      = n(),
        Mean   = mean(Valor, na.rm = TRUE),
        SD     = sd(Valor, na.rm = TRUE),
        Min    = min(Valor, na.rm = TRUE),
        Median = median(Valor, na.rm = TRUE),
        Max    = max(Valor, na.rm = TRUE),
        .groups = "drop"
      )
    
# Visualizar en la consola (como data frame)
numeric_summary
  numeric_summary %>% 
  kable(format = "pandoc")
    
# Generar la salida en formato LaTeX (para Overleaf)
numeric_summary %>% 
    kable(format = "latex", booktabs = TRUE,
    caption = "Resumen de Variables Numéricas") %>% 
    kable_styling(latex_options = c("striped", "hold_position"))
    

var_order <- c("gender", "maxEducLevel_im", "estrato1", "cotPension_im", "sizeFirm")
    
# resumen con Frecuencia y %
    
categorical_summary <- datos_sub %>%
# Selecciona las columnas definidas en categorical_vars
select_at(categorical_vars) %>% 
      pivot_longer(cols = everything(), 
      names_to = "Variable", 
      values_to = "Categoria") %>%
      group_by(Variable, Categoria) %>%
      summarise(Frecuencia = n(), .groups = "drop") %>%
      group_by(Variable) %>%
      mutate(
        Porcentaje = paste0(
          round(100 * Frecuencia / sum(Frecuencia), 1), "%"
        )
      ) %>%
      ungroup() %>%
      # Convertir Variable a factor con el orden especificado
      mutate(Variable = factor(Variable, levels = var_order)) %>%
      # Ordenar primero por Variable (en el orden de var_order) 
      # y luego descendentemente por Frecuencia
      arrange(Variable, desc(Frecuencia))
    
 #Visualizar la tabla en la consola
    
 # a) Como data frame
 categorical_summary
    
 # b) Como tabla Markdown (en la consola)
 categorical_summary %>% 
 kable(format = "pandoc")
    
 # Generar la salida en formato LaTeX
    
 categorical_summary %>%
     kable(format = "latex", booktabs = TRUE,
     caption = "Resumen de Variables Categóricas (con Etiquetas y %)") %>%
     kable_styling(latex_options = c("striped", "hold_position"))
    



 ##Resumen de Estadisticas descriptivas variables modelo   
    
 vars_modelo <- c("y_ingLab_m_ha_wins", "nmenores", "age", 
                                 "gender", "estrato1", "sizeFirm", "maxEducLevel_im", "oficio")
    descriptivas <- datos[,  vars_modelo]
    resumen <- dfSummary(descriptivas, style = "grid", plain.ascii = FALSE)
    print(resumen, method = "browser")   
    
## --- Tratamniento Missing Values --- ##

# -- NA / Missing Values - 2 aproximaciones -- #
is.na(datos$y_ingLab_m_ha)

# 1. Eliminamos NA
datos1 <- datos %>% filter(!is.na(y_ingLab_m_ha))


# 2. Reemplazamos NA por el valor medio

m_y_ingLab_m_ha <- mean(datos$y_ingLab_m_ha, na.rm = TRUE)
datos2 <- datos %>%  mutate(y_ingLab_m_ha = replace_na(y_ingLab_m_ha, m_y_ingLab_m_ha)) 

# Eliminamos NA de maxEducLevel
datos2 <- datos2 %>% filter(!is.na(maxEducLevel))


# Revisión rápida de los datos

skim(datos1)
skim(datos2)

## TRABAJAR CON DATOS SUB



# ------------------------------------------------------------- #
## ------------------------- PUNTO 3 ------------------------- ##
# ------------------------------------------------------------- #

## Creamos las variables
datos1 <- datos%>% mutate(log_s2 = log(y_ingLab_m_ha_mean))
datos2 <- datos%>% mutate(log_s2 = log(y_ingLab_m_ha_wins))

## La regresión 
reg_p3 <- log_s2 ~ age + I(age^2)
modelo_p3_mean <- lm(reg_p3, data = datos1)
modelo_p3_median <- lm(reg_p3, data = datos2)

#Generacion de la tabla 
stargazer(modelo_p3_mean, modelo_p3_median, type = "text", title = "Logaritmo del salario en funcion de la edad")

# Sacando los coeficientes
modelo_p3_mean$coefficients
modelo_p3_median$coefficients

## Revisando el ajuste intramodelo
log_s_hat_mean  <- predict(modelo_p3_mean, newdata = datos1)
log_s_hat_median <- predict(modelo_p3_median, newdata = datos2)
print(c(MSE_p3_mean = mean((datos1$log_s2 - log_s_hat_mean)^2), MSE_p3_median = mean((datos2$log_s2 - log_s_hat_median)^2)))
# Nos quedamos con la mediana

## Gráfica - Punto más alto de ingreso estimado según edad
ggplot(datos2, aes(x = age, y = log_s_hat_median)) +
  geom_point(color = "blue", alpha = 0.6) +  # Puntos en azul con transparencia
  geom_vline(xintercept = 50, color = "red", linetype = "dashed", size = 1) +  # Línea vertical roja en x = 50
  # geom_smooth(method = "lm", color = "red", se = TRUE) +  # Línea de tendencia opcional
  labs(title = "Relación entre Edad y el Logaritmo de los Ingresos",
       x = "Edad",
       y = "Log(Ingresos)") +
  theme_minimal()

## Calculamos con bootstrap el valor máximo de los ingresos.
# Primero creamos la función 
peak_age_f2<-function(datos2,index){
  
  modelo_p3 <- lm(log_s2 ~ age + I(age^2), data = datos2, subset = index)
  
  
  b2s <- coef(modelo_p3)[2]
  b3s <- coef(modelo_p3)[3]
  
  age_max_s <- -b2s/(2*b3s)  #Esto sale de derivar la ecuacion e igualar a 0 en base a los coeficientes estimados
  
  return(age_max_s)
}

peak_age_f2(datos2,1:nrow(datos2))  #Probando la funcion 

##Finalmente hacemos la simulación
set.seed(10101)
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
set.seed(10101)
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
set.seed(10101)
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
set.seed(10101)
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
# MODELOS ANTERIORES
modelo1 <- lm(reg_p3, data = train)
modelo2 <- lm(reg_p4, data = train)
modelo3 <- lm(reg_p4_controles, data = train)

# MODELOS ADICIONALES
modelo4 <- lm(log_s2 ~ age + I(age^2) + female, data = train)
modelo5 <- lm(log_s2 ~ age + I(age^2) + female + (age * female), data = train) # interacción female x age
modelo6 <- lm(log_s2 ~ age + I(age^2) + female + (age * female) + maxEducLevel_im + oficio + estrato1, data = train) # controles principales
modelo7 <- lm(log_s2 ~ age + I(age^2) + female + (age * female) + maxEducLevel_im + (maxEducLevel_im * age) + oficio + estrato1 + nmenores + sizeFirm, data = train) # controles secundarios
modelo8 <- lm(log_s2 ~ age + I(age^2) + female + (age * female) + maxEducLevel_im + I(maxEducLevel_im^2) + (maxEducLevel_im * age) + oficio + estrato1 + nmenores + (nmenores * age) + sizeFirm, data = train) # interacción y forma polinomial 

# Performance (fuera de muestra)
predictions <- list(
  pred1 = predict(modelo1, test),
  pred2 = predict(modelo2, test),
  pred3 = predict(modelo3, test),
  pred4 = predict(modelo4, test),
  pred5 = predict(modelo5, test),
  pred6 = predict(modelo6, test),
  pred7 = predict(modelo7, test),
  pred8 = predict(modelo8, test)
)

# Calcular RMSE sin volver a aplicar `predict()`
performance <- tibble(
  modelo = names(predictions),
  RMSE = map_dbl(predictions, ~ round(RMSE(.x, test$log_s2), 5))
)

performance <- performance %>%
  mutate(RMSE = format(RMSE, nsmall = 5))

performance

# Identificamos el modelo con menor RMSE
sorted_indices <- order(performance$RMSE)
best_model1 <- performance$modelo[sorted_indices[1]]
best_model1
best_model2 <- performance$modelo[sorted_indices[2]]
best_model2

# Extraemos los errores de predicción
best_predictions <- predictions[[best_model1]]
test$prediction_error <- test$log_s2 - best_predictions
summary(test$prediction_error)

# Visualización
# Histograma de los errores
ggplot(test, aes(x = prediction_error)) +
  geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = mean(test$prediction_error), color = "red", linetype = "dashed") +
  labs(title = "Distribución de Errores de Predicción", x = "Error", y = "Frecuencia")

# Boxplot para detectar outliers
ggplot(test, aes(y = prediction_error)) +
  geom_boxplot(fill = "orange", alpha = 0.6) +
  labs(title = "Boxplot de Errores de Predicción", y = "Error")

## ------------------- ##
## ---- Punto 5.3 ---- ## 
## ------------------- ##
form_8 <- log_s2 ~ age + I(age^2) + female + (age * female) + maxEducLevel_im + I(maxEducLevel_im^2) + (maxEducLevel_im * age) + oficio + estrato1 + nmenores + (nmenores * age) + sizeFirm
form_7 <- log_s2 ~ age + I(age^2) + female + (age * female) + maxEducLevel_im + (maxEducLevel_im * age) + oficio + estrato1 + nmenores + sizeFirm

# Entrenamos los modelos - Metodología LOOCV
ctrl <- trainControl(
  method = "LOOCV") ## input the method Leave One Out Cross Validation

# Start timing
start_time <- Sys.time()

# Get total number of observations for progress tracking
n_obs <- nrow(datos2)
cat("Starting LOOCV training with", n_obs, "iterations...\n")

# Train model with progress printing
ctrl$verboseIter <- TRUE  # Enable progress printing
modelo1_LOOCV <- train(form_8,
                  data = datos2,
                  method = 'lm', 
                  trControl = ctrl)

modelo2_LOOCV <- train(form_7,
                       data = datos2,
                       method = 'lm', 
                       trControl = ctrl)

# Vemos el performance
perf1_LOOCV <- RMSE(modelo1_LOOCV$pred$pred, datos2$log_s2)
perf1_LOOCV

perf2_LOOCV <- RMSE(modelo2_LOOCV$pred$pred, datos2$log_s2)
perf2_LOOCV
