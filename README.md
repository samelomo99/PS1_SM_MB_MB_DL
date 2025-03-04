# PS1_SM_MB_MB_DL
## Taller 1 - Problem Set 1: Predicting Income  

### Santiago Melo, Miguel Blanco, María Bernal, Diana Lopera  

---

## 📂 Estructura del repositorio PS1_SM_MB_MB_DL  

El repositorio está organizado en las siguientes carpetas:  

### 📄 `document`  
Contiene el documento final en formato PDF del Problem Set 1. Más adelante se puede observar un resumen del ejercicio realizado, así como los principales resultados obtenidos.  

### 📜 `scripts`  
Contiene dos scripts en R:  
1. **Web Scraping**: Automatiza la extracción de los 10 *chunks* encontrados en la página web: [GEIH2018 Sample](https://ignaciomsarmiento.github.io/GEIH2018_sample/). En esta misma página se encuentra el diccionario de datos y algunas etiquetas de interés.  
2. **Análisis y Modelado**: Incluye la descripción de datos, regresiones de los modelos, procesos de *Bootstrap* y métodos de validación como *Validation Set Approach* y *LOOCV*.  

### 📊 `stores`  
Contiene la base de datos construida a partir del *scraping* de la GEIH.  
- Cada *chunk* fue almacenado en un archivo CSV individual.  
- Finalmente, se realizó el *merge* de las 10 bases generando un único conjunto de datos.  

### 📈 `views`  
Almacena los cuadros y gráficas en orden según su aparición en el documento principal.  

- **Gráficas**: Se encuentran en formatos JPG y PDF, lo que permite su inclusión en Overleaf sin inconvenientes.  
- **Cuadros**: Guardados en formato *text*, compatible con Overleaf. Los cuadros a partir del número 18 corresponden a la sección de Anexos.  

---

## 📌 Resumen del ejercicio  

Este proyecto se basa en datos de la **Gran Encuesta Integrada de Hogares (GEIH) de 2018**, recolectada por el **Departamento Administrativo Nacional de Estadística (DANE)**. El objetivo principal es construir un modelo de predicción de **salarios individuales por hora** en Bogotá, analizando la relación entre la edad y el salario, así como la brecha salarial de género.  

Para ello, se emplearon técnicas de validación como **validación cruzada** y se utilizaron métodos de **web scraping con ChromoteSession** para obtener y procesar los datos. La base de datos resultante incluye información sobre **características sociodemográficas, educación, empleo formal e informal, cotización a seguridad social y otras variables clave**.  

### 🔍 Principales hallazgos  

- **Alta dispersión en los ingresos laborales**: Se observa una gran diferencia entre la media y la mediana de los ingresos, lo que sugiere la presencia de valores atípicos y una distribución desigual.  
- **Factores determinantes del ingreso**: Se identificaron relaciones clave entre el salario y variables como **nivel educativo, estrato socioeconómico y tamaño de la empresa**.  
- **Relación edad-salario**: Se confirma una relación cuadrática, donde los ingresos aumentan con la edad hasta cierto punto antes de estabilizarse o disminuir.  
- **Brecha salarial de género**: Se exploraron distintas metodologías para evaluar esta brecha y se aplicaron técnicas de *Bootstrap* para estimar la edad pico de ingresos.  
- **Evaluación del rendimiento predictivo**: Se comparó el desempeño de varios modelos mediante el **Error Cuadrático Medio (RMSE)**, utilizando enfoques como *Validation Set Approach* y *Leave-One-Out Cross-Validation (LOOCV)*.  

Los resultados indican que la **inclusión progresiva de variables explicativas mejora la precisión del modelo**, siendo el modelo más complejo el que presenta el menor error de predicción.  

---

📌 **Este repositorio corresponde a un ejercicio de análisis y predicción de ingresos, aplicando técnicas aprendidas durante la clase de Big Data y Machine Learning de la Universidad de los Andes.**  



