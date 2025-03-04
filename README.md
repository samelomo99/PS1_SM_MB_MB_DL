# PS1_SM_MB_MB_DL
 Taller 1 - Problem Set 1: Predicting Income

This is the template repository for the problem sets.

The repo should contain at least four folders:

- `document`: contains your final document in `pdf` format. Ideally, the document should pull figures and tables directly from the `views` folder. I've included a latex template I created for the Thesis Seminar. 
- `scripts`: contains all your scripts
- `stores`: contains all the data sets used. If files are "too big" for GitHub, include a document describing where people can access the data.
- `views`: contains all figures and tables

## Resumen del ejercicio

Este proyecto se basa en datos de la Gran Encuesta Integrada de Hogares (GEIH) de 2018, recolectada por el Departamento Administrativo Nacional de Estadística (DANE). El objetivo principal es construir un modelo de predicción de salarios individuales por hora en Bogotá, analizando la relación entre la edad y el salario, así como la brecha salarial de género.  

Para ello, se emplearon técnicas de validación como la validación cruzada y se utilizaron métodos de web scraping con *ChromoteSession* para obtener y procesar los datos. La base de datos resultante incluye información sobre características sociodemográficas, educación, empleo formal e informal, cotización a seguridad social y otras variables clave.  

Los análisis descriptivos evidencian una alta dispersión en los ingresos laborales, con una notable diferencia entre la media y la mediana, lo que sugiere la presencia de valores atípicos y una distribución desigual. También se identificaron relaciones clave entre el ingreso y factores como el nivel educativo, el estrato socioeconómico y el tamaño de la empresa.  

Los modelos de predicción construidos revelan una relación cuadrática entre edad y salario, donde los ingresos aumentan con la edad hasta cierto punto antes de estabilizarse o disminuir. Además, se analizaron distintas metodologías para evaluar la brecha salarial de género y se aplicaron técnicas de *Bootstrap* para estimar la edad pico de ingresos.  

Finalmente, se comparó el rendimiento predictivo de varios modelos mediante el Error Cuadrático Medio (RMSE), utilizando aproximaciones como *Validation Set Approach* y *Leave-One-Out Cross-Validation (LOOCV)*. Los resultados indican que la inclusión progresiva de variables explicativas mejora la precisión del modelo, siendo el modelo más complejo el que presenta el menor error de predicción.  



