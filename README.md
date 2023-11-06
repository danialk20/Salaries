# README

## Descripción General

Este repositorio tiene los elementos usados en el desarrollo de la aplicación web "Ciencia de datos: salarios bajo la lupa", en la cuál se presenta al usuario el análisis descriptivo y predictivo del salario de los profesionales en ciencia de datos de algunas partes del mundo. 

## Componentes

1. **Base de Datos Original:**
   - La base de datos original es un archivo CSV llamado `original_data.csv`, que contiene los datos obtenidos de Kaggle y originados de aijobs.net que pertenecen a Randomarnab.


2. **Archivo de Tratamiento:**
   - El archivo `treatment_file.R` presenta el código del preprocesamiento aplicado al conjunto de datos original. Esto incluye la limpieza de datos y cualquier transformación necesaria para preparar los datos para la creación de la aplicación web.

3. **Datos Procesados:**
   - El archivo `shiny_data.csv` representa nuestra versión curada del conjunto de datos después de aplicar el tratamiento descrito en el script `treatment_file.R`. Estos datos refinados se utilizan en nuestra aplicación Shiny para visualización y análisis interactivos.

4. **Datos de apoyo:**
   - El archivo `data_wc.csv` presenta los datos usados para la elaboración de la nube de palabras expuesta en la página principal de la aplicación web.

## Aplicación Shiny

En el folder `shiny_app` se encontrarán los archivos `global.R`, `server.R` y `ui.R`, esenciales en la creación de la app shiny, y un archivo `tema.css`, en el que se definen algunas características estéticas de la misma.

## Colaboradores

- Daniela Alcázar
  
- Esteffany Peña
  
- Juliana Rueda

Si tienes alguna pregunta o inquietud, no dudes en contactarnos. Estamos aquí para ayudarte.

¡Feliz análisis!
