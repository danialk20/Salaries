library(readr)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(ggtext)
library(plotly)
library(ggridges)
library(wordcloud2)
library(scales)
library(DT)
library(gt)
library(gtsummary)
library(rlang)
library(leaflet)
library(sf)
library(readxl)
library(car)
library(jtools)
library(tseries)
library(goftest)
library(lmtest)
library(Metrics)

DataMundo <- st_read("/vsicurl/https://github.com/EsteffanyP/RDay/raw/main/TM_WORLD_BORDERS_SIMPL-0.3.shp")

url0 <- "https://raw.githubusercontent.com/danialk20/Salarios_CD/main/original_data.csv"
data0 <- read_csv(url0, show_col_types = FALSE)
DataMundo <- st_read("TM_WORLD_BORDERS_SIMPL-0.3.shp")
colF <- c("work_year", "experience_level", "employment_type", "job_title", "salary_currency", "employee_residence", "company_location", "company_size", "remote_ratio")
data0[colF] <- lapply(data0[colF], as.factor)
colN <- c("salary", "salary_in_usd")
data0[colN] <- lapply(data0[colN], as.numeric)

url1 <- "https://raw.githubusercontent.com/danialk20/Salarios_CD/main/shiny_data.csv"
data <- read_csv(url1, show_col_types = FALSE) %>%
  mutate(across(c(where(is.character), "Año"), as.factor))


url2 <- "https://raw.githubusercontent.com/danialk20/Salarios_CD/main/data_wc.csv"
data_wc <- read_csv(url2, show_col_types = FALSE) %>%
  mutate(across(everything(), as.factor))

# Select input - Every variable
c1 <- data %>%
  names()

# Select input - Factor (all - salary)
c2 <- data %>%
  select(-6) %>%
  names()

# Select input - Numeric (salary)
c3 <- data %>%
  select(6) %>%
  names()

# pie or bar
c4 <- c("Bar Plot", "Pie Plot")

# pie or bar
c5 <- c("Box Plot", "Joy Plot")

# niveles de variables
c6 <- c("2020","2021","2022","2023")

# filtro data
c7 <- c("Todos", "Máximo", "Mínimo")

# mapa
c8 <- data %>%
  select(c(7,9)) %>%
  names()

# normalidad

# textos instrucciones
txtinstrucciones <- list()
txtinstrucciones$univariado <- "En el panel izquierdo puedes elegir una variable y el tipo de gráfico que deseas visualizar. En la sección central, verás las gráficas seleccionadas junto con su análisis correspondiente. Recuerda que aquí se analiza una sola variable, es decir, se aprecia la distribución de los datos con base en los niveles de la variable elegida."
txtinstrucciones$categoria <- "En esta sección podrás analizar el salario promedio de los profesionales en ciencia de datos con respecto a los niveles de otra variable. En el panel izquierdo puedes elegir la variable con la que deseas comparar los salarios y seleccionar el tipo de gráfico de tu preferencia. En la sección central encontrarás las gráficas seleccionadas con su respectivo análisis."
txtinstrucciones$facet <- "En la parte izquierda encontrarás un panel donde podrás seleccionar las dos variables con la que va a ser comparado el salario. En la parte central aparecerán la gráfica con su respectivo análisis. En este análisis podrás observar el salario promedio para combinaciones de los niveles de dos variables."
txtinstrucciones$spider <- "Esta gráfica interactiva en forma de telaraña permite analizar los promedios con relación al rol que desempeña el profesional y el nivel de experiencia."
txtinstrucciones$mapa <- "En esta pestaña puedes observar un mapa coroplético que representa el salario promedio según la residencia del profesional en ciencia de datos o de la ubicación de la empresa en la que trabaja"
txtinstrucciones$datatable <- "Aquí puedes interactuar con la base de datos usada. Puedes filtrarla para obtener las observaciones según el nivel de alguna variable y visualizar el salario máximo o mínimo. Los datos proyectados pueden ser descargados en cualquier momento."
txtinstrucciones$summary <- "A continuación un resumen de la base de datos donde podrás observar la distribución de las entradas de la base de datos por variable y por nivel"

# textos análisis gráficas univariado
txtunivariado <- list()
txtunivariado$anio <- "La mayoría de las entradas en la base de datos corresponden a los años 2023 y 2022. Se destaca una tendencia hacia una mayor concentración de observaciones en los años más recientes, lo que resalta la actualidad de los datos y las estadísticas proporcionadas. 2023 es el año que predomina, con un total de 1747 observaciones que representan el 47,3% del total de los datos; seguido de 2022 con 44,5% (1644 observaciones); 6% (228 observaciones) del año 2021 y el 2% (73 observaciones) pertenecen al año 2020."
txtunivariado$experiencia <- "La mayor parte de los profesionales en ciencia de datos están clasificados con nivel de experiencia Senior, con 2473 observaciones que representan el 67% de los datos. Seguido de este nivel están los profesionales con un nivel de experiencia intermedio, con 800 observaciones que equivalen al 21.7%. En tercer lugar los profesionales principiantes (denominados comúnmente Junior), que solo representan el 8.6% con un total de 319 observaciones. Por último, con tan solo 100 observaciones (y un 2.7% de representación) se encuentran aquellos profesionales que se desempeñan en un cargo de nivel ejecutivo."
txtunivariado$contrato <- "Pese a las tendencias de contratos donde se tiene la concepción de que los científicos de datos trabajan como independientes, los análisis evidencian que predominan los contratos a tiempo completo y que los tipos de contrato a medio tiempo, término fijo, o contrato de independientes no son tan comunes, pues estas tres categorías juntas representan aproximadamente 1%, con tan solo 36 observaciones."
txtunivariado$rol <- "Hay una gran variedad de roles que son englobados por la ciencia de datos. Se presentan los más frecuentes y se destaca a los Ingenieros de datos con 1024 profesionales del total de la muestra, los cuales representan el 28%. Los Científicos de datos representan un 22 y los Analistas un 17%. Cabe resaltar que la categoría “Otro” tiene un número considerable de observaciones debido a que en ella están agrupados 86 roles que no fueron presentados de manera explícita como una sola categoría."
txtunivariado$moneda <- "No  es sorpresa que más del 85% de los profesionales en ciencia de datos reciban el pago de su salario en dólares estadounidenses, ya que esta es la moneda más popular en todo el mundo. Por la presencia de países europeos que se pudo apreciar tanto en el análisis de la residencia del profesional como de la ubicación de la empresa, es entendible la razón de que el euro y la libra esterlina hagan su aparición en el segundo y tercer puesto con 6.4% y 4.3% de los datos, respectivamente. Se destaca que solo 9 profesionales reciben su salario en dólar australiano (país que no está entre los más frecuentes de residencia del profesional o de ubicación de la empresa), lo que demuestra que hay un resaltable acuerdo por parte de los empleadores sobre la moneda de pago de los salarios."
txtunivariado$residencia <- "Los científicos de datos se pueden encontrar en todo el mundo, sin embargo, se destaca el papel de Estados Unidos como referente para otros países. En cuanto a Europa, en Reino Unido residen 166, en España 80 y en Alemania 48, que es un 9.7% de los que se encuentran en América del norte. El continente asiático es uno de los que menor residencia de profesionales tiene, siendo India un 1% del total de la muestra. Estos datos son importantes, pues así se puede evidenciar la calidad formativa en cada país, la inversión y el apoyo al sector."
txtunivariado$modalidad <- "Se puede evidenciar que, pese a las tendencias de trabajo remoto resultado de la pandemia, el trabajo presencial es de los más requeridos en el mercado (con un 51.2% de participación), pues a las empresas generalmente les gusta monitorear el trabajo de los profesionales y trabajar bajo conceptos de equipo más dinamizados en la presencialidad. Sin embargo, no sobra destacar que la modalidad remota, a pesar de ser una tendencia emergente, es notoriamente popular en el área. La modalidad híbrida o parcialmente remota es muy poco común, pues solo un 5% del total de los datos trabajó simultáneamente de manera remota y presencial."
txtunivariado$ubicacion <- "Aunque un profesional en ciencia de datos trabaje de manera remota, parcialmente remota o presencial, la ubicación de la empresa empleadora puede cambiar. Estados Unidos se atribuye que aproximadamente el 80.7% profesionales están trabajando para alguna empresa radicada en el país (esto es, 2980 observaciones en la base de datos). Los países de Europa, a pesar de que están presentes, no destacan mucho, por lo que puede inferirse que las empresas europeas no son la prioridad a la hora de buscar un empleo en el área."
txtunivariado$tamanio <- "El 12% de los profesionales en ciencia de datos trabajan en empresas grandes, mientras que el 84% trabajan en empresas medianas, hecho que puede deberse no solo a un interés de este tipo de empresas por los profesionales del área, sino también a que las empresas medianas son las más comunes en la industria. Por otro lado, solo cerca del 4% de los profesionales son contratados por empresas pequeñas, esto puede deberse al poco mercado que abarcan, los pocos datos que pueden tener o el desconocimiento de la importancia de estos profesionales."

# textos análisis multivariado, salarios por categoría
txtcategoria_box <- list()
txtcategoria_box$anio <- "Para todos los años, los salarios aparentan seguir una distribución simétrica, evidenciada por la mediana centrada en la respectiva caja de bigotes. Aunque se presentan algunos datos atípicos en el año 2021 y únicamente dos en 2020, la mayoría de los datos se sitúan dentro de los límites de los bigotes en cada periodo. Los límites superiores e inferiores de los bigotes varían de la siguiente manera: 190200 y 5707 dólares para 2020, 240000 y 5409 dólares para 2021, 275000 y 5132 dólares para el año 2022, y 293000 y 7000 dólares en el año 2023. Es notable que los límites inferiores no experimenten un aumento durante el periodo observado, mientras que los límites superiores aumentan con el tiempo, sugiriendo una creciente disposición de las empresas a ofrecer salarios más elevados."
txtcategoria_box$experiencia <- "Se aprecia una marcada diferencia significativa entre los salarios de los profesionales de ciencia de datos según su nivel de experiencia. Los datos no siguen distribuciones simétricas, mostrando pequeñas desviaciones de la mediana en la caja de cada nivel. Destaca la presencia y variación de valores atípicos en el nivel intermedio, donde el límite superior de la caja alcanza los 230000 dólares y el salario más alto ofrecido a un profesional de este nivel es de 280700 dólares. Además, se nota que los profesionales de nivel ejecutivo no perciben salarios inferiores a 68741 dólares (exceptuando el valor atípico de 15000 dólares), un mínimo significativamente más alto en comparación con los salarios mínimos de los profesionales de nivel intermedio, junior y avanzado, que son de 5132, 5409 y 8000 dólares, respectivamente."
txtcategoria_box$contrato <- "El valor de los salarios segmentados por tipo de contrato no exhibe diferencias significativas entre empleados a medio tiempo, independientes o contratados a término fijo. Los salarios de profesionales contratados a tiempo completo siguen una distribución normal y presentan la mayor variabilidad, como indica la diferencia entre los bigotes correspondientes al tipo de contrato mencionado. Los límites inferiores de cada boxplot por tipo de contrato sugieren que, en el caso de trabajadores independientes, el salario mínimo es más elevado en comparación con las otras categorías, siendo este de 12000 dólares, en contraste con los salarios mínimos de 7500, 5132 y 5409 dólares para las clasificaciones de contrato a término fijo, tiempo completo y medio tiempo, respectivamente."
txtcategoria_box$rol <- "En términos generales, no se observan diferencias significativas entre los salarios de los distintos roles profesionales de ciencia de datos. Los salarios varían de manera similar para roles como ingeniero de datos, científico de datos e ingeniero de machine learning. Sin embargo, destaca que entre los ingenieros analíticos y los arquitectos de datos, el salario mínimo es más alto que en el resto de los roles presentes en la base de datos, con valores mínimos de 48000 y 63000 dólares, respectivamente."
txtcategoria_box$moneda <- "Aunque el proyecto se enfoca en los salarios de los profesionales en USD, debido a que es la moneda con mayor muestra, es importante recalcar otras monedas como el dólar australiano, canadiense y el euro. La rupia india es la moneda de pago menos utilizada para la remuneración de los científicos con una mediana o centralidad de 17929, sin embargo la mayoría de los datos se encuentran sobre esta."
txtcategoria_box$residencia <- "En cuanto a la remuneración salarial dependiendo la residencia del profesional, se puede evidenciar que Estados Unidos es mucho más centralizada, a comparación de los otros países que tienen bastantes sesgos con respecto a su mediana. Se encuentran datos atípicos sobre el límite superior de los salarios en Alemania, India, España, Reino Unido, Estados Unidos y otros, lo que representa que algunos profesionales ganan más del máximo estipulado. Canadá no presenta datos atípicos por lo que un científico de datos puede ganar entre 10000 y 275000, con una mediana de 120000."
txtcategoria_box$modalidad <- "El valor de los salarios dependiendo la modalidad es muy importante, pues se evidencia una mediana centrada para la modalidad remota y presencial, con un salario muy similar. Los salarios de los profesionales que trabajan de manera presencial oscilan entren 5882 y 280000, con algunos datos atípicos. Los salarios para los que trabajan de manera remota manejan un rango entre 5132 y 290000, siendo esta modalidad en la que mayor remuneración se puede encontrar. En cuanto a la modalidad híbrida o parcialmente remora, se encuentra una mediana dónde los datos se concentran en el cuartil 3, es decir, hay sesgos y descentralización por parte de esta medida. En esta última modalidad se encuentran muchos datos atípicos que pueden llegar incluso hasta los 250000 USD de salario, teniendo en cuenta que el limite superior es de 185000."
txtcategoria_box$ubicacion <- "Se debe tener en cuenta que muchos de los profesionales no residen donde está ubicada su empresa empleadora, esto en el caso de los trabajos con modalidad remota, y otros sí se ubican en el mismo lugar. Inicialmente se observa que no hay una centralidad en común de los salarios, pues varían mucho dependiendo de la ubicación de la empresa, sin embargo, Estados unidos maneja una tendencia central acorde con poco sesgo al igual que reino unido, España y otros. Se encuentran muchos datos atípicos en Alemania, India, reino unido, otros y estado unidos, siendo este último el que más variación presenta."
txtcategoria_box$tamanio <- "Es destacable la centralidad de la mediana en el caso de una empresa mediana y grande, sin embargo, en la última se presenta mayor variación de los datos pues se concentran en la parte inferior de los salarios. En el tamaño de la empresa mediana y pequeña se encuentran algunos datos atípicos, siendo al ultima la que más posee. La mediana más descentralizada se encuentra en la empresa pequeña, donde se concentran los salarios entre 61566 y 106731, presentando una brecha considerable."

txtcategoria_joy <- list()
txtcategoria_joy$anio <- "La distribución de los salarios para los años 2022 y 2023 es simétrica con una media entre 100000 y 150000 dólares, para el caso de 2020 y 2021 se observan comportamientos asimétricos por presencia de datos atípicos. El ancho de las curvas de densidad sugiere que el año con menos variabilidad entre salarios es 2022, aunque si se omiten los datos atípicos, en 2020 los salarios de los profesionales en Ciencia de Datos varían menos."
txtcategoria_joy$experiencia <- "Ningún conjunto de salarios agrupado por nivel de experiencia se distribuye simétricamente y presenta baja variabilidad. Los salarios cerca de 150000 dólares son comunes en profesionales junior, caso similar ocurre con otros niveles, específicamente, los salarios de moda superan poco más los 50000 dólares para profesionales junior y rondan los 100000 para profesionales de nivel intermedio; en relación con profesionales ejecutivos se estima que valor de los salarios más pagados son de 150000 y 200000 dólares, aproximadamente. "
txtcategoria_joy$contrato <- "Únicamente se distribuye de forma simétrica los valores de los salarios de los trabajadores de tiempo completo, con una media que sobrepasa los 120000 dólares; sin embargo, es la distribución con más variabilidad si no se toma en cuenta el dato atípico presente en el conjunto de salarios de empleados a término fijo. Para los tipos de contratos restantes los salarios más comunes no sobrepasan los 50000 dólares a excepción de otro gran grupo de datos cercanos a 100000 dólares para el caso de contratos de término fijo."
txtcategoria_joy$rol <- "A excepción de los roles específicos de Analítico de Datos e Ingeniero de datos, las distribuciones de salarios para los puestos de trabajo considerados en la base de datos no se distribuyen simétricamente. Es común observar salarios alrededor de 150000 dólares en todos los roles, a excepción de Analista de datos, cuyo valor en ese caso es de poco más de 100000 dólares."
txtcategoria_joy$moneda <- "El valor de los salarios en dólares americanos se distribuye de manera simétrica, con una media aproximada de 140000. Por lo general, se observan salarios relativamente bajos en rupias indias y otras monedas, con ingresos inferiores a 100000 para todas las monedas, excluyendo el dólar americano."
txtcategoria_joy$residencia <- "Se evidencian distribuciones altamente asimétricas para los salarios en cada país. Estados Unidos destaca por su relativa simetría, con algunas variaciones. La mayoría de los salarios se concentran en el rango de 0 a 200,000 aproximadamente. En el caso de India, Alemania y España, los salarios promedio varían entre 20000 y 100000."
txtcategoria_joy$modalidad <- "Se observa asimetría en los salarios tanto para la modalidad presencial como remota, aunque con presencia de datos atípicos. En el caso de trabajos parcialmente remotos, la media salarial está descentralizada, es decir, los salarios para esta modalidad se concentran en los valores más inferiores"
txtcategoria_joy$ubicacion <- "La distribución de datos es asimétrica, siendo Estados Unidos la excepción con un valor promedio de 144000. Ningún país presenta una dispersión de salarios similar; Alemania muestra la mayor dispersión, seguido de Canadá, donde la mayoría de los datos se concentran en valores inferiores."
txtcategoria_joy$tamanio <- "Se observan considerables variaciones en los salarios para todos los tamaños de empresas. No obstante, la dispersión es más simétrica en las empresas medianas y grandes. Las empresas pequeñas muestran alta asimetría, con valores alejados de la mayoría de los salarios, y una tendencia central de salarios en 61566. A pesar de los datos atípicos en la empresa de tamaño mediana, parece que los salarios de los profesionales que trabajan en este tipo de empresas se distribuyen de manera aproximadamente normal ."

# texto spider
txtspider <- "Los salarios de los científicos de datos varían según su rol y experiencia. En el nivel ejecutivo, los científicos de datos tienen el salario más alto, superando a otros roles. Sin embargo, no hay valores para el rol de investigador en este nivel. En el nivel senior, los salarios son similares, con una media de 154484. En el nivel intermedio, los arquitectos de datos lideran con un salario de 149714, considerablemente por encima de la media. No hay científicos de datos en el nivel junior. El ingeniero de análisis junior supera el sueldo del nivel intermedio para el mismo rol, pero en otros roles, el nivel intermedio sigue siendo predominante respecto al nivel junior."

# textos análisis multivariado, face



# textos análisis multivariado mapa