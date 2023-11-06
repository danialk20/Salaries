fluidPage(
  div(
    id = "banner",
    HTML('<h1>Ciencia de datos: salarios bajo la lupa</h1>')
  ),
  includeCSS("tema.css"),
  
  navbarPage(
    title="Contenido",
    tabPanel("Proyecto",div(
      style = "text-align: center;",
      h2("Acerca del proyecto")
    ),  # Centered Title for the first panel
    div(
      style = "display: flex; flex-direction: column; height: 100%;",
      
      # Top section
      div(
        style = "flex: 1; padding: 10px; border-bottom: 1px solid #ccc;",
        tags$p("In the era of data, the allure of data science careers has never been stronger. Our project embarks on a journey through salary data analysis for professionals in the data science domain. This article encapsulates our extensive exploration, covering both descriptive and predictive aspects of data science salaries."),
        tags$b("Descriptive Insights:"),
        tags$p("Our project's initial phase revolves around dissecting and summarizing salary data. We uncover intricate salary distributions, regional nuances, and the profound influence of education and experience. These insights offer a comprehensive snapshot of data science compensation."),      
        tags$b("Predictive Projections:"),
        tags$p("Taking our analysis a step further, predictive modeling takes center stage. Utilizing machine learning, we aim to forecast salary trends for data science professionals. Our models consider various factors such as industry dynamics, technological shifts, and evolving skill sets, painting a future of earning potential."),
        div(
          style = "display: flex; justify-content: center;",
          wordcloud2Output("wordcloud", width = "400px"))
      ),
      # Bottom sections
      div(
        style = "display: flex; flex: 1; justify-content: space-between;",
        
        # Left bottom section
        div(
          style = "flex: 1; padding: 10px; border-right: 1px solid #ccc;",
          tags$b("Objetivo:"),
          tags$p("El objetivo principal de este trabajo es analizar de manera descriptiva y predictiva los salarios de profesionales que trabajan en el campo de la Ciencia de Datos, haciendo uso de la base de datos “Data Science Salaries 2023”.
                   En primer lugar, se lleva a cabo un análisis descriptivo detallado para comprender las estadísticas y características clave de los datos. Se busca obtener una visión completa de las tendencias salariales en el campo de la Ciencia de Datos, así como comprender la influencia de factores como la experiencia laboral, el tipo de empleo, la ubicación geográfica y el tamaño de la empresa en los salarios.
                   Además del análisis descriptivo, se realiza un análisis predictivo para desarrollar modelos estadísticos que permitan predecir los salarios en función de las variables disponibles en la base de datos. El objetivo es utilizar estos modelos para realizar predicciones alrededor del salario.
                   El proceso de análisis y modelado se lleva a cabo utilizando el lenguaje de programación R. Esto implica realizar tareas de limpieza y transformación de los datos, realizar análisis exploratorios, construir modelos predictivos y evaluar su desempeño. El resultado final es un informe técnico que documenta el proceso completo, presentando visualizaciones, gráficos y tablas que respalden los análisis realizados y los resultados obtenidos.")        ),
        
        # Right bottom section
        div(
          style = "flex: 1; padding: 10px;",
          tags$b("Autoras"),
          tags$p("Daniela Alcázar, Esteffany Peña, Juliana Rueda")
        )
      )
    )),
    tabPanel("Análisis",
             h2("Análisis descriptivo."),
             #titlePanel(""),
             sidebarLayout(
               # Sidebar with a slider input
               sidebarPanel(
                 selectInput(inputId="var1",
                             label="Seleccione una variable:",
                             choices=c2,
                             selected=c2[1]),
                 radioButtons(inputId="var2",
                              label="Seleccione el tipo de gráfico:",
                              choices= c4,
                              selected=c4[1])
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel(title="Visualización", 
                            tabsetPanel(tabPanel(title="frecuenciaydists2", plotlyOutput("distP11")),
                                        tabPanel(title="salariosegun", plotlyOutput("relP2"), plotOutput("jp")),
                                        tabPanel(title="salarios", p("hola")))),
                   tabPanel(title="Data", 
                            tabsetPanel(
                              tabPanel(title="Data Frame", dataTableOutput("dataT")),
                              tabPanel(title="Summary", verbatimTextOutput("summary"))
                            )
                   )
                 )
               )
             )
    ),
    tabPanel("Predictivo"),
    tabPanel("Apoyo")
  )
  # Application title
)
