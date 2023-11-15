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
                   El proceso de análisis y modelado se lleva a cabo utilizando el lenguaje de programación R. Esto implica realizar tareas de limpieza y transformación de los datos, realizar análisis exploratorios, construir modelos predictivos y evaluar su desempeño. El resultado final es un informe técnico que documenta el proceso completo, presentando visualizaciones, gráficos y tablas que respalden los análisis realizados y los resultados obtenidos.")),
        # Right bottom section
        div(
          style = "flex: 1; padding: 10px;",
          tags$b("Autoras"),
          tags$p("Daniela Alcázar, Esteffany Peña, Juliana Rueda")
        )
      )
    )
    ),
    tabPanel("Descriptivo",
             #titlePanel(""),
             sidebarLayout(
               # Sidebar with a slider input
               sidebarPanel(width = 3,
                            h2("Análisis descriptivo."),
                            conditionalPanel(
                              condition = "input.id_des == 'Análisis univariado' || (input.id_des == 'Análisis multivariado' & input.id_mul == 'Salarios por categoría')",
                              selectInput(inputId="var1",
                                          label="Seleccione una variable:",
                                          choices=c2,
                                          selected=c2[2]),
                              radioButtons(inputId="var2",
                                           label="Seleccione el tipo de gráfico:",
                                           choices= c4,
                                           selected=c4[1],
                                           inline = T),
                              div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
                              conditionalPanel(
                                condition = "input.id_des == 'Análisis univariado'",
                                textOutput("insunivariado")
                              ),
                              conditionalPanel(
                                condition = "(input.id_des == 'Análisis multivariado' & input.id_mul == 'Salarios por categoría')",
                                textOutput("inscategoria")
                              )
                              # EXPLICACIÓN DE LA SECCIÓN
                            ),
                            conditionalPanel(
                              condition = "input.id_mul == 'Facet' & input.id_des == 'Análisis multivariado'", #OJO AL CAMBIAR NOMBRE DEL PANEL
                              selectInput(inputId="var3",
                                          label="Seleccione una variable:",
                                          choices=c2,
                                          selected=c2[2]),
                              selectInput(inputId="var4",
                                          label="Seleccione otra variable:",
                                          choices=c2,
                                          selected=c2[1]),
                              div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
                              textOutput("insfacet")
                              # EXPLICACIÓN DE LA SECCIÓN
                            ),
                            conditionalPanel(
                              condition = "input.id_dat == 'Data Frame' & input.id_des == 'Data'",
                              selectInput(inputId="var5",
                                          label="Seleccione una variable:",
                                          choices=c("Ninguna", c2),
                                          selected="Ninguna"),
                              conditionalPanel(
                                condition = "input.var5 != 'Ninguna'",
                                selectInput(inputId="var6",
                                            label="Seleccione un nivel:",
                                            choices=c6,
                                            selected=c6[1]),
                                radioButtons(inputId="var7",
                                             label="Seleccione el salario(s) que desea visualizar:",
                                             choices= c7,
                                             selected=c7[1],
                                             inline = T)
                              ),
                              div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
                              textOutput("insdatatable"),
                              div(style ="padding-top: 20px;",
                                  downloadButton('download',"Descarga los datos"))
                            ),
                            conditionalPanel(
                              condition = "(input.id_des == 'Análisis multivariado' & input.id_mul == 'Spider')",
                              div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
                              textOutput("insspider")
                            ),
                            conditionalPanel(
                              condition = "(input.id_des == 'Análisis multivariado' & input.id_mul == 'Choropleth')",
                              div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
                              textOutput("insmapa")
                            ),
                            conditionalPanel(
                              condition = "(input.id_des == 'Data' & input.id_dat == 'Summary')",
                              div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
                              textOutput("inssummary")
                            )
               ),
               mainPanel(width = 9,
                         tabsetPanel(id = "id_des", #ID - siempre en el tabset
                                     tabPanel(title="Análisis univariado",
                                              div(style = "padding: 10px;  background-color: white;",
                                                  plotlyOutput("piebar", width = "100%")),
                                              div(style = "padding: 20px;  background-color: white; margin-top: 10px;",
                                                  textOutput("anaunivariado"))
                                              ),
                                     tabPanel(title="Análisis multivariado",
                                              tabsetPanel(id = "id_mul",
                                                          tabPanel(title="Salarios por categoría",
                                                                   conditionalPanel(
                                                                     condition = "input.var2 == 'Joy Plot'",
                                                                     div(style = "padding: 10px;  background-color: white;",
                                                                       plotOutput("joyplot")),
                                                                     div(style = "padding: 20px;  background-color: white; margin-top: 10px;",
                                                                         textOutput("anajoyplot"))
                                                                     ),
                                                                   conditionalPanel(
                                                                     condition = "input.var2 == 'Box Plot'",
                                                                     div(style = "flex: 1; border-bottom: 1px;", #este border sería bueno aplicarlo antes de todos los análisis
                                                                       plotlyOutput("boxplot")),
                                                                     div(style = "padding: 20px;  background-color: white; margin-top: 10px;",
                                                                         textOutput("anaboxplot"))
                                                                     )
                                                                   ),
                                                          tabPanel(title="Facet",
                                                                   plotlyOutput("facet", height = "500px", width = "100%")),
                                                          tabPanel(title="Spider",
                                                                   div(style = "padding: 20px;  background-color: white;",
                                                                     plotlyOutput("spider"))),
                                                          tabPanel(title="Choropleth"))
                                     ),
                                     tabPanel(title="Data", 
                                              tabsetPanel(id = "id_dat",
                                                          tabPanel(title="Data Frame",
                                                                   div(style ="padding-top: 20px;",
                                                                       dataTableOutput("dataT"))),
                                                          tabPanel(title="Summary", gt_output("summary"))
                                              )
                                     )
                         )
               )
             )
    ),
    tabPanel("Predictivo"),
    tabPanel("Apoyo")
  )
)

