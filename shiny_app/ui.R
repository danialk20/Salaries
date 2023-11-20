tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   flex-wrap: wrap;
                                   flex-direction: column;
                                   align-content: space_between;
                                 } 
                                 ")) 
  ))

fluidPage(
  div(
    id = "banner",
    HTML('<h1>Ciencia de datos: salarios bajo la lupa</h1>')
  ),
  includeCSS("tema.css"),
  
  navbarPage(
    title="Contenido",
    tabPanel(
      "Proyecto",
      div(
        style = "text-align: center;",
        h2("Acerca del proyecto:")
      ),  # Centered Title for the first panel
      div(
        style = "display: flex; flex-direction: column; height: 100%;",
        
        # Top section
        div(
          style = "flex: 1; padding: 10px; border-bottom: 1px solid #ccc;",
          tags$p("La ciencia de datos es un campo de estudio que ha ganado importancia en los últimos aC1os. Los salarios de los profesionales en esta área dependen de factores inherentes al puesto de trabajo. Este proyecto presenta una aplicaciún Shiny desarrollada en R, que te permitirá explorar la relaciún entre algunas variables que afectan el salario de los profesionales; te presentamos la visualizaciún y el análisis descriptivo de los datos, así como la posibilidad de que predigas un salario a partir de la configuraciún de las demás variables. Usa esta aplicaciún como una herramienta para la toma de decisiones y planificaciún a nivel profesional. Si eres empresario, puedes guiarte para determinar el salario adecuado para un científico de datos. B!Bienvenidos! Esperamos te guste el Shiny."),
          div(
            style = "display: flex; justify-content: center;",
            wordcloud2Output("wordcloud", width = "4000px", height = "150px")
          )
        ),
        # Bottom sections
        div(
          style = "display: flex; flex: 1; justify-content: space-between;",
          
          # Left bottom section
          div(
            style = "flex: 1; padding: 10px; border-right: 1px solid #ccc;",
            tags$b("Objetivo:"),
            tags$p("El propúsito de este proyecto es presentar un análisis descriptivo de los factores que afectan los salarios de los profesionales que trabajan en el campo de la Ciencia de datos, así como la informaciún más relevante de estas relaciones. Por otro lado, se busca predecir mediante modelos estadísticos los salarios de los profesionales en funciún de las variables encontradas.")
          ),
          # Right bottom section
          div(
            style = "flex: 1; padding: 10px;",
            tags$b("Un poco sobre nosotras:"),
            tags$p("B!Hola! somos un grupo de estudiantes de ingeniería industrial de la universidad de Antioquia, las cuales cursan actualmente su sexto semestre. Nos une la pasiún por la analítica, la innovaciún y el deseo de contribuir al ámbito académico. En busca de darle un valor agregado a nuestra educaciún, nace este proyecto, el cual pretende darle una herramienta a profesionales, estudiantes y empresarios para que conozcan un poco de este fascinante mundo de la ciencia de datos. Gracias por realizar este recorrido con nosotras.")
          )
        )
      )
    ),
    tabPanel(
      "Descriptivo",
      #titlePanel(""),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h2("Análisis descriptivo."),
          conditionalPanel(
            condition = "input.id_des == 'Análisis univariado' || (input.id_des == 'Análisis multivariado' & input.id_mul == 'Salario según una variable')",
            selectInput(inputId = "var1",
                        label = "Seleccione una variable:",
                        choices = c2,
                        selected = c2[2]),
            radioButtons(inputId = "var2",
                         label = "Seleccione el tipo de gráfico:",
                         choices = c4,
                         selected = c4[1],
                         inline = TRUE),
            div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
            conditionalPanel(
              condition = "input.id_des == 'Análisis univariado'",
              textOutput("insunivariado")
            ),
            conditionalPanel(
              condition = "(input.id_des == 'Análisis multivariado' & input.id_mul == 'Salario según una variable')",
              textOutput("inscategoria")
            )
            # EXPLICACICÓN DE LA SECCICÓN
          ),
          conditionalPanel(
            condition = "input.id_mul == 'Salario según dos variables' & input.id_des == 'Análisis multivariado'", #OJO AL CAMBIAR NOMBRE DEL PANEL
            selectInput(inputId = "var3",
                        label = "Seleccione una variable:",
                        choices = c2,
                        selected = c2[2]),
            selectInput(inputId = "var4",
                        label = "Seleccione otra variable:",
                        choices = c2,
                        selected = c2[1]),
            div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
            textOutput("insfacet")
            # EXPLICACICÓN DE LA SECCICÓN
          ),
          conditionalPanel(
            condition = "input.id_dat == 'Base de datos' & input.id_des == 'Datos'",
            selectInput(inputId = "var5",
                        label = "Seleccione una variable:",
                        choices = c("Ninguna", c2),
                        selected = "Ninguna"),
            conditionalPanel(
              condition = "input.var5 != 'Ninguna'",
              selectInput(inputId = "var6",
                          label = "Seleccione un nivel:",
                          choices = c6,
                          selected = c6[1]),
              radioButtons(inputId = "var7",
                           label = "Seleccione el salario(s) que desea visualizar:",
                           choices = c7,
                           selected = c7[1],
                           inline = TRUE)
            ),
            div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
            textOutput("insdatatable"),
            div(style ="padding-top: 20px;",
                downloadButton('download', "Descarga los datos"))
          ),
          conditionalPanel(
            condition = "(input.id_des == 'Análisis multivariado' & input.id_mul == 'Gráfico radar')",
            div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
            textOutput("insspider")
          ),
          conditionalPanel(
            condition = "(input.id_des == 'Análisis multivariado' & input.id_mul == 'Mapa coroplético')",
            div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
            textOutput("insmapa")
          ),
          conditionalPanel(
            condition = "(input.id_des == 'Datos' & input.id_dat == 'Resumen')",
            div(style = "border-top: 1px solid rgba(85, 85, 85, 0.5); margin-top: 10px; margin-bottom: 10px;"),
            textOutput("inssummary")
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            id = "id_des", #ID - siempre en el tabset
            tabPanel(
              title = "Análisis univariado",
              div(style = "padding: 10px;  background-color: white;",
                  withSpinner(plotlyOutput("piebar", width = "100%"))),
              div(style = "padding: 20px;  background-color: white; margin-top: 10px;",
                  textOutput("anaunivariado"))
            ),
            tabPanel(
              title = "Análisis multivariado",
              tabsetPanel(
                id = "id_mul",
                tabPanel(
                  title = "Salario según una variable",
                  conditionalPanel(
                    condition = "input.var2 == 'Joy Plot'",
                    div(style = "padding: 10px;  background-color: white;",
                        withSpinner(plotOutput("joyplot"))),
                    div(style = "padding: 20px;  background-color: white; margin-top: 10px;",
                        textOutput("anajoyplot"))
                  ),
                  conditionalPanel(
                    condition = "input.var2 == 'Box Plot'",
                    div(style = "flex: 1; border-bottom: 1px;", #este border sería bueno aplicarlo antes de todos los análisis
                        withSpinner(plotlyOutput("boxplot"))),
                    div(style = "padding: 20px;  background-color: white; margin-top: 10px;",
                        textOutput("anaboxplot"))
                  )
                ),
                tabPanel(
                  title = "Salario según dos variables",
                  withSpinner(plotlyOutput("facet", height = "500px", width = "100%"))
                ),
                tabPanel(
                  title = "Gráfico radar",
                  div(style = "padding: 20px;  background-color: white;",
                      withSpinner(plotlyOutput("spider")))
                ),
                tabPanel(
                  title = "Mapa coroplético"
                )
              )
            ),
            tabPanel(
              title = "Datos",
              tabsetPanel(
                id = "id_dat",
                tabPanel(
                  title = "Base de datos",
                  div(style ="padding-top: 20px;",
                      withSpinner(dataTableOutput("dataT")))
                ),
                tabPanel(
                  title = "Resumen",
                  withSpinner(gt_output("summary"))
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Predictivo",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h2("side")
        ),
        mainPanel(tweaks,
          fluidRow(style = "border: 1px solid lightgrey;",
                   column(8, 
                          list(tags$div(align = 'left', 
                                   class = 'multicol',
                                   h3("Variables de entrenamiento"),
                                   p("Escoge las variables que quieras incluir en el modelo:"),
                                   br(),
                                   checkboxGroupInput("checkGroup",
                                             choices = c2,
                                             label = NULL,
                                             selected = c2,
                                             inline = TRUE))
                               
                          )),
                   column(4, 
                          sliderInput(
                            "slidert",
                            label = h3("Train/Test Split %"),
                            min = 0,
                            max = 100,
                            value = 80),
                          textOutput("cntTrain"),
                          textOutput("cntTest"),
                          br()
                          )   
          ),
          fluidRow(style = "border: 1px solid lightgrey;",
                   column(width = 8,
                          withSpinner(verbatimTextOutput("Model")),
                          title = "Model Summary"),
                   column(width = 4, 
                          titlePanel("My application"),
                          fluidRow(
                            column(5,
                                   textInput("text1", "Enter something")     
                            ),
                            column(5,
                                   textInput("text2", "Enter something else")     
                            )
                          ),
                          fluidRow(
                            column(3,
                                   textInput("text3", "Enter another thing"),
                                   offset = 9
                            )
                            ))),
          fluidRow(style = "border: 4px double red;",
                   numericInput("n", "n", 1))
        )
      ),
      tags$head(
        tags$style(
          HTML(
            ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
          )
        ) 
      )
    )
  )
)
      
# fluidPage(
#   fluidRow(
#     column(
#       width = 3,
#       selectInput(
#         "SelectX",
#         label = "Select variables:",
#         choices = names(data),
#         multiple = TRUE,
#         selected = names(data)
#       )
#     ),
#     column(
#       width = 3,
#       selectInput(
#         "SelectY",
#         label = "Select variable to predict:",
#         choices = names(data)
#       )
#     )
#   )
# )
