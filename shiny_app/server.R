function(input, output, session) {
  options(scipen = 999)
  
  # FUNCTIONS
  
  FPlotlyPieChart <- function(var_, data) {
    data_ <- data[, var_]
    freq_ <- table(data_)
    
    pie_data_ <- data.frame(levels_ = names(freq_),
                            frequency_ = as.numeric(freq_))
    pie_data_$percentage_ <- pie_data_$frequency_ / sum(pie_data_$frequency_) * 100
    colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
    ncolores_ <- length(freq_)
    deft <- 50
    if (var_ == "Tipo de contrato"){
      deft <- 100
    }
    pie_chart <- plot_ly(pie_data_, labels =~ levels_, values =~ frequency_, type = "pie", marker = list(colors = colores_[1:ncolores_])) %>%
      layout(title =  paste0("<b>Distribución según la variable ", tolower(var_), "</b>"),
             showlegend = TRUE,
             legend = list(x = 0.75, y = 0.5, title=list(text= var_)),
             margin = list(l = 0, r = 0, b = 30, t = deft),
             paper_bgcolor = "white",
             plot_bgcolor = "#F5F7F9",
             font = list(family = "Montserrat")) #, size = 14
    pie_chart <- pie_chart %>% 
      add_trace(
        hovertemplate = paste(paste0(var_,":"), pie_data_$levels_,
                              "<br>Frecuencia:", pie_data_$frequency_,
                              "<br>Porcentaje:", paste0(round(pie_data_$percentage_,1),"%"), "<extra></extra>")
      )
    return(pie_chart)
  }
  
  FPlotlyBarChart <- function(var_, data) {
    data_ <- data[, var_]
    freq_ <- table(data_)
    bar_data_ <- data.frame(levels_ = names(freq_), 
                            frequency_ = as.numeric(freq_))
    colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
    ncolores_ <- length(freq_)
    bar_chart_ <- ggplot(bar_data_, aes(x = levels_, y = frequency_, fill = levels_, color=levels_,
                                        text = paste(paste0(var_,":"), levels_,
                                                     "<br>Frecuencia:", frequency_))) +
      geom_bar(stat = "identity", color = "white") +
      labs(title = paste0("Distribución según la variable ", tolower(var_)),
           fill = var_,
           x = var_, y = "Frecuencia") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
            plot.title = element_text(hjust = 0.5, face = "bold", color = "#555555"),
            legend.position = "right",
            legend.title = element_text(color = "#555555"), 
            legend.text = element_text(color = "#555555"),
            axis.title.x = element_text(color = "#555555"),
            axis.text.x = element_text(color = "#555555", hjust = 0.5),
            axis.title.y = element_text(color = "#555555"),
            axis.text.y = element_text(color = "#555555"),
            text = element_text(family = "Montserrat")) +
      geom_text(aes(label = frequency_), 
                position = position_stack(vjust = 0.5), color = "#555555", size = 3.3) +
      scale_x_discrete(labels = label_wrap(10)) +
      scale_y_continuous(labels = NULL) + 
      scale_fill_manual(values = colores_[1:ncolores_]) +
      scale_color_manual(values = colores_[1:ncolores_])+
      guides(color = "none")
    bar_plotly_chart_ <- ggplotly(bar_chart_, tooltip = c("text"))
    return(bar_plotly_chart_)
  }
  
  FPlotlyBoxPlot <- function(var_, data){
    data_ <- data[, var_]
    varS_ <- data_sym(var_)
    colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
    ncolores_ <- nrow(unique(data_))
    boxplot_ <- ggplot(data, aes(x = !!varS_, y = `Salario en USD`, fill = !!varS_, color = !!varS_)) +
      geom_boxplot(alpha = 0.8, size = 1) +
      labs(title = paste0("Salario según la variable ", tolower(var_)),
           x = var_, y = "Salario en USD") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
            plot.title = element_text(hjust = 0.5, face = "bold", color = "#555555"),
            legend.title=element_text(color="#555555"),
            legend.text = element_text(color = "#555555"),
            axis.title.x = element_text(color="#555555"),
            axis.text.x = element_text(color = "#555555", hjust = 0.5),
            axis.title.y = element_text(color="#555555"),
            axis.text.y = element_text(color="#555555"),
            text = element_text(family = "Montserrat"))+
      scale_x_discrete(labels = label_wrap(10)) +
      scale_fill_manual(values = colores_[1:ncolores_]) +
      scale_color_manual(values = colores_[1:ncolores_])
    
    boxplot_ <- plotly_build(boxplot_)
    boxplot_$x$data <- lapply(boxplot_$x$data, FUN = function(x){
      x$marker$outliercolor = x$line$color 
      x$marker$color = x$line$color
      x$marker$line = x$line$color
      return(x)
    })
    boxplot_ <- boxplot_ %>%
      layout(legend = list(orientation = "v", y = 0.5))
    return(boxplot_)
  }
  
  FJoyPlot <- function(var_, data){
    data_ <- data[, var_]
    varS_ <- sym(var_)
    colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
    ncolores_ <- nrow(unique(data_))
    joyplot_ <- ggplot(data, aes(x = `Salario en USD`, y = !!varS_, fill = !!varS_,, color = !!varS_)) +
      geom_density_ridges(alpha = 0.8, size=0.1) +
      theme_ridges() +
      labs(title = paste0("Salario según la variable ", tolower(var_)),
                          x = "Salario en USD", y = var_) +
      #theme_minimal() +
      theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
            plot.title = element_text(hjust = 0.5, face = "bold", color="#555555"),
            legend.title=element_text(color="#555555"),
            legend.text = element_text(color = "#555555"),
            axis.title.x = element_text(color="#555555", hjust = 0.5),
            axis.text.x = element_text(color="#555555"),
            axis.title.y = element_text(color="#555555", hjust = 0.5),
            axis.text.y = element_text(color="#555555"),
            text = element_text(family = "Montserrat"),
            legend.spacing = unit(.4, "cm"))+
      scale_fill_manual(values = colores_[1:ncolores_]) +
      scale_color_manual(values = colores_[1:ncolores_])
    return(joyplot_)
  }
  
  FWordCloud <- function(){
    data_ <- data_wc[, "Rol"]
    job_titles <- as.character(data_$Rol)
    word_list <- unlist(strsplit(job_titles, " "))
    word_list <- as.factor(word_list)
    freq_ <- table(word_list)
    cloud_data_ <- data.frame(levels_ = names(freq_), 
                              frequency_ = as.numeric(freq_))
    cloud_data_ <- subset(cloud_data_, cloud_data_$frequency_ > 5)
    cloud_data_ <- cloud_data_ %>%
      mutate(frequency_ = case_when(
        frequency_ > 1000 ~ as.integer(frequency_/15),
        frequency_ > 100 ~ as.integer(frequency_/10),
        TRUE ~ frequency_
      ))
    colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0", "#A5464E", "#BA6E3F", "#3F75BA", "#4A7A48", "#FACC16")
    set.seed(123)
    w <- wordcloud2(data=cloud_data_, ellipticity = 1, minSize = 5,
                    color = rep_len(colores_, 
                                    nrow(cloud_data_))) + WCtheme(1)
    return(w)
  }
  
  DTable <- function(data){
    dt_ <- DT::datatable(data, options <- list(
      scrollX = TRUE,
      autoWidth = FALSE,
      pageLength = 10,
      paging = TRUE,
      info = FALSE))
    return(dt_)
  }
  
  FSpiderPlot <- function(data){
    resultados <- data %>%
      group_by(`Nivel de experiencia`, Rol) %>%
      summarise(promedio_salary = mean(`Salario en USD`), .groups = "drop")
    
    lista_data_frames <- resultados %>%
      group_split(`Nivel de experiencia`)
    
    df_Junior1 <- lista_data_frames[[3]]["promedio_salary"]
    df_Ejecutivo1 <- lista_data_frames[[1]]["promedio_salary"]
    df_Intermedio1 <- lista_data_frames[[2]]["promedio_salary"]
    df_Senior1 <- lista_data_frames[[4]]["promedio_salary"]
    
    fn <- data.frame(
      promedio_salary = 0) #fila nueva PREGUNTAR PA Q ESTE CERO
    
    df_Junior1 <- rbind(df_Junior1[1:2, ], fn, df_Junior1[3:7, ], df_Junior1[1,])
    df_Ejecutivo1 <- rbind(df_Ejecutivo1[1:5, ], fn, df_Ejecutivo1[6:7, ], df_Ejecutivo1[1,])
    df_Senior1 <- rbind(df_Senior1, df_Senior1[1,])
    df_Intermedio1 <- rbind(df_Intermedio1, df_Intermedio1[1,])
    
    VEN <- c(df_Junior1$promedio_salary)
    VEX <- c(df_Ejecutivo1$promedio_salary)
    VMI <- c(df_Intermedio1$promedio_salary)
    VSE <- c(df_Senior1$promedio_salary)
    # PREUNTAR A JULI Q ES ESTO
    unique_categories <- c(lista_data_frames[[4]]$`Rol`, lista_data_frames[[4]]$`Rol`[1])
    data <- data.frame(
      Category = unique_categories,
      Value1 = c(VEN),
      Value2 = c(VEX),
      Value3 = c(VMI),
      Value4 = c(VSE)
    )
    
    plot_ly(data, type = 'scatterpolar', mode = 'lines+markers', text = 'Category') %>%
      add_trace(
        r = ~ df_Senior1$promedio_salary, theta = ~Category,
        name = 'Senior', fill = 'toself', fillcolor = 'rgba(249, 169, 213, 0.3)', line = list(color = "#F9A9D5"), marker = list(color = "#D378A6", size = 6),
        hoverinfo = 'text',
        text = ~paste('Rol:', Category, '<br>Salario promedio:', df_Senior1$promedio_salary),
        hovertext = ~paste('Rol:', Category, '<br>Salario promedio:', paste0("$",round(df_Senior1$promedio_salary)))
      ) %>%
      add_trace(
        r = ~ df_Intermedio1$promedio_salary, theta = ~Category,
        name = 'Intermedio', fill = 'toself', fillcolor = 'rgba(136, 215, 216, 0.3)', line = list(color = '#88D7D8'), marker = list(color = "#6FA4A5", size = 6),
        hoverinfo = 'text',
        text = ~paste('Rol:', Category, '<br>Salario promedio:', df_Intermedio1$promedio_salary),
        hovertext = ~paste('Rol:', Category, '<br>Salario promedio:', paste0("$",round(df_Intermedio1$promedio_salary)))
      ) %>%
      add_trace(
        r = ~ df_Junior1$promedio_salary, theta = ~Category,
        name = 'Junior', fill = 'toself', fillcolor = 'rgba(165, 229, 128, 0.3)', line = list(color = "#A5E580"), marker = list(color = "#8ECA71", size = 6),
        hoverinfo = 'text',
        text = ~paste('Rol:', Category, '<br>Salario promedio:', df_Junior1$promedio_salary),
        hovertext = ~paste('Rol:', Category, '<br>Salario promedio:', paste0("$",round(df_Junior1$promedio_salary)))
      ) %>%
      add_trace(
        r = ~ df_Ejecutivo1$promedio_salary, theta = ~Category,
        name = 'Nivel ejecutivo', fill = 'toself', fillcolor = 'rgba(255, 219, 77, 0.3)', line = list(color = "#FFDB4D"), marker = list(color = "#FFC743", size = 6),
        hoverinfo = 'text',
        text = ~paste('Rol:', Category, '<br>Salario promedio:', df_Ejecutivo1$promedio_salary),
        hovertext = ~paste('Rol:', Category, '<br>Salario promedio:', paste0("$",round(df_Ejecutivo1$promedio_salary)))
      ) %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, max(resultados$promedio_salary)),
                                            color = "#555555", tickfont = list(color = "#555555")
                                            ),
                          angularaxis = list(linecolor = "#555555")
                          ),
             title = list(text = "<b>Salarios por rol según el nivel de experiencia</b>", font = list(color = "#555555")),
             showlegend = TRUE,
             margin = list(l = 0, r = 0, b = 20, t = 60),
             legend = list(x = 0.9, y = 0.5),
             paper_bgcolor = "white",
             plot_bgcolor = "#F5F7F9",
             font = list(family = "Montserrat"))
    
  }

  FFacetPlot <- function(var_, var2_, data) {
    colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
    ncolores_ <- nrow(unique(data[, var_]))
    varS_ <- sym(var_)
    var2S_ <- sym(var2_)
    facet_plotly_ <- ggplot(data, aes(x = !!varS_, y = `Salario en USD`,
                                      fill = !!varS_, color = !!varS_)) +
      geom_bar(stat = "summary", fun = "mean") +
      facet_wrap(var2S_) +
      labs(title = paste0("Salarios según las variables x y ", tolower(var_)),
           fill = var_,
           x = var_, y = "Salario en USD") +
      theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
            plot.title = element_text(hjust = 0.5, face = "bold", color = "#555555"),
            legend.title=element_text(color="#555555"),
            legend.text = element_text(color = "#555555"),
            axis.title.x = element_text(color="#555555"),
            axis.text.x = element_blank(),
            axis.title.y = element_text(color="#555555"),
            axis.text.y = element_text(color="#555555"),
            text = element_text(family = "Montserrat")) +
      scale_fill_manual(values = colores_[1:ncolores_]) +
      scale_color_manual(values = colores_[1:ncolores_])
    
    facet_plotly_ <- ggplotly(facet_plotly_, tooltip = c("x","y")) %>%
      layout(legend = list(orientation = "v", y = 0.5, itemwidth= "10px"))
    
    return(facet_plotly_)
  }
  
  # REACTIVE
  
  type_r <- reactive({
    x = (input$var2 == c4[1])
    x
  })
  
  plt_pie_chart_r <- reactive({
    FPlotlyPieChart(var_ = input$var1, data = data)
  })
  
  plt_bar_chart_r <- reactive({
    FPlotlyBarChart(var_ = input$var1, data = data)
  })
  
  plt_box_plot_r <- reactive({
    FPlotlyBoxPlot(var_ = input$var1, data = data)
  })
  
  joy_plot_r <- reactive({
    FJoyPlot(var_ = input$var1, data = data)
  })
  
  word_cloud_r <- reactive({
    FWordCloud()
  })
  
  facet_plot_r <- reactive({
    FFacetPlot(var_ = input$var3, var2_ = input$var4, data = data)
  })
  
  data_dt <- reactive({
    if (input$var5 == "Ninguna") {
      data_ <- data
    } else {
      data_ <- subset(data, data[[input$var5]] == input$var6)
      if (input$var7 == "Máximo") {
        max_ <- max(data_$`Salario en USD`)
        data_ <- data_[data_$`Salario en USD` == max_, ]
      } else if (input$var7 == "Mínimo") {
        min_ <- min(data_$`Salario en USD`)
        data_ <- data_[data_$`Salario en USD` == min_, ]
      }
    }
    data_
  })
  
  datatable_r <- reactive({
    DTable(data = data_dt())
  })
  
  inputvar1 <- reactive({
    var_ <- input$var1
    var_ <- switch(var_,
                   "Año" = "anio",
                   "Nivel de experiencia" = "experiencia",
                   "Tipo de contrato" = "contrato",
                   "Rol" = "rol",
                   "Moneda de pago" = "moneda",
                   "Residencia del empleado" = "residencia",
                   "Modalidad de trabajo" = "modalidad",
                   "Ubicación de la empresa" = "ubicacion",
                   "Tamaño de la compañía" = "tamanio",
    )
    var_
  })
  
  # OBSERVE
  observe({
    tab <- input$id_des
    choices_ <- case_when(
      tab == "Análisis univariado" ~ c4,
      tab == "Análisis multivariado" ~ c5
    )
    updateRadioButtons(
      session = session,
      inputId = "var2",
      choices = choices_,
      selected = choices_[1],
      inline = TRUE
    )
    var3_ <- input$var3
    c2_ <- c2[c2 != var3_]
    updateSelectInput(
      session = session,
      inputId = "var4",
      choices = c2_,
      selected = c2_[1]
    )
    c6_ <- unique(data[[input$var5]])
    updateSelectInput(
      session = session,
      inputId = "var6",
      choices = c6_,
      selected = c6_[1]
    )
  })
  
  # OUTPUTS
  
  # WordCloud
  output$wordcloud = renderWordcloud2({
    word_cloud_r()
  })
  
  # Datatable
  output$dataT <- renderDataTable({
    datatable_r()
  })
  
  # Download
  output$download <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(fname) {
      write.csv(data_dt(), fname)
    }
  )
  
  # Resumen
  output$summary <- render_gt({
    as_gt(tbl_summary(data))
  })
  
  # Pie or Bar
  output$piebar <- renderPlotly({
    if (type_r() == TRUE) {
      plt_bar_chart_r()
    } else {
      plt_pie_chart_r()
    }
  })
  
  # Boxplot
  output$boxplot <- renderPlotly({
    plt_box_plot_r()
  })
  
  # Joyplot
  output$joyplot <- renderPlot({
    joy_plot_r()
  })
  
  # Spider
  output$spider <- renderPlotly({
    FSpiderPlot(data)
  })
  
  # Facet wrap (barplot)
  output$facet <- renderPlotly({
    facet_plot_r()
  })
  
  # Instrucciones univariado
  output$insunivariado <- renderText({
    txtinstrucciones$univariado
  })
  
  # Instrucciones salarios por categoria
  output$inscategoria <- renderText({
    txtinstrucciones$categoria
  })
  
  # Instrucciones facet
  output$insfacet <- renderText({
    txtinstrucciones$facet
  })
  
  # Instrucciones spider
  output$insspider <- renderText({
    txtinstrucciones$spider
  })
  
  # Instrucciones mapa
  output$insmapa <- renderText({
    txtinstrucciones$mapa
  })
  
  # Instrucciones datatable
  output$insdatatable <- renderText({
    txtinstrucciones$datatable
  })
  
  # Instrucciones summary
  output$inssummary <- renderText({
    txtinstrucciones$summary
  })
  
  # Análisis univariado
  output$anaunivariado <- renderText({
    var_ <- inputvar1()
    txtunivariado[[var_]]
  })
  
  # Análisis boxplot
  output$anaboxplot <- renderText({
    var_ <- inputvar1()
    txtcategoria_box[[var_]]
  })
  
  # Análisis joyplot
  output$anajoyplot <- renderText({
    var_ <- inputvar1()
    txtcategoria_joy[[var_]]
  })
}