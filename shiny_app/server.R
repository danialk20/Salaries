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
    defb <- 5
    if (var_ == "Año"){
      defb <- 30
    }
    pie_chart <- plot_ly(pie_data_, labels =~ levels_, values =~ frequency_, type = "pie", marker = list(colors = colores_[1:ncolores_])) %>%
      layout(title = paste0("Distribución según la variable ", tolower(var_)),
             showlegend = TRUE,
             legend = list(x = 0.8, y = 0.5),
             margin = list(l = 0, r = 0, b = defb, t = deft),
             paper_bgcolor = "white",
             plot_bgcolor = "#F5F7F9",
             font = list(family = "Montserrat", color = "#555", size = 14))
    
    return(pie_chart)
  }
  
  FPlotlyBarChart <- function(var_, data) {
    data_ <- data[, var_]
    freq_ <- table(data_)
    bar_data_ <- data.frame(levels_ = names(freq_), 
                            frequency_ = as.numeric(freq_))
    colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
    ncolores_ <- length(freq_)
    dangle <- 0
    dhjust <- 0.5
    if (var_ == "Rol"){
      dangle <- 45
      dhjust <- 1
    }
    bar_chart_ <- ggplot(bar_data_, aes(x = levels_, y = frequency_, fill = levels_, color=levels_)) +
      geom_bar(stat = "identity", color = "white") +
      labs(title = paste0("Distribución según la varible ", tolower(var_)),
           fill = var_,
           x = NULL, y = "Frecuencia") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
            plot.title = element_text(hjust = 0.5, face = "bold", color = "#404040"),
            legend.position = "right",
            legend.title = element_text(color = "#404040"),
            legend.text = element_text(color = "#404040"),
            axis.title.x = element_text(color = "#404040"),
            axis.text.x = element_text(color = "#404040", angle = dangle, hjust = dhjust),
            axis.title.y = element_text(color = "#404040"),
            axis.text.y = element_text(color = "#404040"),
            text = element_text(family = "Montserrat")) +
      geom_text(aes(label = frequency_), 
                position = position_stack(vjust = 0.5), 
                color = "black", size = 3) +
      scale_y_continuous(labels = NULL) + 
      scale_fill_manual(values = colores_[1:ncolores_]) +
      scale_color_manual(values = colores_[1:ncolores_])
    
    return(ggplotly(bar_chart_))
  }
  
  FPlotlyBoxPlot <- function(var_, data){
    data_ <- data[, var_]
    colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
    ncolores_ <- nrow(unique(data_))
    dangle <- 0
    dhjust <- 0.5
    if (var_ == "Rol"){
      dangle <- 45
      dhjust <- 1
    }
    boxplot_ <- ggplot(data, aes_string(x = paste0("`", var_, "`"), y = "`Salario en USD`", fill = paste0("`", var_, "`"), color = paste0("`", var_, "`"))) +
      geom_boxplot(alpha = 0.8, size = 1) +
      labs(title = paste0("Salario según la variable ", tolower(var_)),
           x = var_, y = "Salario en USD") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
            plot.title = element_text(hjust = 0.5, face = "bold", color = "#404040"),
            legend.title=element_text(color="#404040"),
            legend.text = element_text(color = "#404040"),
            axis.title.x = element_text(color="#404040"),
            axis.text.x = element_text(color = "#404040", angle = dangle, hjust = dhjust),
            axis.title.y = element_text(color="#404040"),
            axis.text.y = element_text(color="#404040"),
            text = element_text(family = "Montserrat"))+
      scale_fill_manual(values = colores_[1:ncolores_]) +
      scale_color_manual(values = colores_[1:ncolores_])
    
    boxplot_ <- plotly_build(boxplot_)
    boxplot_$x$data <- lapply(boxplot_$x$data, FUN = function(x){
      x$marker$outliercolor = x$line$color 
      x$marker$color = x$line$color
      x$marker$line = x$line$color
      return(x)
    })
    return(boxplot_)
  }
  
  FJoyPlot <- function(var_, data){
    data_ <- data[, var_]
    colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
    ncolores_ <- nrow(unique(data_))
    joyplot_ <- ggplot(data, aes_string(x = "`Salario en USD`", y = paste0("`", var_, "`"), fill = paste0("`", var_, "`"), color = paste0("`", var_, "`"))) +
      geom_density_ridges(alpha = 0.8, size=0.1) +
      theme_ridges() +
      labs(x = "Salario en USD", y = var_) +
      #theme_minimal() +
      theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
            plot.title = element_text(hjust = 0.5, face = "bold", color="#404040"),
            legend.title=element_text(color="#404040"),
            legend.position = "none",
            legend.text = element_text(color = "#404040"),
            axis.title.x = element_text(color="#404040", hjust = 0.5),
            axis.text.x = element_text(color="#404040"),
            axis.title.y = element_text(color="#404040", hjust = 0.5),
            axis.text.y = element_text(color="#404040"),
            text = element_text(family = "Montserrat")) +
      scale_fill_manual(values = colores_[1:ncolores_]) +
      scale_color_manual(values = colores_[1:ncolores_])
    return(joyplot_)
  }
  
  FWordCloud <- function(data){
    data_ <- data[, "Rol"]
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
    FWordCloud(data = datatemp) 
  })
  
  
  #OUTPUTS
  
  # WordCloud
  output$wordcloud = renderWordcloud2({
    word_cloud_r()
  })
  
  # Datatable
  output$dataT <- renderDataTable(data)
  
  # Summary
  output$summary <- renderPrint({
    data %>% 
      summary()
  })
  
  # Prueba - pie bar 2 PLOTLY
  output$distP11 <- renderPlotly({
    if (type_r()==TRUE){
      plt_pie_chart_r()}
    else{
      plt_bar_chart_r()
    }
  })
  
  # Prueba - boxplot
  output$relP2 <- renderPlotly({
    plt_box_plot_r()
  })
  
  output$jp <- renderPlot({
    joy_plot_r()
  })
  
}

