#runApp("C:\\Users\\danii\\OneDrive - Universidad de Antioquia\\Escritorio\\Rday\\Rday directory\\SALARIES")

#####
library(readr)
library(shiny) 
library(shinythemes)
#library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggtext)
library(plotly)
library(RColorBrewer)
library(ggridges)
library(wordcloud2)
#library(htmlwidgets)


#####


# Define la interfaz de usuario


ruta <- "https://raw.githubusercontent.com/danialk20/dear/main/ds_salaries.csv"



data <- read_csv(ruta)

colF <- c("work_year", "experience_level", "employment_type", "job_title", "salary_currency", "employee_residence", "company_location", "company_size", "remote_ratio")
data[colF] <- lapply(data[colF], as.factor)
colN <- c("salary", "salary_in_usd")
data[colN] <- lapply(data[colN], as.numeric)

data$company_size<-case_when(data$company_size=="L"~"Grande", 
                             data$company_size=="M"~"Mediana", 
                             data$company_size=="S" ~"Pequeña") 
data$company_size<-as.factor(data$company_size)

data$experience_level<-case_when(data$experience_level=="EX"~"Ejecutivo", 
                             data$experience_level=="SE"~"Senior", 
                             data$experience_level=="MI" ~"Intermedio",
                             data$experience_level=="EN" ~"Junior") 
data$experience_level<-as.factor(data$experience_level)

data$remote_ratio<-case_when(data$remote_ratio=="0"~"Presencial", 
                             data$remote_ratio=="50"~"Parcialmente remota", 
                             data$remote_ratio=="100" ~"Remota") 
data$remote_ratio<-as.factor(data$remote_ratio)

data$company_location<-case_when(data$company_location=="US"~"US", 
                                 data$company_location=="GB"~"GB", 
                                 data$company_location=="CA"~"CA", 
                                 data$company_location=="ES"~"ES", 
                                 data$company_location=="IN"~"IN", 
                                 data$company_location=="DE"~"DE", 
                                 T~"Other")
data$company_location<-as.factor(data$company_location)

data$employee_residence<-case_when(data$employee_residence=="US"~"US", 
                                   data$employee_residence=="GB"~"GB", 
                                   data$employee_residence=="CA" ~"CA", 
                                   data$employee_residence=="ES"~"ES", 
                                   data$employee_residence=="IN"~"IN", 
                                   data$employee_residence=="DE"~"DE", 
                                   T~"Other")
data$employee_residence<-as.factor(data$employee_residence)

data$salary_currency<-case_when(data$salary_currency=="USD"~"USD", 
                                data$salary_currency=="EUR"~"EUR", 
                                data$salary_currency=="GBP" ~"GBP", 
                                data$salary_currency=="INR"~"INR", 
                                data$salary_currency=="CAD"~"CAD", 
                                data$salary_currency=="AUD"~"AUD", 
                                T~"Other")
data$salary_currency<-as.factor(data$salary_currency)

#for wordcloud
datatemp <- data

data$job_title<-case_when(data$job_title=="Data Engineer"~"Data Engineer", 
                          data$job_title=="Data Scientist"~"Data Scientist", 
                          data$job_title=="Data Analyst" ~"Data Analyst", 
                          data$job_title=="Machine Learning Engineer"~"Machine Learning Engineer", 
                          data$job_title=="Analytics Engineer"~"Analytics Engineer", 
                          data$job_title=="Data Architect"~"Data Architect",
                          data$job_title=="Research Scientist"~"Research Scientist", 
                          T~"Other")
data$job_title<-as.factor(data$job_title)

v_eliminar=c("salary")
data <- dplyr::select(data, (-all_of(v_eliminar)))
datatemp <- dplyr::select(datatemp, (-all_of(v_eliminar)))

lim_sup<- quantile(data$salary_in_usd,0.75) +(IQR(data$salary_in_usd)*1.5) 
lim_inf<- quantile(data$salary_in_usd,0.25) - (IQR(data$salary_in_usd)*1.5 ) 
data<-subset(data, data$salary_in_usd<=lim_sup & data$salary_in_usd>lim_inf)
lim_suptemp<- quantile(datatemp$salary_in_usd,0.75) +(IQR(datatemp$salary_in_usd)*1.5) 
lim_inftemp<- quantile(datatemp$salary_in_usd,0.25) - (IQR(datatemp$salary_in_usd)*1.5 ) 
datatemp<-subset(datatemp, datatemp$salary_in_usd<=lim_suptemp & datatemp$salary_in_usd>lim_inftemp)
#####
## Treatment
names(data) <- c("Año", "Nivel de experiencia", "Tipo de contrato", "Rol", "Moneda de pago",
                 "Salario en USD", "Residencia del empleado", "Modalidad de trabajo",
                 "Ubicación de la empresa", "Tamaño de la compañía")
names(datatemp) <- c("Año", "Nivel de experiencia", "Tipo de contrato", "Rol", "Moneda de pago",
                 "Salario en USD", "Residencia del empleado", "Modalidad de trabajo",
                 "Ubicación de la empresa", "Tamaño de la compañía")
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
c4 <- c("Pie Plot", "Barplot")

#####
# Pie_chart
# 
# output$pieP1 <- renderPlot({
#   var_ <- input$var1
#   data_ <- data[, var_]
#   freq_ <- table(data_)
#   pie_data_ <- data.frame(levels_ = names(freq_), 
#                          frequency_ = as.numeric(freq_))
#   pie_data_$percentage_ <- pie_data_$frequency_ / sum(pie_data_$frequency_) * 100
#   colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
#   ncolores_ <- length(freq_)
#   
#   pie_chart_ <- ggplot(pie_data_, aes(x = "", y = frequency_, fill = levels_, color=levels_)) +
#     geom_bar(stat = "identity", width = 1, color = "white") +
#     coord_polar("y", start = 0) +
#     labs(title = paste0("Distribución de ",var_),
#          fill = var_,
#          x = NULL, y = NULL) +
#     theme_minimal() +
#     theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
#           plot.title = element_text(hjust = 0.5, face = "bold", color = "#404040"),
#           legend.position = "right",
#           legend.title = element_text(color = "#404040"),
#           legend.text = element_text(color = "#404040"),
#           axis.title.x = element_text(color = "#404040"),
#           axis.text.x = element_text(color = "#404040"),
#           axis.title.y = element_text(color = "#404040"),
#           axis.text.y = element_text(color = "#404040"))+
#     geom_text(aes(label = paste0(round(percentage_), "%")), 
#               position = position_stack(vjust = 0.5), 
#               color = "white", size = 4, fontface = "bold") +
#     scale_y_continuous(labels = NULL) +
#     scale_fill_manual(values = colores_[1:ncolores_])+
#     scale_color_manual(values = colores_[1:ncolores_]) +
#     theme_void()
#   
# 
#   pie_chart_
# })
######
FPieChart <- function(var_, data) {
  data_ <- data[, var_]
  freq_ <- table(data_)
  pie_data_ <- data.frame(levels_ = names(freq_), 
                          frequency_ = as.numeric(freq_))
  pie_data_$percentage_ <- pie_data_$frequency_ / sum(pie_data_$frequency_) * 100
  colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
  ncolores_ <- length(freq_)
  
  pie_chart_ <- ggplot(pie_data_, aes(x = "", y = frequency_, fill = levels_, color=levels_)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    labs(title = paste0("Distribución de ", var_),
         fill = var_,
         x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
          plot.title = element_text(hjust = 0.5, face = "bold", color = "#404040"),
          legend.position = "right",
          legend.title = element_text(color = "#404040"),
          legend.text = element_text(color = "#404040"),
          axis.title.x = element_text(color = "#404040"),
          axis.text.x = element_text(color = "#404040"),
          axis.title.y = element_text(color = "#404040"),
          axis.text.y = element_text(color = "#404040")) +
    geom_text(aes(label = paste0(round(percentage_), "%")), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 4, fontface = "bold") +
    scale_y_continuous(labels = NULL) +
    scale_fill_manual(values = colores_[1:ncolores_]) +
    scale_color_manual(values = colores_[1:ncolores_]) +
    theme_void()
  
  return(pie_chart_)
}

#####
# output$pieP11 <- renderPlotly({
# 
#   var_ <- input$var1
#   data_ <- data[, var_]
#   freq_ <- table(data_)
#   pie_data_ <- data.frame(levels_ = names(freq_),
#                           frequency_ = as.numeric(freq_))
#   pie_data_$percentage_ <- pie_data_$frequency_ / sum(pie_data_$frequency_) * 100
#   colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
#   ncolores_ <- length(freq_)
# 
#   pie_chart <- plot_ly(pie_data_, labels =~ levels_, values =~ frequency_, type = "pie", marker = list(colors = colores_[1:ncolores_])) %>%
#     layout(title = paste0("Distribución de ", var_),
#            showlegend = TRUE,
#            legend = list(x = 0.9, y = 0.5),
#            margin = list(l = 0, r = 0, b = 0, t = 50),
#            paper_bgcolor = "transparent",
#            plot_bgcolor = "#F5F7F9")
# 
# })
generatePlotlyPieChart <- function(var_, data) {
  data_ <- data[, var_]
  freq_ <- table(data_)
  pie_data_ <- data.frame(levels_ = names(freq_),
                          frequency_ = as.numeric(freq_))
  pie_data_$percentage_ <- pie_data_$frequency_ / sum(pie_data_$frequency_) * 100
  colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
  ncolores_ <- length(freq_)
  
  pie_chart <- plot_ly(pie_data_, labels =~ levels_, values =~ frequency_, type = "pie", marker = list(colors = colores_[1:ncolores_])) %>%
    layout(title = paste0("Distribución de ", var_),
           showlegend = TRUE,
           legend = list(x = 0.9, y = 0.5),
           margin = list(l = 0, r = 0, b = 0, t = 50),
           paper_bgcolor = "transparent",
           plot_bgcolor = "#F5F7F9")
  
  return(pie_chart)
}


# output$barP1 <- renderPlot({
#   var_ <- input$var1
#   data_ <- data[, var_]
#   freq_ <- table(data_)
#   bar_data_ <- data.frame(levels_ = names(freq_), 
#                           frequency_ = as.numeric(freq_))
#   colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
#   ncolores_ <- length(freq_)
#   
#   bar_chart_ <- ggplot(bar_data_, aes(x = levels_, y = frequency_, fill = levels_, color=levels_)) + #revisar color = levels_
#     geom_bar(stat = "identity", color = "white") +
#     labs(title = paste0("Distribución de ", var_),
#          fill = var_,
#          x = NULL, y = "Frecuencia") +
#     theme_minimal() +
#     theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
#           plot.title = element_text(hjust = 0.5, face = "bold", color = "#404040"),
#           legend.position = "right",
#           legend.title = element_text(color = "#404040"),
#           legend.text = element_text(color = "#404040"),
#           axis.title.x = element_text(color = "#404040"),
#           axis.text.x = element_text(color = "#404040"),
#           axis.title.y = element_text(color = "#404040"),
#           axis.text.y = element_text(color = "#404040"))+
#     geom_text(aes(label = frequency_), 
#               position = position_stack(vjust = 0.5), 
#               color = "white", size = 4, fontface = "bold") +
#     scale_y_continuous(labels = NULL) + 
#     scale_fill_manual(values = colores_[1:ncolores_])+
#     scale_color_manual(values = colores_[1:ncolores_])
#   
#   bar_chart_
# })


# output$barP11 <- renderPlotly({
#   var_ <- input$var1
#   data_ <- data[, var_]
#   freq_ <- table(data_)
#   bar_data_ <- data.frame(levels_ = names(freq_), 
#                           frequency_ = as.numeric(freq_))
#   colores_ <- c('#FF616E', "#88D7D8", "#FFA600", "#A5E580", "#FFDB4D", "#F9A9D5", "#4ABF6B", "#C668A0")
#   ncolores_ <- length(freq_)
#   
#   bar_chart_ <- ggplot(bar_data_, aes(x = levels_, y = frequency_, fill = levels_, color=levels_)) + #revisar color = levels_
#     geom_bar(stat = "identity", color = "white") +
#     labs(title = paste0("Distribución de ", var_),
#          fill = var_,
#          x = NULL, y = "Frecuencia") +
#     theme_minimal() +
#     theme(panel.background = element_rect(fill = "#F5F7F9", color = "lightgray"), 
#           plot.title = element_text(hjust = 0.5, face = "bold", color = "#404040"),
#           legend.position = "right",
#           legend.title = element_text(color = "#404040"),
#           legend.text = element_text(color = "#404040"),
#           axis.title.x = element_text(color = "#404040"),
#           axis.text.x = element_text(color = "#404040"),
#           axis.title.y = element_text(color = "#404040"),
#           axis.text.y = element_text(color = "#404040"))+
#     geom_text(aes(label = frequency_), 
#               position = position_stack(vjust = 0.5), 
#               color = "white", size = 4, fontface = "bold") +
#     scale_y_continuous(labels = NULL) + 
#     scale_fill_manual(values = colores_[1:ncolores_])+
#     scale_color_manual(values = colores_[1:ncolores_])
#   
#   ggplotly(bar_chart_)
# })
