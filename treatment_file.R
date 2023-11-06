library(dplyr)

url <- "https://raw.githubusercontent.com/danialk20/Salarios_CD/main/original_data.csv"

data <- readr::read_csv(url, show_col_types = FALSE)

data <- data %>%
  mutate(across(c("work_year", "experience_level", "employment_type", "job_title", "salary_currency",
                  "employee_residence", "company_location", "company_size", "remote_ratio"), as.factor),
         across(c("salary", "salary_in_usd"), as.numeric),
         company_size = case_when(company_size == "L" ~ "Grande",
                                  company_size == "M" ~ "Mediana",
                                  company_size == "S" ~ "Pequeña"),
         experience_level = case_when(experience_level == "EX" ~ "Ejecutivo",
                                      experience_level == "SE" ~ "Senior",
                                      experience_level == "MI" ~ "Intermedio",
                                      experience_level == "EN" ~ "Junior"),
         remote_ratio = case_when(remote_ratio == "0" ~ "Presencial",
                                  remote_ratio == "50" ~ "Parcialmente remota",
                                  remote_ratio == "100" ~ "Remota"),
         company_location = case_when(data$company_location=="US"~"Estados Unidos", 
                                      data$company_location=="GB"~"Reino Unido", 
                                      data$company_location=="CA"~"Canadá", 
                                      data$company_location=="ES"~"España", 
                                      data$company_location=="IN"~"India", 
                                      data$company_location=="DE"~"Alemania", 
                                      T~"Otro"),
         employee_residence = case_when(data$employee_residence=="US"~"Estados Unidos", 
                                        data$employee_residence=="GB"~"Reino Unido", 
                                        data$employee_residence=="CA" ~"Canadá", 
                                        data$employee_residence=="ES"~"España", 
                                        data$employee_residence=="IN"~"India", 
                                        data$employee_residence=="DE"~"Alemania", 
                                        T~"Otro"),
         salary_currency = case_when(salary_currency == "USD" ~ "Dólar americano",
                                     salary_currency == "EUR" ~ "Euro",
                                     salary_currency == "GBP" ~ "Libra esterlina",
                                     salary_currency == "INR" ~ "Rupia india",
                                     salary_currency == "CAD" ~ "Dólar canadiense",
                                     salary_currency == "AUD" ~ "Dólar australiano",
                                     TRUE ~ "Otra"),
         employment_type = case_when(data$employment_type=="CT"~"Término fijo", 
                                     data$employment_type=="FL"~"Independiente", 
                                     data$employment_type=="FT" ~"Tiempo completo",
                                     T~"Medio tiempo"),
         job_title = case_when(data$job_title=="Data Engineer"~"Ingeniero de datos", 
                               data$job_title=="Data Scientist"~"Científico de datos", 
                               data$job_title=="Data Analyst" ~"Analista de datos", 
                               data$job_title=="Machine Learning Engineer"~"Ingeniero de aprendizaje automático", 
                               data$job_title=="Analytics Engineer"~"Ingeniero de análisis", 
                               data$job_title=="Data Architect"~"Arquitecto de datos",
                               data$job_title=="Research Scientist"~"Investigador científico", 
                               T~"Otro"),
         across(c("work_year", "experience_level", "employment_type", "job_title", "salary_currency",
                  "employee_residence", "company_location", "company_size", "remote_ratio"), as.factor),
         across(c("salary", "salary_in_usd"), as.numeric)
  )

v_eliminar <- c("salary")
data <- data %>% select(-all_of(v_eliminar))

lim_sup <- quantile(data$salary_in_usd, 0.75) + (IQR(data$salary_in_usd) * 1.5)
lim_inf <- quantile(data$salary_in_usd, 0.25) - (IQR(data$salary_in_usd) * 1.5)
data <- subset(data, salary_in_usd <= lim_sup & salary_in_usd > lim_inf)

names(data) <- c("Año", "Nivel de experiencia", "Tipo de contrato", "Rol", "Moneda de pago",
                 "Salario en USD", "Residencia del empleado", "Modalidad de trabajo",
                 "Ubicación de la empresa", "Tamaño de la compañía")

