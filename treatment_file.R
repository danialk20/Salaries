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
         company_location = case_when(company_location=="US"~"Estados Unidos", 
                                      company_location=="GB"~"Reino Unido", 
                                      company_location=="CA"~"Canadá", 
                                      company_location=="ES"~"España", 
                                      company_location=="IN"~"India", 
                                      company_location=="DE"~"Alemania", 
                                      T~"Otro"),
         employee_residence = case_when(employee_residence=="US"~"Estados Unidos", 
                                        employee_residence=="GB"~"Reino Unido", 
                                        employee_residence=="CA" ~"Canadá", 
                                        employee_residence=="ES"~"España", 
                                        employee_residence=="IN"~"India", 
                                        employee_residence=="DE"~"Alemania", 
                                        T~"Otro"),
         salary_currency = case_when(salary_currency == "USD" ~ "Dólar americano",
                                     salary_currency == "EUR" ~ "Euro",
                                     salary_currency == "GBP" ~ "Libra esterlina",
                                     salary_currency == "INR" ~ "Rupia india",
                                     salary_currency == "CAD" ~ "Dólar canadiense",
                                     salary_currency == "AUD" ~ "Dólar australiano",
                                     TRUE ~ "Otra"),
         employment_type = case_when(employment_type=="CT"~"Término fijo", 
                                     employment_type=="FL"~"Independiente", 
                                     employment_type=="FT" ~"Tiempo completo",
                                     T~"Medio tiempo"),
         job_title = case_when(job_title=="Data Engineer"~"Ingeniero de datos", 
                               job_title=="Data Scientist"~"Científico de datos", 
                               job_title=="Data Analyst" ~"Analista de datos", 
                               job_title=="Machine Learning Engineer"~"Ingeniero de aprendizaje automático", 
                               job_title=="Analytics Engineer"~"Ingeniero de análisis", 
                               job_title=="Data Architect"~"Arquitecto de datos",
                               job_title=="Research Scientist"~"Investigador científico", 
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
