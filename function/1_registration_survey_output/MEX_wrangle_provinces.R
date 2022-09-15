##################
# D. Eastern Kang
# 09/15/2022
# This script does: 
# Takes an Mexico output from Qualtrics
# Wrangles to output the response ID, name, department, and municipality

# load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(glue, tidyverse)


MEX_wrangle <- function(MEX_tibble) {
  MEX_wrangled <- MEX_tibble %>% 
    filter(Q0.2 != "No") %>% #remove people who didn't consent
    select(ResponseId, 
           EndDate,  # Select respondent, dept (Q6.2), and mun.svy (Q7.1:Q7.33)
           Q6.2, 
           Q7.1:Q7.32, #previously Q7.1:Q7.33; Mexico has 32 states 
           Q8.1.1:Q8.32.2, #previously Q8.1.1:Q8.33.2 
           Q1.3_1, #nombre 
           Q1.3_2, #apellido
           email = Q1.4_4, 
           exp = Q1.5, #experiencia en anos
           industry = Q1.6, 
           citizen = Q5.5, 
           lang = UserLanguage, 
           phone = Q5.3.1, 
           phone_company = Q5.4) %>%   # select metadata
    separate_rows(Q6.2,  
                  sep = "," , 
                  convert = FALSE) %>%  # Separate content by comma and create new row
    mutate(# replace variables if the validation is triggered:     
      Q7.1 = ifelse(Q6.2 == 'Aguascalientes' &  Q8.1.1 == 'No', Q8.1.2,  Q7.1),
      Q7.2 = ifelse(Q6.2 == 'Baja California' &  Q8.2.1 == 'No', Q8.2.2, Q7.2),
      Q7.3 = ifelse(Q6.2 == 'Baja California Sur' &  Q8.3.1 == 'No', Q8.3.2, Q7.3),
      Q7.4 = ifelse(Q6.2 == 'Campeche' &  Q8.4.1 == 'No', Q8.4.2, Q7.4),
      Q7.5 = ifelse(Q6.2 == 'Coahuila' &  Q8.5.1 == 'No', Q8.5.2, Q7.5),
      Q7.6 = ifelse(Q6.2 == 'Colima' &  Q8.6.1 == 'No', Q8.6.2, Q7.6),
      Q7.7 = ifelse(Q6.2 == 'Chiapas' &  Q8.7.1 == 'No', Q8.7.2, Q7.7),
      Q7.8 = ifelse(Q6.2 == 'Chihuahua' &  Q8.8.1 == 'No', Q8.8.2, Q7.8),
      Q7.9 = ifelse(Q6.2 == 'Ciudad de México' &  Q8.9.1 == 'No', Q8.9.2, Q7.9),
      Q7.10 = ifelse(Q6.2 == 'Durango' &  Q8.10.1 == 'No', Q8.10.2, Q7.10),
      Q7.11 = ifelse(Q6.2 == 'Guanajuato' &  Q8.11.1 == 'No', Q8.11.2, Q7.11),
      Q7.12 = ifelse(Q6.2 == 'Guerrero' &  Q8.12.1 == 'No', Q8.12.2, Q7.12),
      Q7.13 = ifelse(Q6.2 == 'Hidalgo' &  Q8.13.1 == 'No', Q8.13.2, Q7.13),
      Q7.14 = ifelse(Q6.2 == 'Jalisco' &  Q8.14.1 == 'No', Q8.14.2, Q7.14),
      Q7.15 = ifelse(Q6.2 == 'México' &  Q8.15.1 == 'No', Q8.15.2, Q7.15),
      Q7.16 = ifelse(Q6.2 == 'Michoacán' &  Q8.16.1 == 'No', Q8.16.2, Q7.16),
      Q7.17 = ifelse(Q6.2 == 'Morelos' &  Q8.17.1 == 'No', Q8.17.2, Q7.17),
      Q7.18 = ifelse(Q6.2 == 'Nayarit' &  Q8.18.1 == 'No', Q8.18.2, Q7.18),
      Q7.19 = ifelse(Q6.2 == 'Nuevo León' &  Q8.19.1 == 'No', Q8.19.2, Q7.19),
      Q7.20 = ifelse(Q6.2 == 'Oaxaca' &  Q8.20.1 == 'No', Q8.20.2, Q7.20),
      Q7.21 = ifelse(Q6.2 == 'Puebla' &  Q8.21.1 == 'No', Q8.21.2, Q7.21),
      Q7.22 = ifelse(Q6.2 == 'Querétaro' &  Q8.22.1 == 'No', Q8.22.2, Q7.22),
      Q7.23 = ifelse(Q6.2 == 'Quintana Roo' &  Q8.23.1 == 'No', Q8.23.2, Q7.23),
      Q7.24 = ifelse(Q6.2 == 'San Luis Potosí' &  Q8.24.1 == 'No', Q8.24.2, Q7.24),
      Q7.25 = ifelse(Q6.2 == 'Sinaloa' &  Q8.25.1 == 'No', Q8.25.2, Q7.25),
      Q7.26 = ifelse(Q6.2 == 'Sonora' &  Q8.26.1 == 'No', Q8.26.2, Q7.26),
      Q7.27 = ifelse(Q6.2 == 'Tabasco' &  Q8.27.1 == 'No', Q8.27.2, Q7.27),
      Q7.28 = ifelse(Q6.2 == 'Tamaulipas' &  Q8.28.1 == 'No', Q8.28.2, Q7.28),
      Q7.29 = ifelse(Q6.2 == 'Tlaxcala' &  Q8.29.1 == 'No', Q8.29.2, Q7.29),
      Q7.30 = ifelse(Q6.2 == 'Veracruz' &  Q8.30.1 == 'No', Q8.30.2, Q7.30),
      Q7.31 = ifelse(Q6.2 == 'Yucatán' &  Q8.31.1 == 'No', Q8.31.2, Q7.31),
      Q7.32 = ifelse(Q6.2 == 'Zacatecas' &  Q8.32.1 == 'No', Q8.32.2, Q7.32),
      mun = # Gen mun var matching their respective department
        case_when(# Create var mun and enter corresponding municipality per state
          Q6.2 == "Aguascalientes" ~ Q7.1,     
          Q6.2 == "Baja California" ~ Q7.2,
          Q6.2 == "Baja California Sur" ~ Q7.3,
          Q6.2 == "Campeche" ~ Q7.4,
          Q6.2 == "Coahuila" ~ Q7.5,
          Q6.2 == "Colima" ~ Q7.6,
          Q6.2 == "Chiapas" ~ Q7.7,
          Q6.2 == "Chihuahua" ~ Q7.8,
          Q6.2 == "Ciudad de México" ~ Q7.9,
          Q6.2 == "Durango" ~ Q7.10,
          Q6.2 == "Guanajuato" ~ Q7.11,
          Q6.2 == "Guerrero" ~ Q7.12,
          Q6.2 == "Hidalgo" ~ Q7.13,
          Q6.2 == "Jalisco" ~ Q7.14,
          Q6.2 == "México" ~ Q7.15,
          Q6.2 == "Michoacán" ~ Q7.16,
          Q6.2 == "Morelos" ~ Q7.17,
          Q6.2 == "Nayarit" ~ Q7.18,
          Q6.2 == "Nuevo León" ~ Q7.19,
          Q6.2 == "Oaxaca" ~ Q7.20,
          Q6.2 == "Puebla" ~ Q7.21,
          Q6.2 == "Querétaro" ~ Q7.22,
          Q6.2 == "Quintana Roo"~ Q7.23,
          Q6.2 == "San Luis Potosí" ~ Q7.24,
          Q6.2 == "Sinaloa" ~ Q7.25,
          Q6.2 == "Sonora" ~ Q7.26,
          Q6.2 == "Tabasco" ~ Q7.27,
          Q6.2 == "Tamaulipas" ~ Q7.28,
          Q6.2 == "Tlaxcala" ~ Q7.29,
          Q6.2 == "Veracruz" ~ Q7.30,
          Q6.2 == "Yucatán" ~ Q7.31,
          Q6.2 == "Zacatecas" ~ Q7.32
        ),
      name = glue("{str_to_title(Q1.3_1)} {str_to_title(Q1.3_2)}")) %>% 
    separate_rows(mun, sep = ",") 
  return(MEX_wrangled)
}