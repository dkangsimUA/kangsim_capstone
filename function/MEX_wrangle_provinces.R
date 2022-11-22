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
           Q7.1:Q7.32
          ) %>%   # select metadata
    separate_rows(Q6.2,  
                  sep = "," , 
                  convert = FALSE) %>%  # Separate content by comma and create new row
    mutate(# replace variables if the validation is triggered:     
      Q7.1 = case_when(Q6.2 == 'Aguascalientes' ~  Q7.1),
      Q7.2 = case_when(Q6.2 == 'Baja California'~ Q7.2),
      Q7.3 = case_when(Q6.2 == 'Baja California Sur'~ Q7.3),
      Q7.4 = case_when(Q6.2 == 'Campeche'~ Q7.4),
      Q7.5 = case_when(Q6.2 == 'Coahuila'~ Q7.5),
      Q7.6 = case_when(Q6.2 == 'Colima'~ Q7.6),
      Q7.7 = case_when(Q6.2 == 'Chiapas'~ Q7.7),
      Q7.8 = case_when(Q6.2 == 'Chihuahua'~ Q7.8),
      Q7.9 = case_when(Q6.2 == 'Ciudad de México'~ Q7.9),
      Q7.10 = case_when(Q6.2 == 'Durango'~ Q7.10),
      Q7.11 = case_when(Q6.2 == 'Guanajuato'~ Q7.11),
      Q7.12 = case_when(Q6.2 == 'Guerrero'~ Q7.12),
      Q7.13 = case_when(Q6.2 == 'Hidalgo'~Q7.13),
      Q7.14 = case_when(Q6.2 == 'Jalisco'~ Q7.14),
      Q7.15 = case_when(Q6.2 == 'México'~ Q7.15),
      Q7.16 = case_when(Q6.2 == 'Michoacán'~ Q7.16),
      Q7.17 = case_when(Q6.2 == 'Morelos'~ Q7.17),
      Q7.18 = case_when(Q6.2 == 'Nayarit'~ Q7.18),
      Q7.19 = case_when(Q6.2 == 'Nuevo León'~ Q7.19),
      Q7.20 = case_when(Q6.2 == 'Oaxaca'~ Q7.20),
      Q7.21 = case_when(Q6.2 == 'Puebla'~ Q7.21),
      Q7.22 = case_when(Q6.2 == 'Querétaro'~ Q7.22),
      Q7.23 = case_when(Q6.2 == 'Quintana Roo'~ Q7.23),
      Q7.24 = case_when(Q6.2 == 'San Luis Potosí'~ Q7.24),
      Q7.25 = case_when(Q6.2 == 'Sinaloa'~ Q7.25),
      Q7.26 = case_when(Q6.2 == 'Sonora'~ Q7.26),
      Q7.27 = case_when(Q6.2 == 'Tabasco'~ Q7.27),
      Q7.28 = case_when(Q6.2 == 'Tamaulipas'~ Q7.28),
      Q7.29 = case_when(Q6.2 == 'Tlaxcala'~ Q7.29),
      Q7.30 = case_when(Q6.2 == 'Veracruz'~ Q7.30),
      Q7.31 = case_when(Q6.2 == 'Yucatán'~ Q7.31),
      Q7.32 = case_when(Q6.2 == 'Zacatecas'~ Q7.32),
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
        )) %>% 
    separate_rows(mun, sep = ",") 
  return(MEX_wrangled)
}