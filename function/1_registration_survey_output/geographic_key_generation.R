#####################
# D. Eastern Kang
# 09/15/2022
# This script does: 
# -filters names of State and Municipality
#####################

#Load all packages----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, glue, openxlsx, tidyverse)

#List of State (Estado) and Municaplity (Municipios) downloaded from:
# https://www.inegi.org.mx/app/ageeml/#
# Codebook: https://www.inegi.org.mx/contenidos/app/ageeml/Ayuda/Ayuda_Gral_Cat_Unico.pdf
# Nombre de AGEE: Nombre oficial de las AGEE (Entidades Federativas); E= estatal
# Nombre de AGEM: Nombre oficial de la base de lav division territorial y de la organizacion politica y administrativa del municipio
# Nombre de Localidad Geoestadistica: Nombre asignado a una localidad por la ley o la costumbre. 

# Note: AGEEML_2022882348534.csv file size is 44MB with 300k rows; it may take awhile
mex_geo_keys<- read_csv("./data/file")