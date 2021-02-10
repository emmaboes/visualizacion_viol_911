library(dplyr)
library(readr)
library(tidyr) 
library(mxmaps)


# Base de datos -----------------------------------------------------------
#Carga la base y asigna clase a cada columna
df_viol <- read.csv("1_Data/violencia_familiar_llamadas_911.csv", 
               colClasses = c("character","character","character",
                              "character","character","character",
                              "numeric","numeric","numeric","numeric",
                              "numeric","numeric","numeric"),
               header = TRUE) %>% 
  #Convierte columna fecha
  mutate_at(vars(fecha), as.Date, format="%Y-%m-%d") %>% 
  #Excluye los datos nacionales, incluye solo datos del mes A de 2019
  filter(cve_entidad !=33, cve_mes == "A", año == 2019)
  #select(año, cve_entidad, entidad, cve_mes, vio_fam_911_mensual)

# Modificaciones ----------------------------------------------------------

  #Agrega ceros para coincidir con formato de las claves de entidad del INEGI
df_viol$cve_entidad[df_viol$cve_entidad == 1:9] <- paste("0", df_viol$cve_entidad[df_viol$cve_entidad== 1:9], sep = "")

#Selecciona las columnas necesarias para generar el mapa
df_viol_map <- df_viol %>% select(region = cve_entidad, value = vio_fam_911_mensual)   


# Mapa --------------------------------------------------------------------


df_mxstate_2020 %>% #dataset contenido en paquetería mxmaps
  select(region, pop) %>% #selecciona la clave de entidad y población
  inner_join(df_viol_map) %>%  #junta al dataset de mxsmaps el dataset de violencia
  mutate(value = value/pop) %>% #Calcula tasa de llamadas per capita
  mxstate_choropleth( 
                     num_colors = 1,
                     title = "Llamadas de emergencia relacionadas con incidentes de violencia familiar",
                     legend = "Tasa per capita") #Genera el mapa usando region y value
  #choroplethr::Choropleth


