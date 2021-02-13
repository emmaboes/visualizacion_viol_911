library(dplyr)
library(readr)
library(mxmaps)
library(scales)
library(leaflet)
# Base de datos enero 2019 -----------------------------------------------------------
#Carga la base y asigna clase a cada columna
df_viol <- read.csv("violencia_familiar_llamadas_911.csv", 
               colClasses = c("character","character","character",
                              "character","character","character",
                              "numeric","numeric","numeric","numeric",
                              "numeric","numeric","numeric"),
               header = TRUE) %>% 
  #Convierte columna fecha
  mutate_at(vars(fecha), as.Date, format="%Y-%m-%d") %>% 
  #Excluye los datos nacionales, incluye solo datos del enero de 2019
  filter(cve_entidad !=33, cve_mes == "A", año == 2019)
  #select(año, cve_entidad, entidad, cve_mes, vio_fam_911_mensual)

# Modificaciones ----------------------------------------------------------

  #Agrega ceros para coincidir con formato de las claves de entidad del INEGI
df_viol$cve_entidad[df_viol$cve_entidad == 1:9] <- paste("0", df_viol$cve_entidad[df_viol$cve_entidad== 1:9], sep = "")

#Selecciona las columnas necesarias para generar el mapa
df_viol_map <- df_viol %>% 
  select(region = cve_entidad, value = vio_fam_911_mensual, state_name = entidad)   


# Mapa para enero de 2019--------------------------------------------------------------------


df_viol_pop <- df_mxstate_2020 %>% #dataset contenido en paquetería mxmaps
  select(region, pop) %>% #selecciona la clave de entidad y población
  inner_join(df_viol_map) %>%  #junta al dataset de mxsmaps el dataset de violencia
  mutate(value = value/pop) #Calcula tasa de llamadas per capita
  
df_viol_pop %>% mxstate_choropleth( 
                     num_colors = 1,
                     title = "Llamadas de emergencia relacionadas con incidentes de violencia familiar",
                     legend = "Tasa per capita") #Genera el mapa usando region y value
  #choroplethr::Choropleth

#scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(8,"PuOr"))

# Mapa interactivo para enero de 2019 --------------------------------------------------------

pal <- colorNumeric("Blues", domain = df_viol_pop$value)
# Tasa per capita
mxstate_leaflet(df_viol_pop,
                pal,
                ~ pal(value),
                ~ sprintf("State: %s<br/>Tasa per capita: %s",
                          state_name, comma(value))) %>%
  addLegend(position = "bottomright", 
            pal = pal, 
            values = df_viol_pop$value,
            title = "Tasa<br>per capita",
            #labFormat = labelFormat(
                                  #  transform = function(x) {100 * x})
            ) %>%
  addProviderTiles("CartoDB.Positron")


# Promediar datos por año ----------------------------------------

df_viol_2019 <- read.csv("violencia_familiar_llamadas_911.csv", 
                    colClasses = c("character","character","character",
                                   "character","character","character",
                                   "numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric"),
                    header = TRUE) %>% 
    #Excluye los datos nacionales, incluye solo datos del mes A de 2019
  filter(cve_entidad !=33, año == 2019) %>% 
    group_by(cve_entidad, entidad) %>% 
summarise(
    mean_calls_2019 = mean(vio_fam_911_mensual) 
  ) %>% 
  mutate(cve_entidad = sprintf("%02d",as.numeric(cve_entidad))) %>% 
  arrange(cve_entidad)


