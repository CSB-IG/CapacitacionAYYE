library(tidyverse)
library(data.table)
library(igraph)

#Cargamos la tabla de rutas
tabla_aerolineas <- data.table::fread(
  input = "/Users/yuriko/Documents/UNAM/INMEGEN/R/CapacitacionAYYE/data/aerolineas.txt")

#Vemos qué información contiene (6 primeros renglones)
tabla_aerolineas %>% head

#Renombramos las columnas de la tabla
names(tabla_aerolineas) <- c("Airline ID", 
                             "Name", 
                             "Alias", 
                             "IATA", 
                             "ICAO", 
                             "Callsign", 
                             "Country", 
                             "Active"
                              )
#Checamos la tabla con los nuevos nombres
tabla_aerolineas %>% head                    

#Quitamos el primer renglón de la tabla
tabla_aerolineas <- tabla_aerolineas [-c(1), ]

#Checamos la tabla
tabla_aerolineas %>% head  

#Ahora hacemos un ciclo que recorra toda la tabla en el cual remueva los renglones correspondientes
#a las aerolíneas que NO estén activas.
tabla_aerol_activas <- tabla_aerolineas %>% 
  dplyr::filter(Active == "Y")

#Checamos la tabla_aerol_activas
tabla_aerol_activas %>% head

