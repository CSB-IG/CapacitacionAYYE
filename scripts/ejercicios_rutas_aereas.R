library(tidyverse)
library(data.table)

#Cargamos la tabla de rutas
tabla <- data.table::fread(
  input = "/Users/yuriko/Documents/UNAM/INMEGEN/R/CapacitacionAYYE/data/rutas_aereas.txt")

#Vemos de qué tipo es tabla
str(tabla)

#Vemos qué información contiene (6 primeros renglones)
tabla %>% head

#Quitamos columnas de ID que no necesitamos
tabla_nueva <- tabla %>%
  dplyr::select(Airline,
                Source_airport,
                Destination_Airport,
                Codeshare,
                Stops_Number,
                Equipment)

#Checamos la tabla nueva
tabla_nueva %>% head

tabla_nueva$Codeshare %>% table

#Queremos converir la columna de Codeshare a booleano
tabla_nueva <- tabla_nueva %>%
  dplyr::mutate(Codeshare = ifelse(test = Codeshare == "Y",
                            yes = TRUE,
                            no = FALSE
                            )
  )

#Vemos la tabla nueva modificada
tabla_nueva $ Codeshare %>%
  table

tabla_nueva %>% 
  dplyr::filter(Codeshare == TRUE) %>%
  head

#Checamos el tipo de estructura de la columna Codeshare
str(tabla_nueva $Codeshare)

#Hago un subset de las rutas que contienen escalas
escalas <- subset(tabla_nueva, Stops_Number != 0)

escalas %>% head

nrow(escalas)

#Ahora voy a quitar las rutas que contienen escalas, 
#i.e. los que son distintos de 0 en "Stops_Number"
tabla_sin_escalas <- tabla_nueva %>% 
  dplyr::filter(Stops_Number == 0)

tabla_sin_escalas %>% head

