library(tidyverse)
library(data.table)

#cargar datos 
tabla <- data.table::fread(input = "data/rutas_aereas.txt")


#queremos quitar columnas redundantes 
#usamos dplyr::select


tabla2 <- tabla %>% 
  dplyr::select(Airline, 
                Source_airport, 
                Destination_Airport, 
                Codeshare, 
                Stops_Number, 
                Equipment)


tabla2



#queremos agregar o modificar una columna
#usamos dplyr::mutate

tabla2 <- tabla2 %>%
    mutate(Codeshare = ifelse(test = Codeshare == "Y",
                              yes  = "si_tiene",
                              no   = "no_tiene"                   
                              )
           )

#queremos ver todas las rutas desde o hacia MEX

rutas_mex <- tabla2 %>%
  dplyr::filter(Source_airport == "MEX" | Destination_Airport == "MEX")

#Aquí comprobamos que obtenemos tanto Source_airport 
#como Destination_Airport en rutas_mex
rutas_mex %>% 
  dplyr::filter(Source_airport == "MEX") %>%
  head
