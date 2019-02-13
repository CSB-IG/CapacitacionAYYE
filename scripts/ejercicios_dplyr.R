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
