library(tidyverse)
library(data.table)

#cargar datos 
tabla <- data.table::fread(input = "data/rutas_aereas.txt")
tabla

#Queremos quitar columnas redundantes
#usamos dplyr::select
tabla2 <- tabla %>%
    dplyr::select(Airline,
                  Source_airport,
                  Destination_Airport,
                  Codeshare,
                  Stops_Number,
                  Equipment
                  )
tabla2
