library(tidyverse)
library(data.table)

#cargar datos 
tabla <- data.table::fread(input = "data/rutas_aereas.txt")
