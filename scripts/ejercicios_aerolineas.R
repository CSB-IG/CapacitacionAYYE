library(tidyverse)
library(data.table)
library(igraph)

#Cargamos la tabla de rutas
tabla_rutas <- data.table::fread(
  input = "/Users/yuriko/Documents/UNAM/INMEGEN/R/CapacitacionAYYE/data/rutas_aereas.txt"
)

#Cargamos la tabla de aerolíneas
tabla_aerolineas <- data.table::fread(
  input = "/Users/yuriko/Documents/UNAM/INMEGEN/R/CapacitacionAYYE/data/aerolineas.txt"
  )

#Vemos qué información contienen (6 primeros renglones)
tabla_rutas %>% head
tabla_aerolineas %>% head

#Renombramos las columnas de la tabla de aerolíneas
names(tabla_aerolineas) <- c("Airline ID", 
                             "Name", 
                             "Alias", 
                             "IATA", 
                             "ICAO", 
                             "Callsign", 
                             "Country", 
                             "Active"
                              )

#Checamos la tabla_aerolíneas con los nuevos nombres
tabla_aerolineas %>% head                    

#Quitamos el primer renglón de la tabla_aerolíneas
tabla_aerolineas <- tabla_aerolineas [-c(1), ]

#Checamos la tabla_aerolíneas
tabla_aerolineas %>% head  

#Quitamos columnas de ID que no necesitamos de tabla_rutas
tabla_rutas <- tabla_rutas %>%
  dplyr::select(Airline,
                Source_airport,
                Destination_Airport,
                Codeshare,
                Stops_Number,
                Equipment)

#Checamos la tabla_rutas
tabla_rutas %>% head

#Ahora hacemos un ciclo que recorra toda la tabla_aerolíneas, el cual 
#remueva los renglones correspondientes a las aerolíneas que NO están activas.
tabla_aerolineas <- tabla_aerolineas %>% 
  dplyr::filter(Active == "Y")

#Checamos la tabla_aerolineas
tabla_aerolineas %>% head

#Encontramos la intersección entre tabla_aerolineas y tabla_rutas
#para usar sólo las rutas de las aerolíneas activas.
tabla_rutas <- semi_join(tabla_rutas, tabla_aerolineas, by = c("Airline" = "IATA"))

#Checamos la tabla_rutas modificada
tabla_rutas %>% head

#Cargamos la tabla de rutas2 (actualizada en dic de 2018)
tabla_rutas2 <- data.table::fread(
  input = "/Users/yuriko/Documents/UNAM/INMEGEN/R/CapacitacionAYYE/data/AirlineRouteMapper/routes.txt"
)

#Checamos la tabla_rutas2
tabla_rutas2 %>% head

#Quitamos el primer y segundo renglón de la tabla_aerolíneas
tabla_rutas2 <- tabla_rutas2 [-c(2), ]

#Checamos tabla_rutas2
tabla_rutas2 %>% head

#Renombramos las columnas de la tabla_rutas2
names(tabla_rutas2) <- c("Airline", 
                         "Source", 
                         "Destination", 
                         "Codeshare", 
                         "Stops", 
                         "Equipment"
)

#Checamos tabla_rutas2
tabla_rutas2 %>% head

#Encontramos la intersección entre tabla_aerolineas y tabla_rutas2
#para usar sólo las rutas de las aerolíneas activas.
tabla_rutas2 <- semi_join(tabla_rutas2, tabla_aerolineas, by = c("Airline" = "IATA"))


##############################################################################################

#Usando la tabla_rutas2

#Queremos converir la columna de Codeshare a booleano
tabla_rutas2 <- tabla_rutas2 %>%
  dplyr::mutate(Codeshare = ifelse(test = Codeshare == "*",
                                   yes = TRUE,
                                   no = FALSE
  )
  )

#Checamos
tabla_rutas2 %>% head

#Ahora voy a quitar las rutas que contienen escalas, 
#i.e. los que son distintos de 0 en "Stops_Number"
tabla_rutas2 <- tabla_rutas2 %>% 
  dplyr::filter(Stops == 0)

#Checamos
tabla_rutas2 

#Ahora voy a quitar las rutas que contienen Codeshare, 
#i.e. los que son TRUE en "Codeshare"
tabla_rutas2 <- tabla_rutas2 %>%
  dplyr::filter(Codeshare == FALSE)

#Checamos
tabla_rutas2 

#Cambio orden de columnas
tabla_rutas2[ , -1]
tabla_rutas2[ , 1]

da.fr <- cbind(tabla_rutas2[,-1], 
               Airline = tabla_rutas2$Airline
)

#Checo cómo se ve
da.fr %>% head

#Hago red
g <- igraph::graph_from_data_frame(d = da.fr, 
                                   directed = TRUE)

#Graficamos todo en una capa
plot(g, edge.arrow.size=.4, edge.curved=.1,
     vertex.color="orange", 
     vertex.frame.color="#555555",
     vertex.label.color="black",
     vertex.label.cex=.7)


#Grado
degree(g) %>% head

#Estructura de degree(g)
str(degree(g))

#Average Shortest Path Length
average.path.length(g)

#Betweenness
betweenness(g) %>% head

#Diámetro
diameter(g)

#Radio
radius(g)

#Excentricidad
eccentricity(g) %>% head

#Transitivity/Clustering Coefficient
transitivity(g)

#Vemos cuántas aerolíneas distintas tenemos
length(unique(da.fr$Airline))

#Tomo todas las aerolíneas posibles con unique
#Como las aerolíneas son de tipo Factor, lo convierto a caracteres 
mis_aerolineas <- as.character(unique(da.fr$Airline)) 

#La lista "hereda" los nombres
names(mis_aerolineas) <- mis_aerolineas



#Se aplica el filtrado para aerolíneas, que corresponderán a distintas capas 
#y le agrega la gráfica a cada capa
graph_capas <- lapply(mis_aerolineas, FUN = function(i){
  xf <- dplyr::filter(.data = da.fr, Airline == i)
  g <- igraph::graph_from_data_frame(d = xf, 
                                     directed = TRUE)
}) 

#Eg. Se grafica la red de la capa correspondiente a la aerolínea 2B
plot(graph_capas$`2B`)


#Agrego al ciclo las centralidades de cada capa
capas <- lapply(mis_aerolineas, FUN = function(i){
  xf <- dplyr::filter(.data = da.fr, Airline == i)
  g <- igraph::graph_from_data_frame(d = xf, 
                                     directed = TRUE)
  Resultados <- list(APL = average.path.length(g), 
                     Diametro = diameter(g),
                     Radio = radius(g),
                     Clustering_Coefficient = transitivity(g),
                     Red = g)
  
  return(Resultados)
}) 


#Llamamos la información de APL de la capa correspondiente a la aerolínea 2B
capas$`2B`$APL

#Eg. Para graficar la capa correspondiente a la aerolínea 2I.
#Graficamos la entrada Red de la aerolínea 2I
plot(capas$`2I`$Red)

#Juntamos toda la información de las capas
capas_totales <- capas[1:567
                 ]

#Checamos los nombres de todas las aerolíneas
names(capas_totales)

#Generalizamos.
#Hacemos un ciclo que llame a la función capas y las grafique
for(i in seq_along(capas)){
  plot(capas[[i]][["Red"]],
       main = names(capas[i]))
}

#Eg. Llamamos la entrada 1 del conjunto de capas totales y después llamamos la Red
capas[[1]][["Red"]]
#Para graficarlo
plot(capas_totales[[1]][["Red"]])
