library(tidyverse)
library(data.table)
library(igraph)

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

#Hago un subset de las rutas que tienen Codeshare
rutas_compartidas <- subset(tabla_sin_escalas, Codeshare == TRUE)
rutas_compartidas %>% head

#Ahora voy a quitar las rutas que contienen Codeshare, 
#i.e. los que son TRUE en "Codeshare"
rutas_final <- tabla_sin_escalas %>%
  dplyr::filter(Codeshare == FALSE)

rutas_final %>% head

#Cambio orden de columnas
rutas_final[ , -1]
rutas_final[ , 1]

da.fr <- cbind(rutas_final[,-1], 
               Airline = rutas_final$Airline
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
#No supe cómo cambiar la "distribución"

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


#Ejemplo de filtrado, rutas desde México hacia Atlanta
da.fr %>%
  dplyr::filter(Source_airport == "MEX" & Destination_Airport == "ATL")

#Tomo todas las aerolíneas posibles con unique
#Como esta lista es de tipo Factor, lo convierto a caracteres 
mis_aerolineas <- as.character(unique(da.fr$Airline)) 

#La lista "hereda" los nombres
names(mis_aerolineas) <- mis_aerolineas

#Quiero hacer un ciclo for que me filtre por aerolíneas (pseudocódigo)
for i = 1:length(list)
  array = []
  array[i] <- 
    da.fr %>%
      filter(Airline == list[i])
end

#Aquí ya se aplica el filtrado para aerolíneas, que corresponderán a las distintas capas
capas <- lapply(mis_aerolineas, FUN = function(i){
  da.fr %>%
    dplyr::filter(Airline == i)
}) 

#Se le agrega la gráfica a cada capa
capas <- lapply(mis_aerolineas, FUN = function(i){
    xf <- dplyr::filter(.data = da.fr, Airline == i)
  g <- igraph::graph_from_data_frame(d = xf, 
                                     directed = TRUE)
}) 

#Se grafica la red de la capa correspondiente a la aerolínea 2B
plot(capas$`2B`)

#Se grafica la red de la capa correspondiente a la aerolínea 2B
plot(capas$NH)

#Agrego al ciclo las centralidades de cada capa
capas <- lapply(mis_aerolineas, FUN = function(i){
  xf <- dplyr::filter(.data = da.fr, Airline == i)
  g <- igraph::graph_from_data_frame(d = xf, 
                                     directed = TRUE)
  #propiedades_nombres <- c("APL", "Diámetro", "Radio", "Clustering Coefficient")
  Resultados <- list(APL = average.path.length(g), 
                      Diametro = diameter(g),
                      Radio = radius(g),
                      Clustering_Coefficient = transitivity(g),
                      Red = g)
  #names(propiedades) <- propiedades_nombres
  return(Resultados)
}) 

plot(capas$`2I`$Red)

capas_4 <- capas[1:4
                 ]

capas_4[[1]][["Red"]]
names(capas_4)

for(i in seq_along(capas)){
  plot(capas[[i]][["Red"]],
       main = names(capas[i]))
}

capas[1:4]





#return(plot(g))
return(plot(g, edge.arrow.size=.4, edge.curved=.1,
            vertex.color="orange", 
            vertex.frame.color="#555555",
            vertex.label.color="black",
            vertex.label.cex=.7))
plot(capas$`2B`)

capas$`2I`







## Pruebas
propiedades_nom <- c("APL", "Diámetro", "Radio", "Clustering Coefficient")
propiedades_nom
propi <- vector(mode = "character", length = 5) #num nunca sirvió
propi
names(propi) <- propiedades_nom
propiedades_nom
propi["Grado"] <- 4
propi["Diámetro"] <- 45.5
propi["Grado"] <- 9
propi
plot(g)
propi["APL"] <- average.path.length(g)
propi
