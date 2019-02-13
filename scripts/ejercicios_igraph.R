library(igraph)

#hacer todo el script de ejercicios
source("scripts/ejercicios_dplyr.R")

#usar tabla2 para hacer una red

##modificar tabla para que source y target sean columnas 1 y 2

tabla2[,-1]
tabla2[,1]

da.fr <- cbind(tabla2[,-1], 
               Airline = tabla2$Airline
)


#falta filtrar para que los stops sean siempre 0 ... usar dplyr::filter

g <- igraph::graph_from_data_frame(d = da.fr, 
                                   directed = TRUE)


degree(g)
hist(degree(g), breaks = 1000)
betweenness(g)

average.path.length(g)
transitivity(g)

shortest_paths(g, from = "MEX")
diameter(g)
radius(g, mode = "in")
radius(g, mode = "out")
eccentricity(g, mode = "out")

tabla2 %>% head

table(tabla2$Stops_Number)


V(g)
E(g) 

V(g)$name
V(g)$grado_all <- degree(graph = g, mode = "all")
V(g)$grado_all
V(g)$name 

get.data.frame(g, "vertices")
V(g)$grado_in <- degree(graph = g, mode = "in")
get.data.frame(g, "vertices") %>%  head

g <- set.vertex.attribute(graph = g, name = "degree_out", 
                     value = degree(graph = g, mode = "out"))

get.data.frame(g, "vertices") %>%  head

g
get.data.frame(g, "vertices")
