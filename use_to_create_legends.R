g1 <- readRDS("./data/g1.rds")
g1_agriculture <- readRDS("./data/g1_agriculture.rds")
g1_environment <- readRDS("./data/g1_environment.rds")
g1_flooding <- readRDS("./data/g1_flooding.rds")
g1_groundwater <- readRDS("./data/g1_groundwater.rds")
g1_innovation <- readRDS("./data/g1_innovation.rds")
g1_municipal <- readRDS("./data/g1_municipal.rds")
g1_oilandgas <- readRDS("./data/g1_oilandgas.rds")
g1_rural <- readRDS("./data/g1_rural.rds")

data <- list(
  "g1" = g1,
  "g1_agriculture" = g1_agriculture,
  "g1_environment" = g1_environment,
  "g1_flooding" = g1_flooding,
  "g1_groundwater" = g1_groundwater,
  "g1_innovation" = g1_innovation,
  "g1_municipal" = g1_municipal,
  "g1_oilandgas" = g1_oilandgas,
  "g1_rural" = g1_rural
)


for (i in 1:length(data)) {
  
  print(names(data[i]))
  werk <- data[i]
  gvis <- toVisNetworkData(werk)
  print("Nodes")
  nodes <- gvis$nodes
  print(unique(nodes$type))
  
  
}


g1 <- readRDS("./data/g12.rds")
g1_agriculture <- readRDS("./data/g1_agriculture2.rds")
g1_environment <- readRDS("./data/g1_environment2.rds")
g1_flooding <- readRDS("./data/g1_flooding2.rds")
g1_groundwater <- readRDS("./data/g1_groundwater2.rds")
g1_innovation <- readRDS("./data/g1_innovation2.rds")
g1_municipal <- readRDS("./data/g1_municipal2.rds")
g1_oilandgas <- readRDS("./data/g1_oilandgas2.rds")
g1_rural <- readRDS("./data/g1_rural2.rds")


data2 <- list(
  "g1" = g1,
  "g1_agriculture" = g1_agriculture,
  "g1_environment" = g1_environment,
  "g1_flooding" = g1_flooding,
  "g1_groundwater" = g1_groundwater,
  "g1_innovation" = g1_innovation,
  "g1_municipal" = g1_municipal,
  "g1_oilandgas" = g1_oilandgas,
  "g1_rural" = g1_rural
)
  
  
  for (i in 1:length(data2)) {
    
    print(names(data2[i]))
    werk <- data2[[i]]
    gvis <- toVisNetworkData(werk)
    print("Nodes")
    nodes <- gvis$nodes
    print(unique(nodes$type))
    print("Edges")
    edges <- gvis$edges
    print(unique(edges$type))
    print("")
    print("")
    print("")
    
    
  }
