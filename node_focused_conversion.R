
g1 <- readRDS("./data/g12.rds")
g1_agriculture <- readRDS("./data/g1_agriculture2.rds")
g1_environment <- readRDS("./data/g1_environment2.rds")
g1_flooding <- readRDS("./data/g1_flooding2.rds")
g1_groundwater <- readRDS("./data/g1_groundwater2.rds")
g1_innovation <- readRDS("./data/g1_innovation2.rds")
g1_municipal <- readRDS("./data/g1_municipal2.rds")
g1_oilandgas <- readRDS("./data/g1_oilandgas2.rds")
g1_rural <- readRDS("./data/g1_rural2.rds")


gvis<-toVisNetworkData(g1)
edges <- gvis$edges
edges <- edges %>% replace_na(list(color = "gray"))
nodes <- sort(gvis$nodes)
edge_color <- select(nodes, id, color)
edges <- edges %>% left_join(edge_color, by = c("from" = "id")) %>% select(-color.x) %>% rename(color = color.y)
g1 <- graph_from_data_frame(d= edges, vertices = nodes)

gvis<-toVisNetworkData(g1_agriculture)
edges <- gvis$edges
edges <- edges %>% replace_na(list(color = "gray"))
nodes <- sort(gvis$nodes)
edge_color <- select(nodes, id, color)
edges <- edges %>% left_join(edge_color, by = c("from" = "id")) %>% select(-color.x) %>% rename(color = color.y)
g1_agriculture <- graph_from_data_frame(d= edges, vertices = nodes)

gvis<-toVisNetworkData(g1_environment)
edges <- gvis$edges
edges <- edges %>% replace_na(list(color = "gray"))
nodes <- sort(gvis$nodes)
edge_color <- select(nodes, id, color)
edges <- edges %>% left_join(edge_color, by = c("from" = "id")) %>% select(-color.x) %>% rename(color = color.y)
g1_environment <- graph_from_data_frame(d= edges, vertices = nodes)

gvis<-toVisNetworkData(g1_flooding)
edges <- gvis$edges
edges <- edges %>% replace_na(list(color = "gray"))
nodes <- sort(gvis$nodes)
edge_color <- select(nodes, id, color)
edges <- edges %>% left_join(edge_color, by = c("from" = "id")) %>% select(-color.x) %>% rename(color = color.y)
g1_flooding <- graph_from_data_frame(d= edges, vertices = nodes)

gvis<-toVisNetworkData(g1_groundwater)
edges <- gvis$edges
edges <- edges %>% replace_na(list(color = "gray"))
nodes <- sort(gvis$nodes)
edge_color <- select(nodes, id, color)
edges <- edges %>% left_join(edge_color, by = c("from" = "id")) %>% select(-color.x) %>% rename(color = color.y)
g1_groundwater <- graph_from_data_frame(d= edges, vertices = nodes)

gvis<-toVisNetworkData(g1_innovation)
edges <- gvis$edges
edges <- edges %>% replace_na(list(color = "gray"))
nodes <- sort(gvis$nodes)
edge_color <- select(nodes, id, color)
edges <- edges %>% left_join(edge_color, by = c("from" = "id")) %>% select(-color.x) %>% rename(color = color.y)
g1_innovation <- graph_from_data_frame(d= edges, vertices = nodes)

gvis<-toVisNetworkData(g1_municipal)
edges <- gvis$edges
edges <- edges %>% replace_na(list(color = "gray"))
nodes <- sort(gvis$nodes)
edge_color <- select(nodes, id, color)
edges <- edges %>% left_join(edge_color, by = c("from" = "id")) %>% select(-color.x) %>% rename(color = color.y)
g1_municipal <- graph_from_data_frame(d= edges, vertices = nodes)

gvis<-toVisNetworkData(g1_oilandgas)
edges <- gvis$edges
edges <- edges %>% replace_na(list(color = "gray"))
nodes <- sort(gvis$nodes)
edge_color <- select(nodes, id, color)
edges <- edges %>% left_join(edge_color, by = c("from" = "id")) %>% select(-color.x) %>% rename(color = color.y)
g1_oilandgas <- graph_from_data_frame(d= edges, vertices = nodes)

gvis<-toVisNetworkData(g1_rural)
edges <- gvis$edges
edges <- edges %>% replace_na(list(color = "gray"))
nodes <- sort(gvis$nodes)
edge_color <- select(nodes, id, color)
edges <- edges %>% left_join(edge_color, by = c("from" = "id")) %>% select(-color.x) %>% rename(color = color.y)
g1_rural <- graph_from_data_frame(d= edges, vertices = nodes)


saveRDS(g1, file="./data/g13.rds")
saveRDS(g1_agriculture, file="./data/g1_agriculture3.rds")
saveRDS(g1_environment, file="./data/g1_environment23.rds")
saveRDS(g1_flooding, file="./data/g1_flooding3.rds")
saveRDS(g1_groundwater, file="./data/g1_groundwater3.rds")
saveRDS(g1_innovation, file="./data/g1_innovation3.rds")
saveRDS(g1_municipal, file="./data/g1_municipal3.rds")
saveRDS(g1_oilandgas, file="./data/g1_oilandgas3.rds")
saveRDS(g1_rural, file="./data/g1_rural3.rds")