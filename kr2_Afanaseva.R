library(igraph)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

generate_edgelist <- function(num_vertices, num_edges) { 
  edges <- matrix(0, nrow = num_edges, ncol = 2) 
  for (i in 1:num_edges) { 
    edges[i, 1] <- sample(1:num_vertices, 1) 
    edges[i, 2] <- sample(1:num_vertices, 1)  
  } 
  edgelist <- data.frame(from = edges[, 1], to = edges[, 2]) 
  return(edgelist) 
} 

create_graph <- function(edgelist) { 
  graph <- graph_from_edgelist(as.matrix(edgelist)) 
  return(graph) 
} 

edgelist <- generate_edgelist(10, 15) 
graph <- create_graph(edgelist) 

graph_circle <- graph
graph_fr <- graph
graph_kk <- graph

layout_circle <- layout.circle(graph_circle)
layout_fr <- layout.fruchterman.reingold(graph_fr)
layout_kk <- layout.kamada.kawai(graph_kk)

p1 <- plot(graph_circle, layout = layout_circle, vertex.color = "blue", edge.color = "black", vertex.label.cex = 0.8)
p2 <- plot(graph_fr, layout = layout_fr, vertex.color = "red", edge.color = "black", vertex.label.cex = 0.8)
p3 <- plot(graph_kk, layout = layout_kk, vertex.color = "green", edge.color = "black", vertex.label.cex = 0.8)

multiplot(p1, p2, p3, cols = 3)

save_adjacency_matrix(edgelist, "edgelist.txt")
save_graph_plot(p1, "graph.png", format = "png")

