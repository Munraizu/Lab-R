install.packages("igraph")
install.packages("ggplot2")

library(igraph)
library(ggplot2)

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

visualize_graph <- function(graph) {
  plot(graph, layout = layout.circle, vertex.color = "blue", edge.color = "black", vertex.label.cex = 0.8)
  legend("topleft", legend = "Укладка 1", pch = 21, col = "blue", pt.bg = "blue", bg = "white", cex = 0.8)
  
  plot(graph, layout = layout.fruchterman.reingold, vertex.color = "red", edge.color = "black", vertex.label.cex = 0.8)
  legend("topleft", legend = "Укладка 2", pch = 21, col = "red", pt.bg = "red", bg = "white", cex = 0.8)
  
  plot(graph, layout = layout.kamada.kawai, vertex.color = "green", edge.color = "black", vertex.label.cex = 0.8)
  legend("topleft", legend = "Укладка 3", pch = 21, col = "green", pt.bg = "green", bg = "white", cex = 0.8)
}

save_adjacency_matrix <- function(graph, file_name) {
  adjacency_matrix <- as_adjacency_matrix(graph)
  write.table(adjacency_matrix, file = file_name, sep = "\t", quote = FALSE)
}

save_graph_plot <- function(graph, file_name, format) {
  png(file_name, width = 800, height = 600)
  plot(graph, layout = layout.fruchterman.reingold, vertex.color = "red", edge.color = "black", vertex.label.cex = 0.8)
  dev.off()
}

edgelist <- generate_edgelist(10, 15)

graph <- create_graph(edgelist)

visualize_graph(graph)

save_adjacency_matrix(graph, "adjacency_matrix.txt")
save_graph_plot(graph, "graph_plot.png", format = "png")

