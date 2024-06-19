extract_subnetwork <- function(graph, node_name, distance=NULL) {
  node <- which(V(graph)$name == node_name)
  
  if(length(node) == 0) {
    # Return empty graph
    return(tbl_graph())
  }
  
  distance <- ifelse(is.null(distance), length(graph), distance)
  vertices <- ego(graph, nodes = node, order = distance)[[1]]
  igraph_subgraph <- induced_subgraph(graph, vids = vertices)
  nodes_df <- as_data_frame(igraph_subgraph, what = "vertices")
  edges_sf <- as_data_frame(igraph_subgraph, what = "edges")
  tbl_graph(nodes=nodes_df, edges=edges_sf, directed=is_directed(graph))
}