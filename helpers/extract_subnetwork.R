extract_subnetwork <- function(graph, node_name, distance=-1) {
  # negative distance will show full graph
  node <- which(V(graph)$name == node_name)
  
  if(length(node) == 0) {
    # Return empty graph
    return(tbl_graph())
  }
  
  distance <- ifelse(distance < 0, length(graph), distance)
  vertices <- ego(graph, nodes = node, order = distance)[[1]]
  igraph_subgraph <- induced_subgraph(graph, vids = vertices)
  nodes_df <- as_data_frame(igraph_subgraph, what = "vertices") %>%
    mutate(included = 1)
  edges_sf <- as_data_frame(igraph_subgraph, what = "edges") %>%
    mutate(
      start_date = as_date(ymd(start_date)),
      end_date = as_date(ymd(end_date)),
      included = 1
    )
  tbl_graph(nodes=nodes_df, edges=edges_sf, directed=is_directed(graph))
}