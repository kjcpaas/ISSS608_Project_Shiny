convert_graph_to_power_flow <- function(graph) {
  nodes_df <- as_data_frame(graph, what = "vertices")
  edges_sf <- as_data_frame(graph, what = "edges")
  
  resource_edges <- convert_edges_to_power_flow(edges_sf)
  tbl_graph(nodes=nodes_df, edges=resource_edges, directed=TRUE)
}