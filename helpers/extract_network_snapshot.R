extract_network_snapshot <- function(graph, datestring) {
  date <- as_date(datestring)
  
  graph_nodes = as_data_frame(graph, what = "vertices")
  graph_edges = as_data_frame(graph, what = "edges")
  
  # Assume transition is at 12 AM of given date
  filtered_edges <- graph_edges %>%
    filter(is.na(start_date) | (
      start_date <= date &
        (is.na(end_date) |
           end_date > date)
    ))
  
  tbl_graph(nodes = graph_nodes,
            edges = filtered_edges,
            directed = is_directed(graph))
}