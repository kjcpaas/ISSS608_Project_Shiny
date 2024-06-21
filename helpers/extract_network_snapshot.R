extract_network_snapshot <- function(graph, datestring, delete = FALSE) {
  date <- as_date(datestring)
  
  graph_nodes = as_data_frame(graph, what = "vertices")
  graph_edges = as_data_frame(graph, what = "edges")
  
  if(is.na(date) || vcount(graph) == 0) {
    return(graph)
  }
  
  # Assume transition is at 12 AM of given date
  graph_edges <- graph_edges %>%
    mutate(
      included = ifelse(is.na(start_date) | (
        start_date <= date &
          (is.na(end_date) |
             end_date > date)
      ), 1, 0)
    )
  
  filtered_edges <- graph_edges %>% filter(included == 1)
    
  graph_nodes <- graph_nodes %>%
    mutate(included = (
      name %in% filtered_edges$from | name %in% filtered_edges$to
    ))
  
  if(!delete) {
    return(
      tbl_graph(nodes = graph_nodes,
                edges = graph_edges,
                directed = is_directed(graph))
    )
  }
  
  tbl_graph(nodes = graph_nodes %>% filter(included == 1),
            edges = filtered_edges,
            directed = is_directed(graph))
}