supernetwork <- readRDS("data/rds/supernetwork.rds")

function(input) {
  nodes <- as_data_frame(supernetwork, what = 'vertices')
  rownames(nodes) <- NULL
  
  if(!is.null(input$nodeSubtype)) {
    nodes <- nodes %>% filter(subtype == input$nodeSubtype)
  }
  
  nodes %>% select(name)
}