output_refNodeSelection <- source("outputs/refNodeSelection.R", local = TRUE)$value

supernetwork <- readRDS("data/rds/supernetwork.rds")

function(input) {
  nodes <- output_refNodeSelection(input)
  
  if(length(input$refNodeSelection_row_last_clicked) > 0) {
    nodes$name[input$refNodeSelection_row_last_clicked]
  } else {
    NULL
  }
}