library(shiny)
library(shinythemes)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggiraph)
library(igraph)
library(ggtext)
library(patchwork)
library(DT)

source("helpers/extract_subnetwork.R", local = TRUE)$value
source("helpers/plot_fishing_relationships.R", local = TRUE)$value
source("helpers/Settings.R", local = TRUE)$value
page_relationship_graph <- source("pages/relationship_graph.R", local = TRUE)$value
output_refNodeSelection <- source("outputs/refNodeSelection.R", local = TRUE)$value
output_refNode <- source("outputs/refNode.R", local = TRUE)$value

supernetwork <- readRDS("data/rds/supernetwork.rds")
node_names <- as_data_frame(supernetwork, what="vertices")$name

ui <- tagList(
  useShinyjs(),
  navbarPage(
    title = "Vast Challenge: Mini-challenge 3",
    fluid = TRUE,
    theme = shinytheme("cosmo"),
    page_relationship_graph,
  )
)

server <- function(input, output, session) {
  observeEvent(input$showFullNetwork, {
    toggleState(id = "distance", condition = !input$showFullNetwork)
  })
  
  output$refNodeSelection <- renderDT(
    output_refNodeSelection(input),
    selection = 'single',
    extensions = 'Scroller',
    colnames = rep("", ncol(output_refNodeSelection(input))),
    options = list(
      pageLength = 10,
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE,
      ordering = FALSE,
      language = list(
        search = '',
        searchPlaceholder = 'Search nodes'
      )
    )
  )
  
  refNode <- reactive({output_refNode(input)})
  distance <- reactive({
    if(input$showFullNetwork) {
      NULL
    } else {
      input$distance
    }
  })
  
  graph <- reactive({
    supernetwork %>%
      extract_subnetwork(node_name=refNode(), distance = distance())
  })
  
  nodes <- reactive({
    n <- as_data_frame(graph(), what = 'vertices')
    rownames(n) <- NULL
    
    if(nrow(n) > 0) {
      n %>% arrange(name) %>% select(name, alias, subtype)
    } else {
      NULL
    }
  })
  
  edges <- reactive({
    if(vcount(graph()) > 0) {
      as_data_frame(graph(), what = 'edges') %>%
        select(from, to, subtype, start_date, end_date)
    } else {
      NULL
    }
  })
  
  output$relationshipGraph <- renderGirafe({
      graph() %>%
      plot_fishing_relationships(
        emphasize_nodes=c(refNode(), nodes()$name[input$nodesList_rows_selected])
      )
  })
  
  output$nodesList <- renderDT(nodes())
  output$edgesList <- renderDT(edges(), selection = "none")
}

shinyApp(ui = ui, server = server)
