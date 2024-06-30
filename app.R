library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggiraph)
library(igraph)
library(ggtext)
library(DT)
library(scales)

source("helpers/Settings.R", local = TRUE)$value

corpGraphUI <- source("ui/corp_structure.R", local = TRUE)$value
corpGraphServer <- source("server/corp_structure.R", local = TRUE)$value
networkGraphUI <- source("ui/network_graph.R", local = TRUE)$value
networkGraphServer <- source("server/network_graph.R", local = TRUE)$value
stylesUI <- source("ui/styles.R", local = TRUE)$value
influenceGraphUI <- source("ui/influence_graph.R", local = TRUE)$value
influenceGraphServer <- source("server/influence_graph.R", local = TRUE)$value

supernetwork <- readRDS("data/rds/supernetwork.rds")

ui <- tagList(
  useShinyjs(),
  stylesUI,
  navbarPage(
    title = "The Big Fish",
    fluid = TRUE,
    inverse = TRUE,
    collapsible = TRUE,
    corpGraphUI(supernetwork, "corpGraph"),
    networkGraphUI(supernetwork, "networkGraph"),
    influenceGraphUI(supernetwork, "influenceGraph"),
  )
)

server <- function(input, output, session) {
  moduleServer("corpGraph", corpGraphServer, session = session)
  moduleServer("networkGraph", networkGraphServer, session = session)
  moduleServer("influenceGraph", influenceGraphServer, session = session)
}

shinyApp(ui = ui, server = server)