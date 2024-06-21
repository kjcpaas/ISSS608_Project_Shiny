library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggiraph)
library(igraph)
library(ggtext)
library(patchwork)
library(DT)

source("helpers/Settings.R", local = TRUE)$value

networkGraphUI <- source("ui/network_graph.R", local = TRUE)$value
networkGraphServer <- source("server/network_graph.R", local = TRUE)$value

supernetwork <- readRDS("data/rds/supernetwork.rds")

ui <- tagList(
  useShinyjs(),
  navbarPage(
    title = "Vast Challenge: Mini-challenge 3",
    fluid = TRUE,
    theme = shinytheme("cosmo"),
    networkGraphUI(supernetwork, "networkGraph"),
  )
)

server <- function(input, output, session) {
  moduleServer("networkGraph", networkGraphServer, session = session)
}

shinyApp(ui = ui, server = server)