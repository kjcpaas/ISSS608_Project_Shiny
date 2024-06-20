side <- function(ns, subtypes) {
  sidebarPanel(width = 3,
    selectInput(
      ns("nodeSubtype"),
      "Reference Node",
      choices = subtypes,
      selected = "FishingCompany"
    ),
    DTOutput(ns("refNodeSelection")),
    sliderInput(ns("distance"), "Network Size (by distance)",
                min = 0,
                max = 10,
                value = 3),
    checkboxInput(ns("showFullNetwork"), "All connected nodes (may take long to render)", value = FALSE),
    
    dateInput(ns("snapshotDate"), "Network State on Date", value = "2035-05-25"),
    disabled(checkboxInput(ns("filterByDate"), "Filter by Date", value = FALSE, width = NULL)),
  )
}

main_panel <- function(ns) {
  mainPanel(
    width = 9,
    fluidRow(
      column(
        width = 4,
        "Title"
      ),
      column(
        width = 8,
        girafeOutput(ns("plot"))
      ),
    ),
    tabsetPanel(
      tabPanel("Nodes", DTOutput(ns("nodesList"))),
      tabPanel("Edges", DTOutput(ns("edgesList"))),
    )
  )
}

function(supernetwork, id) {
  ns = NS(id)
  subtypes <- as_data_frame(supernetwork, what = "vertices")$subtype %>% unique()
  
  tabPanel("Network Graph", sidebarLayout(side(ns, subtypes), main_panel(ns)))
}