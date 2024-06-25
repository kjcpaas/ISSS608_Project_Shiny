ig_side <- function(ns, subtypes) {
  sidebarPanel(
    width = 3,
    radioGroupButtons(
      ns("plotType"),
      label = "Plot",
      status = "primary",
      justified = TRUE,
      choices = c("Power Holders", "Power Brokers"),
      selected = "Power Holders",
      size = "sm"
    ),
    selectInput(
      ns("nodeSubtype"),
      "Select Reference Node",
      choices = subtypes,
      selected = "FishingCompany"
    ),
    DTOutput(ns("refNodeSelection")),
    strong("Distance from Reference"),
    sliderInput(ns("distance"), "",
                min = 0,
                max = 10,
                value = 3),
    checkboxInput(
      ns("showFullNetwork"),
      "Show all connected nodes",
      value = FALSE
    ),
    div(
      strong("Warning: "),
      "Large networks may take a while to render",
      class = "alert alert-warning plot-instruction"
    ),
    sliderTextInput(
      ns("snapshotDate"),
      label = "Select Date", 
      choices = c(Sys.Date()),
      force_edges = TRUE
    ),
    checkboxInput(
      ns("showAllTimeConnections"),
      "Show all-time connections",
      value = TRUE
    )
  )
}

ig_titleWell <- function(ns) {
  wellPanel(
    fluid = TRUE,
    textOutput(ns("title"), container = h3),
    textOutput(ns("subtitle"), container = p),
  )
}

ig_main_panel <- function(ns) {
  mainPanel(
    width = 9,
    fluidRow(
      column(
        width = 4,
        ig_titleWell(ns),
        girafeOutput(ns("temporal"))
      ),
      column(
        width = 8,
        girafeOutput(ns("plot")),
        em("*Hover on the nodes on the plot to see more details.", class = "text-muted")
      ),
    ),
    tabsetPanel(
      tabPanel(
        "Nodes",
        div(
          "Select nodes from the table to highlight them in the plot.",
          class = "alert alert-info plot-instruction"
        ),
        DTOutput(ns("nodesList")),
      ),
      tabPanel("Edges", DTOutput(ns("edgesList"))),
    )
  )
}

function(supernetwork, id) {
  ns = NS(id)
  subtypes <- as_data_frame(supernetwork, what = "vertices")$subtype %>% unique()
  
  tabPanel("Influence Graphs", sidebarLayout(ig_side(ns, subtypes), ig_main_panel(ns)))
}