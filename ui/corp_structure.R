corp_side <- function(ns, subtypes) {
  sidebarPanel(
    width = 3,
    selectInput(
      ns("nodeSubtype"),
      "Select Reference Node",
      choices = subtypes,
      selected = "FishingCompany"
    ),
    DTOutput(ns("refNodeSelection")),
    strong("Network Size"),
    sliderInput(ns("distance"), "",
                min = 0,
                max = 3,
                value = 1),
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

corp_titleWell <- function(ns) {
  wellPanel(
    fluid = TRUE,
    h3("Corporate Structures"),
    p("Reflect the corporate structures in the fishing business network"),
  )
}

corp_main_panel <- function(ns) {
  mainPanel(
    width = 9,
    fluidRow(
      column(
        width = 4,
        corp_titleWell(ns),
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
        DTOutput(ns("nodesList"))
      ),
      tabPanel("Edges", DTOutput(ns("edgesList"))),
    )
  )
}

function(supernetwork, id) {
  ns = NS(id)
  subtypes <- as_data_frame(supernetwork, what = "vertices")$subtype %>% unique()
  tabPanel("Corporate Structure", sidebarLayout(corp_side(ns, subtypes), corp_main_panel(ns)))
}