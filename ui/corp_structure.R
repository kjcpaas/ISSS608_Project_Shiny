corp_side <- function(ns, subtypes) {
  sidebarPanel(
    width = 3,
    radioGroupButtons(
      ns("plotType"),
      label = "Plot",
      status = "primary",
      justified = TRUE,
      choices = c("Relationship", "Power"),
      selected = "Relationship",
      size = "sm"
    ),
    selectInput(
      ns("nodeSubtype"),
      "Select Reference Node",
      choices = subtypes,
      selected = "FishingCompany"
    ),
    DTOutput(ns("refNodeSelection")),
    strong("Network Size"),
    helpText("Large networks may take a while to render"),
    sliderInput(ns("distance"), "",
                min = 0,
                max = 3,
                value = 1),
    checkboxInput(
      ns("showFullNetwork"),
      "Show all connected nodes",
      value = FALSE
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
    textOutput(ns("title"), container = h3),
    textOutput(ns("subtitle"), container = p),
    tags$ul(
      tags$li("Set the parameters for the plot on the left."),
      tags$li("Hover on the nodes on the plot to see more details."),
      tags$li("Explore nodes and edges in the tables at the bottom."),
      tags$li("Select nodes from the table to highlight them in the plot.")
    )
  )
}

corp_main_panel <- function(ns) {
  mainPanel(
    width = 9,
    fluidRow(
      column(
        width = 12,
        corp_titleWell(ns),
      ),
    ),
    fluidRow(
      column(
        width = 6,
        girafeOutput(ns("plot"))
      ),
      column(
        width = 6,
        girafeOutput(ns("temporal"))
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
  tabPanel("Corporate Structure", sidebarLayout(corp_side(ns, subtypes), corp_main_panel(ns)))
}