supernetwork <- readRDS("data/rds/supernetwork.rds")
subtypes <- as_data_frame(supernetwork, what = 'vertices')$subtype %>% unique()

tabPanel(
  "Relationship Graph",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "nodeSubtype",
        "Select Node Subtype",
        choices = subtypes,
        selected = "FishingCompany"
      ),
      strong("Select Reference Node"),
      DTOutput('refNodeSelection'),
      sliderInput("distance", "Distance from Reference Node",
                  min = 0,
                  max = 10,
                  value = 3),
      checkboxInput("showFullNetwork", "Show full network (may take long to render)", value = FALSE, width = NULL),
      disabled(checkboxInput("filterByDate", "Filter by Date", value = FALSE, width = NULL)),
      dateInput("snapshotDate", "Select date", value = "2035-05-25")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(
          width = 7,
          girafeOutput('relationshipGraph')
        ),
        column(
          width = 5,
          DTOutput('nodesList')
        )
      ),
      DTOutput('edgesList')
    )
  )
)
