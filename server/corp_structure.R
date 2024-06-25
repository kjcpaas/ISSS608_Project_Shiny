source("helpers/extract_subnetwork.R", local = TRUE)$value
source("helpers/extract_network_snapshot.R", local = TRUE)$value
source("helpers/convert_graph_to_power_flow.R", local = TRUE)$value
source("helpers/plot_fishing_relationships.R", local = TRUE)$value
source("helpers/plot_temporal.R", local = TRUE)$value

supernetwork <- readRDS("data/rds/supernetwork.rds")

all_nodes <- as_data_frame(supernetwork, what = "vertices")

function(input, output, session) {
  # Enable/disable related inputs when checkboxes are toggled
  observeEvent(input$showFullNetwork, {
    toggleState("distance", condition = !input$showFullNetwork)
  })
  
  # Enable/disable related inputs when checkboxes are toggled
  observeEvent(input$showAllTimeConnections, {
    toggleState("snapshotDate", condition = !input$showAllTimeConnections)
  })
  
  observeEvent(graph(), {
    need(vcount(graph()) > 0, "")
    e <- as_data_frame(graph(), what = "edges")
    
    dates <- c(
      as.Date(e$start_date),
      as.Date(e$end_date)
    ) %>% sort() %>% unique()
    
    # Make sure slider has at least 2 values
    if(length(dates) == 0) {
      dates <- c(c(Sys.Date()))
    }
    
    if(length(dates) == 1) {
      dates = c(dates[1], dates[1])
    }
    
    updateSliderTextInput(
      "snapshotDate",
      choices = dates,
      selected = NULL,
      session = session
    )
  })
  
  # =======================================================
  # =================   REACTIVE VALUES   =================
  # =======================================================
  
  refNodeItems <- reactive({
    nodes <- as_data_frame(supernetwork, what = "vertices")
    rownames(nodes) <- NULL
    
    if(!is.null(input$nodeSubtype)) {
      nodes <- nodes %>% filter(subtype == input$nodeSubtype)
    }
    
    degree_df <- read.csv("./data/degree.csv")
    nodes <- left_join(nodes, degree_df, by = "name")
    nodes <- nodes %>%
      arrange(desc(degree))
    nodes %>% select(name)
  })
  
  refNode <- reactive({
    if(length(input$refNodeSelection_rows_selected) > 0) {
      refNodeItems()$name[input$refNodeSelection_rows_selected[1]]
    } else {
      NULL
    }
  })
  
  distance <- reactive({
    ifelse(input$showFullNetwork, -1, input$distance)
  })
  
  snapshotDate <- reactive({
    ifelse(input$showAllTimeConnections, "", input$snapshotDate)
  })
  
  # Generate graph
  graph <- reactive({
    need(length(refNode()) > 0, "")
    
    supernetwork %>%
      extract_subnetwork(
        node_name = refNode(),
        distance = distance()
      ) %>%
      extract_network_snapshot(snapshotDate())
  })
  
  nodes <- reactive({
    n <- as_data_frame(graph(), what = "vertices") %>% filter(included == TRUE)
    n <- n %>% filter(supertype=='Organization')
    rownames(n) <- NULL
    company_df <- read.csv("./data/company.csv")
    n <- left_join(n, company_df, by = "name")
    
    if(nrow(n) > 0) {
      n %>% arrange(name) %>% select(name, alias, ProductServices)
    } else {
      NULL
    }
  })
  
  edges <- reactive({
    if(vcount(graph()) > 0) {
      as_data_frame(graph(), what = "edges") %>%
        filter(included == TRUE) %>%
        select(from, to, subtype, start_date, end_date)
    } else {
      NULL
    }
  })
  
  
  # =======================================================
  # =====================   OUTPUTS   =====================
  # =======================================================
  output$refNodeSelection <- renderDT(
    refNodeItems(),
    selection = list(mode = 'single', selected = c(1)),
    extensions = "Scroller",
    rownames = FALSE,
    colnames = rep("", ncol(refNodeItems())),
    class = "compact row-border hover",
    options = list(
      dom = "ft",
      deferRender = TRUE,
      scrollY = 180,
      scroller = TRUE,
      ordering = FALSE,
      language = list(search = "", searchPlaceholder = "Search nodes")
    )
  )
  
  output$plot <- renderGirafe({
    validate(
      need(length(refNode()) > 0, "Please select a node to start")
    )
    
    graph() %>%
      plot_fishing_relationships(emphasize_nodes = c(refNode(), nodes()$name[input$nodesList_rows_selected]))
  })
  
  output$temporal <- renderGirafe({
    validate(
      need(length(refNode()) > 0, "Please select a node to start")
    )
    data <- read.csv("./data/transaction.csv")
    grouped_data <- data %>%
      filter(source== refNode() | target==refNode()) %>%
      group_by(year) %>%
      summarize(count = n())
    plot_temporal(
      grouped_data,
      title = paste0("Yearly Activities for ", refNode())
    )
  })
  
  output$nodesList <- renderDT({
    validate(
      need(length(refNode()) > 0, "Network nodes will show here.")
    )
    
    datatable(
      nodes(),
      rownames = TRUE,
      extensions = "Scroller",
      class = "compact hover row-border",
      options = list(
        dom = "ift",
        deferRender = TRUE,
        scrollY = 240,
        scroller = TRUE,
        language = list(search = "", searchPlaceholder = "Search")
      )
    )
  })
  
  output$edgesList <- renderDT(
    edges(),
    selection = "none",
    extensions = "Scroller",
    class = "compact hover row-border",
    options = list(
      dom = "ift",
      deferRender = TRUE,
      scrollY = 240,
      scroller = TRUE,
      language = list(
        search = "",
        searchPlaceholder = "Search"
      )
    )
  )
}