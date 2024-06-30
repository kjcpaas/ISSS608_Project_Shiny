source("helpers/extract_subnetwork.R", local = TRUE)$value
source("helpers/extract_network_snapshot.R", local = TRUE)$value
source("helpers/convert_graph_to_power_flow.R", local = TRUE)$value
source("helpers/plot_fishing_relationships.R", local = TRUE)$value
source("helpers/plot_fishing_power.R", local = TRUE)$value
source("helpers/plot_temporal.R", local = TRUE)$value
source("helpers/get_activities_by_year.R", local = TRUE)$value

supernetwork <- readRDS("data/rds/supernetwork.rds")
# Do not allow full network render for these nodes as the network has 23k+ nodes
# It will crash the Shiny server if we attempt to visualize
large_network_list <- readRDS("data/rds/large_network_list.rds")

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
  
  observeEvent(refNode(), {
    if(refNode() %in% large_network_list) {
      updateCheckboxInput("showFullNetwork", label = "Full network too big to visualize", value = FALSE, session = session)
      disable("showFullNetwork")
    } else {
      updateCheckboxInput("showFullNetwork", label = "Show all connected nodes", session = session)
      enable("showFullNetwork")
    }
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
    ifelse(input$showFullNetwork && !(refNode() %in% large_network_list), -1, input$distance)
  })
  
  snapshotDate <- reactive({
    ifelse(input$showAllTimeConnections, "", input$snapshotDate)
  })
  
  # Subnetwork unfiltered by date
  subnetwork <- reactive({
    need(length(refNode()) > 0, "")
    
    supernetwork %>%
      extract_subnetwork(
        node_name = refNode(),
        distance = distance()
      )
  })
  
  # Generate graph
  graph <- reactive({
    need(length(refNode()) > 0, "")
    
    g <- subnetwork() %>% extract_network_snapshot(snapshotDate())
    
    if(input$plotType == "Power") {
      g <- g %>% convert_graph_to_power_flow()
    }
    
    g
  })
  
  nodes <- reactive({
    n <- as_data_frame(graph(), what = "vertices") %>% filter(included == TRUE)
    rownames(n) <- NULL
    
    if(nrow(n) > 0) {
      n %>% arrange(name) %>% select(name, alias, subtype)
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
  output$title <- renderText(paste(input$plotType, "Plot"))
  output$subtitle <- renderText({
    if(input$plotType == "Power") {
      "Shows the power dynamic in the network. Arrow points to the more powerful entity. Used for Influence Graphs."
    } else {
      "Shows the relationship in the network from the Mini-Challenge 3 Data."
    }
  })
  
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
  
  output$temporal <- renderGirafe({
    validate(
      need(length(refNode()) > 0, "Please select a node to start")
    )
    
    subnetwork() %>%
      get_activities_by_year() %>%
      plot_temporal(title = "Yearly Activities for this Network")
  })
  
  output$plot <- renderGirafe({
    validate(
      need(length(refNode()) > 0, "Please select a node to start")
    )
    
    pl <- if(input$plotType == "Power") {
      graph() %>%
        plot_fishing_power(
          emphasize_nodes=c(refNode(), nodes()$name[input$nodesList_rows_selected])
        )
    } else {
      graph() %>%
        plot_fishing_relationships(
          emphasize_nodes=c(refNode(), nodes()$name[input$nodesList_rows_selected])
        )
    }
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