source("helpers/extract_subnetwork.R", local = TRUE)$value
source("helpers/extract_network_snapshot.R", local = TRUE)$value
source("helpers/convert_graph_to_power_flow.R", local = TRUE)$value
source("helpers/plot_centrality.R", local = TRUE)$value

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
  
  observeEvent(template_graph(), {
    need(vcount(template_graph()) > 0, "")
    e <- as_data_frame(template_graph(), what = "edges")
    
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
  
  # Subnetwork unfiltered by date
  subnetwork <- reactive({
    need(length(refNode()) > 0, "")
    
    supernetwork %>%
      extract_subnetwork(
        node_name = refNode(),
        distance = distance()
      )
  })
  
  template_graph <- reactive({
    need(length(refNode()) > 0, "")
    
    subnetwork() %>%
    convert_graph_to_power_flow() %>%
    extract_network_snapshot(snapshotDate())
  })
  
  influence_nodes <- reactive({
    template_graph() %>%
      extract_network_snapshot(snapshotDate(), delete = TRUE) %>%
      mutate(
        pagerank = centrality_pagerank(weights = E(.)$weight) %>% round(4),
        betweenness = centrality_betweenness(
          weights = E(.)$weight,
          normalized = TRUE
        ) %>% round(4)
      ) %>%
      as_data_frame(what="vertices")
  })
  
  nodes <- reactive({
    n <- influence_nodes()
    rownames(n) <- NULL
    
    if(nrow(n) == 0) {
      NULL
    } else if (input$plotType == "Power Holders") {
      n %>% arrange(-pagerank) %>% select(name, alias, subtype, pagerank)
    } else {
      n %>% arrange(-betweenness) %>% select(name, alias, subtype, betweenness)
    }
  })
  
  edges <- reactive({
    if(vcount(template_graph()) > 0) {
      as_data_frame(template_graph(), what = "edges") %>%
        filter(included == TRUE) %>%
        select(from, to, subtype, start_date, end_date)
    } else {
      NULL
    }
  })
  
  # =======================================================
  # =====================   OUTPUTS   =====================
  # =======================================================
  output$title <- renderText(input$plotType)
  output$subtitle <- renderText({
    if(input$plotType == "Power Holders") {
      "Calculates the influence of Power Holders using pagerank centrality."
    } else {
      "Calculates the influence of Power Brokers using betweenness centrality."
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
    
    centrality <- ifelse(
      input$plotType == "Power Holders",
      "pagerank",
      "betweenness"
    )
    
    template_graph() %>%
      left_join(influence_nodes()) %>%
      plot_centrality(
        centrality_col = centrality,
        emphasize_nodes=c(refNode(),
                          nodes()$name[input$nodesList_rows_selected])
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