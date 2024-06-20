source("helpers/extract_subnetwork.R", local = TRUE)$value
source("helpers/extract_network_snapshot.R", local = TRUE)$value
source("helpers/plot_fishing_relationships.R", local = TRUE)$value

supernetwork <- readRDS("data/rds/supernetwork.rds")
all_nodes <- as_data_frame(supernetwork, what = 'vertices')

function(input, output, session) {
  # Activate snapshot selector
  observeEvent(input$refNodeSelection_row_last_clicked, {
    toggleState(id = "filterByDate", condition = !is.null(input$refNodeSelection_row_last_clicked))
  })
  
  # Enable/disable related inputs when checkboxes are toggled
  observeEvent(input$showFullNetwork, {
    toggleState(id = "distance", condition = !input$showFullNetwork)
  })
  
  observeEvent(input$filterByDate, {
    toggleState(id = "snapshotDate", condition = input$filterByDate)
  })
  
  refNodeItems <- reactive({
    nodes <- as_data_frame(supernetwork, what = 'vertices')
    rownames(nodes) <- NULL
    
    if(!is.null(input$nodeSubtype)) {
      nodes <- nodes %>% filter(subtype == input$nodeSubtype)
    }
    
    nodes %>% select(name)
  })
  
  refNode <- reactive({
    if(length(input$refNodeSelection_row_last_clicked) > 0) {
      refNodeItems()$name[input$refNodeSelection_row_last_clicked]
    } else {
      NULL
    }
  })
  
  distance <- reactive({
    ifelse(input$showFullNetwork, -1, input$distance)
  })
  
  snapshotDate <- reactive({
    ifelse(input$filterByDate, input$snapshotDate, "")
  })
  
  # Generate graph
  graph <- reactive({
    need(length(refNode()) > 0, "Select a reference node to start.")
    
    supernetwork %>%
      extract_subnetwork(
        node_name = refNode(),
        distance = distance()
      ) %>%
      extract_network_snapshot(snapshotDate())
  })
  
  nodes <- reactive({
    n <- as_data_frame(graph(), what = 'vertices') %>% filter(included == TRUE)
    rownames(n) <- NULL
    
    if(nrow(n) > 0) {
      n %>% arrange(name) %>% select(name, subtype)
    } else {
      NULL
    }
  })
  
  edges <- reactive({
    if(vcount(graph()) > 0) {
      as_data_frame(graph(), what = 'edges') %>%
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
    selection = 'single',
    extensions = 'Scroller',
    rownames = FALSE,
    colnames = rep("", ncol(refNodeItems())),
    options = list(
      dom = "ft",
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE,
      ordering = FALSE,
      language = list(search = '', searchPlaceholder = 'Search nodes')
    )
  )
  
  output$plot <- renderGirafe({
    validate(
      need(length(refNode()) > 0, "Please select a node to start")
    )
    graph() %>%
      plot_fishing_relationships(
        emphasize_nodes=c(refNode(), nodes()$name[input$nodesList_rows_selected])
      )
  })
  
  output$nodesList <- renderDT({
    validate(
      need(length(refNode()) > 0, "Network nodes will show here.")
    )
    
    datatable(
      nodes(),
      rownames = TRUE,
      extensions = 'Scroller',
      options = list(
        dom = "ift",
        deferRender = TRUE,
        scrollY = 300,
        scroller = TRUE,
        language = list(search = '', searchPlaceholder = 'Search')
      )
    )
  })
  
  output$edgesList <- renderDT(
    edges(),
    selection = "none",
    rownames = FALSE,
    extensions = 'Scroller',
    options = list(
      dom = "ift",
      deferRender = TRUE,
      scrollY = 300,
      scroller = TRUE,
      language = list(
        search = '',
        searchPlaceholder = 'Search'
      )
    )
  )
}