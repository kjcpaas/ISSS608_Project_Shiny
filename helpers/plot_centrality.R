plot_centrality <- function(graph,
                            # Column containing centrality measure, can be
                            # pagerank: for most powerful nodes
                            # betweeness: for power brokers
                            centrality_col,
                            # Name of nodes to emphasize
                            emphasize_nodes = c(),
                            # Date where to get the snapshot
                            datestring = NULL,
                            # Layout options
                            layout = "nicely",
                            circular = FALSE,
                            # Texts
                            title = NULL,
                            subtitle = NULL,
                            caption = paste("*Bigger and brighter nodes are more influential.", STYLES$default_caption),
                            # Plot styling
                            arrow_margin = STYLES$arrow_margin / 2,
                            edge_thickness = STYLES$base_edge_thickness,
                            # Seed
                            seed_num = CONFIGS$default_seed) {
  set.seed(seed_num)
  
  centrality_type = NULL
  
  if (endsWith(centrality_col, "pagerank")) {
    centrality_type = "pagerank"
  } else if (endsWith(centrality_col, "betweenness")) {
    centrality_type = "betweenness"
  }
  
  if (!(centrality_type %in% c("pagerank", "betweenness"))) {
    stop("Only pagerank and betweenness centralities are relevant in our analysis")
  }
  
  date <- NULL
  if(!is.null(datestring)) { date <- as_date(datestring) }
  
  nodes <- as_data_frame(graph, what = "vertices")
  score_boundary <- (nodes[[centrality_col]] %>% max()) * 0.6
  
  edges <- as_data_frame(graph, what = "edges")
  max_weight <- edges$weight %>% max()
  min_weight <- edges$weight %>% min()
  
  g <- ggraph(graph, layout = layout, circular = circular) +
    # Render nodes
    geom_point_interactive(
      aes(
        x = x,
        y = y,
        data_id = name,
        tooltip = sprintf("%s (%s)<br/>Score: %0.5f", name, subtype, .data[[centrality_col]]),
        # To show people as triangle, organizations as circle
        # See scale_shape_manual code below
        shape = supertype,
        # Get centrality measures from a column
        fill = .data[[centrality_col]],
        size = .data[[centrality_col]],
      ),
      # Thicken border if emphasized
      color = ifelse(
        nodes$name %in% emphasize_nodes,
        STYLES$node_emphasized_border_color,
        STYLES$node_border_color
      ),
      stroke = ifelse(
        nodes$name %in% emphasize_nodes,
        STYLES$node_emphasized_border_stroke,
        STYLES$node_border_stroke
      ),
    ) +
    geom_node_text(
      aes(label = alias),
      family = STYLES$font_family,
      size = STYLES$node_label_size,
      # Nodes with lower centrality have lighter nodes and have black text
      color = ifelse(
        nodes[[centrality_col]] < score_boundary,
        STYLES$node_label_dark,
        STYLES$node_label_light
      ),
      fontface = ifelse(nodes$name %in% emphasize_nodes, "bold", "plain"),
    ) +
    
    # Render edges. Use geom_edge fan so edges along the same path don't overlap
    geom_edge_fan(
      aes(color = subtype, edge_width = weight,
          # Will identify if the edge is active at this date, if not do not display
          # Ideally this should be in a function but I can't figure out how to make it work inside aes
          # Logic is same as is extract_network_snapshot.R
          filter = ifelse(is.null(date) | is.na(start_date), TRUE,
                          ifelse(start_date <= date & (is.na(end_date) | end_date > date),
                                 TRUE,
                                 FALSE
                          )
          )),
      strength = 0.5,
      arrow = STYLES$arrow_style,
      end_cap = circle(arrow_margin, "mm"),
      start_cap = circle(arrow_margin, "mm"),
      alpha = 0.8
    ) +
    scale_shape_manual(values = MAPPINGS$node_supertype_to_shape) +
    scale_edge_color_manual(values = MAPPINGS$edge_power_subtype_to_color) +
    
    # Centrality visualization
    scale_fill_gradient(
      high = ifelse(
        centrality_type == "pagerank",
        STYLES$primary_color,
        STYLES$secondary_color
      ),
      low = ifelse(
        centrality_type == "pagerank",
        STYLES$primary_light_color,
        STYLES$secondary_light_color
      )
    ) +
    scale_size_continuous(range = c(3, 12), guide = FALSE) +
    
    # Make sure edge widths are consistent across diff graphs
    scale_edge_width(
      range = c(min_weight * edge_thickness, max_weight * edge_thickness),
      guide = "none"
    ) +
    
    # Change legend names
    labs(
      fill = ifelse(
        centrality_type == "pagerank",
        "PageRank Score",
        "Betweenness Score"
      ),
      shape = "Node Supertypes",
      edge_color = "Edge Subtypes"
    ) +
    
    # Make sure the plot is not clipped
    scale_x_continuous(expand = expansion(mult = c(0.10, 0.10))) +
    scale_y_continuous(expand = expansion(mult = c(0.10, 0.10))) +
    
    # Style legend keys
    guides(
      shape = guide_legend(
        override.aes = list(
          size = 3,
          fill = STYLES$primary_color
        ),
        order = 1
      ),
      edge_color = guide_legend(order = 2),
      fill = guide_colorbar(order = 3)
    ) +
    
    # Style graph
    unset_graph_style() +
    theme_graph(base_family = STYLES$font_family,
                plot_margin = margin(0)) +
    
    plot_annotation(title = title,
                    subtitle = subtitle,
                    caption = caption) &
    COMMON_THEME
  
  girafe(
    ggobj = g,
    width_svg = STYLES$svg_width,
    height_svg = STYLES$svg_height,
    options = list(opts_tooltip(css = STYLES$tooltip_css))
  )
}