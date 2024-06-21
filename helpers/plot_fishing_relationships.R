plot_fishing_relationships <- function(graph,
                                       # Name of nodes to emphasize
                                       emphasize_nodes = c(),
                                       # Layout options
                                       layout = "nicely",
                                       circular = FALSE,
                                       # Texts
                                       title = NULL,
                                       subtitle = NULL,
                                       instruction = STYLES$default_instruction,
                                       # Plot styling
                                       node_size = STYLES$node_size,
                                       arrow_margin = STYLES$arrow_margin,
                                       # Seed
                                       seed_num = CONFIGS$default_seed) {
  set.seed(seed_num)
  
  if(vcount(graph) == 0) {
    return(NULL)
  }
  
  nodes <- as_data_frame(graph, what = "vertices")
  
  g <- ggraph(graph, layout = layout, circular = circular) +
    annotate("label", label=instruction,
      x=-Inf, y=-Inf, hjust=0, vjust=0,
      color = STYLES$muted_color,
      family = STYLES$font_family, size = STYLES$instruction_size, fontface = "italic"
    ) +
    # Render nodes
    geom_point_interactive(
      aes(
        x = x,
        y = y,
        # Replace ' as it causes tooltips to not render correctly in JS
        data_id = sub("'", "&#39;", name),
        tooltip = sprintf("%s<br/>(%s)", sub("'", "&#39;", name), subtype),
        fill = subtype,
        # To show people as triangle, organizations as circle
        # See scale_shape_manual code below
        shape = supertype,
      ),
      alpha = nodes$included,
      size = node_size,
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
      color = STYLES$node_label_light,
      fontface = ifelse(nodes$name %in% emphasize_nodes, "bold", "plain"),
      alpha = nodes$included,
    ) +
    
    # Render edges. Use geom_edge fan so edges along the same path don't overlap
    geom_edge_fan(
      aes(
        color = subtype,
        filter = ifelse(included == 1, TRUE, FALSE)
      ),
      strength = 0.5,
      arrow = STYLES$arrow_style,
      end_cap = circle(arrow_margin, "mm"),
      start_cap = circle(arrow_margin, "mm"),
      alpha = 0.8
    ) +
    scale_shape_manual(values = MAPPINGS$node_supertype_to_shape) +
    scale_fill_manual(values = MAPPINGS$node_subtype_to_color) +
    scale_edge_color_manual(values = MAPPINGS$edge_relationship_subtype_to_color) +
    
    # Change legend names
    labs(shape = "Node Supertypes",
         fill = "Node Subtypes",
         edge_color = "Edge Subtypes") +
    
    # Make sure the plot is not clipped
    scale_x_continuous(expand = expansion(mult = c(0.10, 0.10))) +
    scale_y_continuous(expand = expansion(mult = c(0.10, 0.10))) +
    
    # Style legend keys
    guides(
      shape = guide_legend(
        override.aes = list(size = 3, fill = STYLES$primary_color),
        order = 1
      ),
      fill = guide_legend(
        override.aes = list(
          size = 4,
          shape = 22,
          color = NA
        ),
        order = 2,
      ),
      edge_color = guide_legend(order = 3),
    ) +
    
    # Style graph
    unset_graph_style() +
    theme_graph(base_family = STYLES$font_family,
                plot_margin = margin(0)) +
    
    plot_annotation(title = title,
                    subtitle = subtitle) &
    COMMON_THEME
  
  g <- girafe(
    ggobj = g,
    width_svg = STYLES$svg_width,
    height_svg = STYLES$svg_height,
    options = list(opts_tooltip(css = STYLES$tooltip_css))
  )
  girafe_options(g,
                 opts_sizing(rescale = TRUE),
                 opts_selection(type = "none"),
                 opts_zoom(min = 1, max = 5))
}