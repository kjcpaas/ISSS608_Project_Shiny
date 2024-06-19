CONFIGS = list(
  default_seed = 4231 # For reproduceability
)

STYLES = list(
  font_family = "Roboto Condensed",
  # Colors
  primary_color = "blue",
  #primary_light_color = "#e6e6ff",
  primary_light_color = "white",
  secondary_color = "darkorange",
  secondary_light_color = "white",
  muted_color = "grey50",
  title_color = "grey25",
  normal_text_color = "grey50",
  # Graph
  node_size = 7.5,
  node_border_color = "grey50",
  node_border_stroke = 0.5,
  node_emphasized_border_color = "black",
  node_emphasized_border_stroke = 1,
  arrow_margin = 3.2,
  arrow_style = arrow(type = "closed", length = unit(2, "pt")),
  base_edge_thickness = 0.2,
  # Panel
  panel_border_color = "grey50",
  panel_border_thickness = 0.5,
  # Texts
  node_label_size = 2,
  node_label_dark = "black",
  node_label_light = "white",
  
  default_caption = "Hover on the nodes to see more details.",
  # Interactive elemnents
  tooltip_css = paste0(
    "background-color:black;color:white;",
    "font-family:Roboto Condensed;font-size:10pt;",
    "padding:4px;text-align:center;"
  ),
  svg_width = 6,
  svg_height = 6 * 0.618
)

MAPPINGS = list(
  # Available shapes: https://www.datanovia.com/en/blog/ggplot-point-shapes-best-tips/
  node_supertype_to_shape = c(
    "Person" = 24, # Triangle
    "Organization" = 21 # Circle
  ),
  # Color schemes
  # Colorblind pallettes from https://davidmathlogic.com/colorblind
  node_subtype_to_color = c(
    "Person" = "#44AA99",
    "CEO" = "#117733",
    "Company" = "#DDCC77",
    "FishingCompany" = "#88CCEE",
    "LogisticsCompany" = "#332288",
    "FinancialCompany" = "#AA4499",
    "NGO" = "#CC6677",
    "NewsCompany" = "#882255"
  ),
  edge_relationship_subtype_to_color = c(
    "WorksFor" = "#D81B60",
    "Shareholdership" = "#FFC107",
    "BeneficialOwnership" = "#004D40",
    "FamilyRelationship" = "#1E88E5"
  ),
  edge_power_subtype_to_color = c(
    "WorksFor" = "#D81B60",
    "HasShareholder" = "#FFC107",
    "OwnedBy" = "#004D40",
    "FamilyRelationship" = "#1E88E5"
  )
)

STYLES = list(
  font_family = "Roboto Condensed",
  # Colors
  primary_color = "blue",
  #primary_light_color = "#e6e6ff",
  primary_light_color = "white",
  secondary_color = "darkorange",
  secondary_light_color = "white",
  muted_color = "grey50",
  title_color = "grey25",
  normal_text_color = "grey50",
  # Graph
  node_size = 7.5,
  node_border_color = "grey50",
  node_border_stroke = 0.5,
  node_emphasized_border_color = "black",
  node_emphasized_border_stroke = 1,
  arrow_margin = 3.2,
  arrow_style = arrow(type = "closed", length = unit(2, "pt")),
  base_edge_thickness = 0.2,
  # Panel
  panel_border_color = "grey50",
  panel_border_thickness = 0.5,
  # Texts
  node_label_size = 2,
  node_label_dark = "black",
  node_label_light = "white",
  
  default_caption = "Hover on the nodes to see more details.",
  # Interactive elemnents
  tooltip_css = paste0(
    "background-color:black;color:white;",
    "font-family:Roboto Condensed;font-size:10pt;",
    "padding:4px;text-align:center;"
  ),
  svg_width = 6,
  svg_height = 6 * 0.618
)

COMMON_THEME = theme(
  text = element_text(family = STYLES$font_family, color = STYLES$normal_text_color),
  plot.title = element_markdown(
    color = STYLES$title_color,
    face = "bold",
    size = unit(10, "pt")
  ),
  plot.subtitle = element_markdown(size = unit(8, "pt")),
  plot.caption = element_markdown(size = unit(6, "pt"), hjust = 0),
  plot.margin = margin(2, 0, 0, 0, unit = "pt"),
  
  # Legend styles
  legend.position = "right",
  legend.location = "plot",
  legend.justification = "top",
  legend.title = element_markdown(
    color = STYLES$title_color,
    face = "bold",
    size = unit(8, "pt")
  ),
  legend.text = element_text(size = unit(6, "pt"), vjust = 0.5),
  legend.box.spacing = unit(4, "pt"),
  legend.margin = margin(0),
  legend.spacing.x = unit(2, "pt"),
  legend.spacing.y = unit(8, "pt"),
  legend.key.size = unit(12, "pt"),
  
  panel.border = element_rect(
    color = STYLES$panel_border_color,
    fill = NA,
    linewidth = STYLES$panel_border_thickness
  )
)