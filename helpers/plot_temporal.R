plot_temporal <- function(data, title = "Yearly Activities") {
  g <- ggplot(data, aes(x = year, y = count)) +
    geom_bar_interactive(
      aes(tooltip = paste0(year, ": ", count)),
      stat = "identity", width = 0.75,
      fill = "#007bff"
    ) +
    labs(title = title,
         x = "Year",
         y = "No. of activities") +
    scale_y_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    scale_x_continuous(breaks = breaks_pretty())+
    theme_minimal() +
    theme(
      text = element_text(family = "Roboto Condensed"),
      panel.grid.minor = element_blank(),
      axis.line = element_line(),
      axis.ticks = element_line(),
      plot.title = element_text(face = "bold"),
    )
  girafe(
    ggobj = g,
    options = list(
      opts_tooltip(css = STYLES$tooltip_css)
    )
  )
}