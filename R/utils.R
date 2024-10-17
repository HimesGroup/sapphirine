## Not in
"%ni%" <- function(x, table) match(x, table, nomatch = 0L) == 0L

.get_pal <- function(min_val, max_val, reverse = TRUE, palette = "Spectral") {
  colorNumeric(palette = palette, domain = c(min_val, max_val),
               na.color = "transparent", reverse = reverse)
}

.trend_plot <- function(data, title, fmt_x = "%{x}", fmt_y = "%{y:.3f}",
                        xlab = "Year", ylab = "Value") {
  data <- data[order(data$YEAR), ]
  plot_ly(data, x = ~ YEAR, y = ~ VALUE,
          type = "scatter", mode = "lines+markers",
          hovertemplate = paste0("<b>Year</b>: ", fmt_x,
                                 "<br><b>Value</b>: ", fmt_y,
                                 "<extra></extra>")) |>
    layout(title = title, xaxis = list(title = xlab), yaxis = list(title = ylab))
}
