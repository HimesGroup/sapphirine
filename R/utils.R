## Not in
"%ni%" <- function(x, table) match(x, table, nomatch = 0L) == 0L

.get_pal <- function(min_val, max_val, reverse = TRUE, palette = "Spectral") {
  colorNumeric(palette = palette, domain = c(min_val, max_val),
               na.color = "transparent", reverse = reverse)
}

.draw_leaflet <- function(x, min_val, max_val, title = NULL,
                          zoom = 11, num_fmt = "%.3f",
                          project = FALSE, grid = TRUE,
                          col_reverse = TRUE, palette = "Spectral") {
  p <- leaflet() |>
    addTiles() |>
    setView(lng = -75.1652, lat = 39.9525, zoom = zoom) |>
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Recenter",
      onClick = JS("function(btn, map){map.setView([39.9525, -75.1652], 11);}")
    )) |>
    addLegend(
      position = "bottomright",
      pal = .get_pal(min_val, max_val, reverse = !col_reverse, palette = palette),
      values = c(min_val, max_val),
      labFormat = labelFormat(transform = function(k) sort(k, decreasing = TRUE)),
      title = title
    )
  if (grid) {
    p |>
      addRasterImage(
        as(x, "Raster"), colors = .get_pal(min_val, max_val, reverse = col_reverse,
                                           palette = palette),
        opacity = 0.6, project = project
      )
  } else {
    p |>
      addPolygons(
        data = st_transform(x, 4326),
        fillColor = ~.get_pal(min_val, max_val, reverse = col_reverse,
                              palette = palette)(VALUE),
        weight = 1, opacity = 1,
        color = "#444444",
        dashArray = NULL, fillOpacity = 0.6,
        highlightOptions = highlightOptions(
          weight = 3, color = "#444444", dashArray = NULL,
          fillOpacity = 0.9, bringToFront = FALSE
        ),
        layerId = ~ LOCATION,
        label = paste0(x$LOCATION, ": ", sprintf(num_fmt, x$VALUE))
      )
  }
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

.line_plot <- function(data, fmt_y = "%{y}", xlab = "Year", ylab = "Value") {
  data <- data[order(data$YEAR), ]
  plot_ly(data = data, x = ~ YEAR, y = ~ VALUE,
          color = ~ LOCATION, colors = "Set2",
          type = "scatter", mode = "lines+markers",
          hovertemplate = paste0(": ", fmt_y, "<extra></extra>")
          ) |>
    layout(xaxis = list(title = xlab), yaxis = list(title = ylab),
           hovermode = "x unified", legend = list(orientation = "h", y = -0.25))
}
