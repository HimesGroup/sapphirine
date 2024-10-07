## Not in
"%ni%" <- function(x, table) match(x, table, nomatch = 0L) == 0L

.get_pal <- function(min_val, max_val, reverse = TRUE, palette = "Spectral") {
  colorNumeric(palette = palette, domain = c(min_val, max_val),
               na.color = "transparent", reverse = reverse)
}
