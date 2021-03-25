#' My Label Format
#'
#' Helper function to flip the legend labels on a `leaflet` map to increase vertically rather than decrease. Also allows for other forms of customization
#'
#' @param prefix Prefix (character) to add to legend labels
#' @param suffix Suffix (character) to add to legend labels
#' @param between Character to add between limits of numerically ranged legend labels, dash by default
#' @param digits Number of digits to which to round numeric legend labels
#' @param big.mark Character to add every 3 digits in large numeric legend labels, comma by default
#' @param transform Transformation to apply to numeric legend labels (e.g., log10), identity (none) by default
#' @param t.val Maximum value at which to cut off a numeric legend (to be used if all values of a raster layer beyond said maximum are coerced to the maximum value to eliminate outliers); will be given a suffix of +
#' @export

myLabelFormat = function(prefix = "", suffix = "", between = " &ndash; ", digits = 3,
                         big.mark = ",", transform = identity, t.val = Inf) {
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE,
           big.mark = big.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      cuts <- sort(cuts, decreasing = T) #just added
      paste0(prefix, formatNum(cuts), ifelse(cuts == t.val, "+", ""))
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]),
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n],
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}
