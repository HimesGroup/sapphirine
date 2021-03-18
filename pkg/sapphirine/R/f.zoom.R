#' Function
#' @export
#' @examples
#' f.zoom()

f.zoom <- function(x, y){
  val <- ifelse(x > y, x, y)
  return(as.integer(round(11.47 - 1.5*val, digits = 0)))
}
