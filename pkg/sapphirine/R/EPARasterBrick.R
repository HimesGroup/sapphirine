#' EPARasterBrick Class
#' 
#' A RasterBrick with an extra slot added for EPA monitors data frame
#' 
#' @export

setClass('EPARasterBrick', contains = 'RasterBrick',
         slots = c(monitor_data = 'list'))