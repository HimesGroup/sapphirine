addLegendEPA <- function(map, colors, labels, sizes, opacity = 0.8){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, 
                           "px;margin-top: 4.5px;height:", sizes, "px")  
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, 
                           "px;margin-top: 0px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity))
}