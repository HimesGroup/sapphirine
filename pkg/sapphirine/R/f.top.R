#' Function
#' @export
#' @examples
#' f.top()

f.top <- function(x){
  no.string <- toString(as.integer(x))
  lead.digit <- as.numeric(substr(no.string, 1, 1))
  no.digits <- nchar(no.string)
  if(lead.digit == 1){
    if(x == 100){
      return(100)
    }
    else{
      return(RoundTo(x, multiple = 2*10**(no.digits - 2), FUN = ceiling))
    }
  }
  else if(lead.digit >= 2 && lead.digit <= 4){
    return(RoundTo(x, multiple = 5*10**(no.digits - 2), FUN = ceiling))
  }
  else if(lead.digit >= 5){
    return(RoundTo(x, multiple = 10**(no.digits - 1), FUN = ceiling))
  }
}
