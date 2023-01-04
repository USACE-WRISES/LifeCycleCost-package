#' Compute Annual Values using capital recovery factor 
#'  
#' \code{present.to.annual} calculate uniform series annual value 
#'     
#'Function for computing capital recovery factor (crf)
#'@input discount rate (dr)
#'@input life span in years (yr)
#'@input cost of present value (PValue)
#'
#' @return ann.value 
#'
#' @references
#'
#'  
#' @examples
#' # Result: ann.value 
#' ann.value(0.08, 2000, 30)  
#' 
    
ann.value <- function(dr, PValue, yr){
  # compute recovery factor
  crf <-(dr(1+dr)^yr)/((1+dr)^yr-1)
  
  # compute annual costs
  annual <-(PValue)(crf)
}

ann.value(0.08, 2000, 30)

