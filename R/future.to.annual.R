#' Compute annual value from accumulated amount after years
#' using sinking fund factor (sff) 
#'  
#' \code{future.to.annual} calculate annual value from future value 
#'     
#'Function for computing annual value based on:
#'@input discount rate (dr)
#'@input life span in years (yr)
#'@input accumulated(future)amount (fr.value)
#'
#' @return ann.value 
#'
#' @references
#'
#'  
#' @examples  
#' # Result: ann.value 
#' ann.value(0.08, 650000, 30)  
#' 

ann.value <- function(dr, fr.value, yr){
  # compute sinking fund factor
  sff <- dr/((1+dr)^yr-1)
  
  # compute annual value of costs
  ann.value <-(fr.value)(caf)
}