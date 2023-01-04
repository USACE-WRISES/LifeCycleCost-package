#' Compute annual payment value from future value 
#'  
#' \code{future.to.annual} calculate annual value from future value using
#'      the accumulated amount after years using sinking fund factor (sff) 
#'     
#' @param dr discount rate
#' @param span life span in years
#' @param fr.value accumulated (future) amount 
#'
#' @return ann.value 
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: ann.value 
#' future.to.annual(0.08, 30, 650000)  
#' 
#' @export
future.to.annual <- function(dr, span, fr.value){
  # compute sinking fund factor
  sff <- dr / ((1+dr)^span-1)
  
  # compute annual value of costs
  ann.value <- fr.value * sff
  
  #Send the output
  ann.value
}