#' Calculate annual value from uniform gradient payment 
#'  
#' \code{gradient.to.future} Compute annual value from uniform gradient payments 
#'      using uniform gradient present worth factor (ugaw)
#'      
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param G uniform gradient payments
#'
#' @return ann.value
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
#'  
#' @examples  
#' # Result: ann.value = $
#' gradient.to.annual(0.08, 30, )  
#' 
#' @export
gradient.to.annual <- function(i, n, G){
  # compute uniform series present worth factor
  ugaw <- (n/i)-((n/(1+i)^n)-1)
  
  # compute present value of costs
  ann.value <- G * ugaw
  
  #Compute total present value
  sum(ann.value)
  
  #Send the output
  ann.value
  
}
