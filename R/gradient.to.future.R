#' Calculate future value from uniform gradient payment 
#'  
#' \code{gradient.to.future} Compute future value from uniform gradient payments 
#'      using uniform gradient present worth factor (ugfw)
#'      
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param G uniform gradient payments
#'
#' @return fr.value
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
#'  
#' @examples  
#' # Result: fr.value = $ 312,312
#' gradient.to.future(0.08, 30,300)  
#' 
#' @export
gradient.to.future <- function(i, n, G){
  # compute uniform series present worth factor
  ugfw <-(((1+i)^n-1) / (i^2))-(n/i)
  
  # compute present value of costs
  fr.value <- G * ugfw
  
  #Compute total present value
  sum(fr.value)
  
  #Send the output
  fr.value
  
}
