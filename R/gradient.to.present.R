#' Calculate present value from uniform gradient payment 
#'  
#' \code{gradient.to.present} Compute present value from uniform gradient payments 
#'      using uniform gradient present worth factor (ugpw)
#'      
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param G uniform gradient payments
#'
#' @return pr.value
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
#'  
#' @examples  
#' # Result: pr.value =
#' gradient.to.present(0.08, 30, )  
#' 
#' @export
gradient.to.present <- function(i, n, G){
  # compute uniform series present worth factor
  ugpw <-(((1+i)^n-1) / (i^2 *(1+i)^n))-(n/(i*(1+i)^n))
  
  # compute present value of costs
  pr.value <- G * ugpw
  
  #Compute total present value
  sum(pr.value)
  
  #Send the output
  pr.value
  
}
