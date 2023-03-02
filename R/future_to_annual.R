#' Compute annual payment value from future value 
#'  
#' \code{future_to_annual} calculate annual value from future value using
#'      the accumulated amount after years using sinking fund factor (sff) 
#'     
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param FV accumulated (future) value 
#'
#' @return A 
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: A = 5738
#' future_to_annual(0.08, 30, 650000)  
#' 
#' @export
future_to_annual <- function(i, n, FV){
  # compute sinking fund factor
  sff <- i / ((1+i)^n-1)
  
  # compute annual value of costs
  A <- FV * sff
  
  #Send the output
  A
}