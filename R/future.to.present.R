#' Compute present values from future payments (or cashflow)
#'  
#' \code{future.to.present} calculate present value from future value using 
#'      present worth factor (pwf) 
#'
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param FV future value
#'
#' @return pr.value 
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: pr.value = 2981.32
#' future.to.present(0.08, 30, 30000)  
#' 
#' @export
future.to.present <- function(i, n, FV){
  # compute present worth factor
  pwf <-1/((1+i)^n)
  
  # compute present value of costs
  pr.value <- FV * pwf
  
  #Compute total present value
  sum(pr.value)
  
  #Send the output
  pr.value
}
