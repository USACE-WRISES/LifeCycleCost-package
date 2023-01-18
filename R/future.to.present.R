#' Compute present values from future payments (or cashflow)
#'  
#' \code{future.to.present} calculate present value from future value using 
#'      present worth factor (pwf) 
#'
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param FV future value
#'
#' @return PV 
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: PV
#' future.to.present(0.08, 30, 30000)  
#' 
#' @export
future.to.present <- function(i, n, FV){
  # compute present worth factor
  pwf <-1/((1+i)^yr)
  
  # compute present value of costs
  PV <- FV * pwf
  
  #Compute total present value
  sum(PV)
  
  #Send the output
  PV
}
