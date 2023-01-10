#' Compute present values from future payments (or cashflow)
#'  
#' \code{future.to.present} calculate present value from future value using 
#'      present worth factor (pwf) 
#'
#' @param dr discount rate in percent per year
#' @param span life span in years
#' @param fr.value future value
#'
#' @return Pr.value 
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: Pr.value
#' future.to.present(0.08, 30, 30000)  
#' 
#' @export
future.to.present <- function(dr, span, fr.value){
  # compute present worth factor
  pwf <-1/((1+dr)^yr)
  
  # compute present value of costs
  Pr.value <- fr.value * pwf
  
  #Compute total present value
  sum(Pr.value)
  
  #Send the output
  pr.value
}