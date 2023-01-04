#' Compute present values from future payments (or cashflow)
#' using present worth factor (pwf) 
#'  
#' \code{Pr.value.fv} calculate present value from future payments  
#'     
#'Function for computing present value based on:
#'@input discount rate (dr)
#'@input life span in years (yr)
#'@input future payments (fv)
#'
#' @return Pr.value.fv 
#'
#'@references
#'Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
#'  
#' @examples  
#' # Result: Pr.value.fv
#' Pr.value.fr(0.08, 3000, 30)  
#' 

Pr.value <- function(dr, fv, yr){
  # compute present worth factor
  pwf <-1/((1+dr)^yr)
  
  # compute present value of costs
  Pr.value.fr <-(fv)(pwf)
  
  #Compute total present value
  sum(Pr.value.fr)
}