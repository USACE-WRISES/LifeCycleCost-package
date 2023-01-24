#' Compute future values from present value
#' 
#' \code{present.to.future} calculate future value from present value using
#'      compound amount factor (caf)   
#'     
#' @param i discount rate in percent per year
#' @param n life span in years 
#' @param PV present value
#'
#' @return fr.value
#'
#' @references
#'Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: fr.value 
#' present.to.future (0.08, 30, 900000)  
#' 
#' @export
present.to.future <- function(i, n, PV){
  # compute compound amount factor 
  caf <-(1+i)^n
  
  # compute future value of costs
  fr.value <- PV * caf
  
  #Send the output
  fr.value
}
