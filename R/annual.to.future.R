#' Calculate future value from uniform annual payment
#'  
#' \code{annual.to.future} Compute future values from uniform annual payments
#'      using uniform series compound amount factor (uscaf)  
#'     
#' @param dr discount rate in percent per year
#' @param span life span in years
#' @param ann.value series of uniform annual payments
#'
#' @return fr.value 
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: fr.value 
#' annual.to.future(0.08, 30, 50000)  
#' 
#' @export
annual.to.future <- function(dr, span, ann.value){
  # compute uniform series compound amount factor
  uscaf <- ((1+dr)^span - 1) / dr
  
  # compute future value of costs
  fr.value <- ann.value * uscaf
  
  #Compute total future value
  sum(fr.value)

  #Send the output
  fr.value
}
