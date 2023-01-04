#' Calculate future value from uniform annual payment
#'  
#' \code{annual.to.future} Compute future values from uniform annual payments
#'      using uniform series compound amount factor (uscaf)  
#'     
#' @param dr discount rate in percent per year
#' @param span life span in years
#' @param ann series of uniform annual payments
#'
#' @return Cost in future value 
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: Future Value = $566,416
#' annual.to.future(0.08, 30, 5000)  
#' 
#' @export
annual.to.future <- function(dr, span, ann){
  # compute uniform series compound amount factor
  uscaf <- ((1+dr)^span - 1) / dr
  
  # compute future value of costs
  fr.value <- ann * uscaf
  
  #Compute total future value
  sum(fr.value)
}
