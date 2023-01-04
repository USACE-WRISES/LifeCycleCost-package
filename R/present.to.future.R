#' Compute future values from present value
#' using compound amount factor (caf) 
#'  
#' \code{present.to.future} calculate future value from present value  
#'     
#'Function for computing future value based on:
#'@input discount rate (dr)
#'@input life span in years (yr)
#'@input present value (pv)
#'
#' @return fr.from.pv
#'
#'#'@references
#'Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: fr.from.pr 
#' fr.from.pr(0.08, 900000, 30)  
#' 

fr.from.pr <- function(dr, pv, yr){
  # compute compound amount factor 
  caf <-(1+dr)^yr
  
  # compute future value of costs
  fr.from.pr <-(pv)(caf)
  
}
