#' Calculate future value from uniform series annual value
#' using uniform series compound amount factor (uscaf) 
#'  
#' \code{annual.to.future} Compute future values from uniform annual payment
#'      using uniform series compound amount factor (uscaf)  
#'     
#' @param discount rate (dr)
#' @param life span in years (yr)
#' @param a series of uniform payment (ann)
#'
#' @return Future value  
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#' Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: fr.value 
#' fr.value(0.08, 5000, 30)  
#' 

annual.to.future <- function(dr, ann, yr){
  # compute uniform series compound amount factor
  uscaf <-((1+dr)^yr-1)/dr
  
  # compute future value of costs
  fr.value <-(ann)(uscaf)
  
  #Compute total future value
  sum(fr.value)
}
