#' Calculate annual payment from present value 
#'  
#' \code{present.to.annual} Compute uniform series annual payments from present value 
#'      using capital recovery factor (crf) 
#'      
#' @param dr discount rate in percent per year
#' @param span life span in years
#' @param pr.value present value (PValue)
#'
#' @return ann.value 
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples
#' # Result: ann.value 
#' ann.value(0.08, 2000, 30)  
#' 
#' @export    
ann.value <- function(dr, PValue, yr){
  # compute recovery factor
  crf <-(dr(1+dr)^yr)/((1+dr)^yr-1)
  
  # compute annual costs
  ann.value <-(PValue)(crf)
  
  #Send the output
  ann.value
}

