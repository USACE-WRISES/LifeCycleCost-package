#' Calculate annual payment from present value 
#'  
#' \code{present.to.annual} Compute uniform series annual payments from present value 
#'      using capital recovery factor (crf) 
#'      
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param PV present value 
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
ann.value <- function(i, n, PV){
  # compute recovery factor
  crf <-(i(1+i)^n)/((1+i)^n-1)
  
  # compute annual costs
  ann.value <-(PV)(crf)
  
  #Send the output
  ann.value
}

