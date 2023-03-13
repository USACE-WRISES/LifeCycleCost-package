#' Compute annual payment from present value 
#'  
#' \code{present_to_annual} Compute uniform series annual payments from present value 
#'      using capital recovery factor (crf) 
#'      
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param PV present value 
#'
#' @return ann.value
#'
#' @references
#' Newnan, D. G., Eschenbach, T. G., Lavelle, J. P., & Oxford, N. Y. (2020). 
#'         Engineering Economic Analysis (14th ed.). 
#'         New York, Oxford University Press.    
#' 
#'  David, W., & Terry, R. (2012). 
#'        Fundamentals of Engineering Economics and Decision Analysis. 
#'        Springer Nature.
#'  
#' @examples
#' # Result: ann.value = $17765.49
#' present_to_annual(0.08, 30, 200000)  
#' 
#' @export    
present_to_annual <- function(i, n, PV){
  # compute capital recovery factor
  crf <-(i*(1+i)^n)/((1+i)^n-1)
  
  # compute annual costs
  ann.value <-PV * crf
  
  #Send the output
  ann.value
}

