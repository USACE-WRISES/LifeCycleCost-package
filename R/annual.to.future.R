#' Calculate future value from uniform annual payment
#'  
#' \code{annual.to.future} Compute future values from uniform annual payments
#'      using uniform series compound amount factor (uscaf)  
#'     
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param A series of uniform annual payments
#'
#' @return FV 
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012
#'  
#' @examples  
#' # Result: FV 
#' annual.to.future(0.08, 30, 50000)  
#' 
#' @export
annual.to.future <- function(i, n, A){
  # compute uniform series compound amount factor
  uscaf <- ((1+i)^n - 1) / i
  
  # compute future value of costs
  FV <- A * uscaf
  
  #Compute total future value
  sum(FV)

  #Send the output
  FV
}
