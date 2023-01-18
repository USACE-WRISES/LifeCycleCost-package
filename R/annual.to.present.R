#' Calculate present value from uniform annual payment 
#'  
#' \code{annual.to.present} Compute present value from uniform annual payments 
#'      using uniform series present worth factor (uswf)
#'      
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param A series of uniform annual payments
#'
#' @return P
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
#'  
#' @examples  
#' # Result: P = $33,3773 
#' annual.to.present(0.08, 30, 30000)  
#' 
#' @export
annual.to.present <- function(i, n, A){
  # compute uniform series present worth factor
  uswf <-((1+i)^n-1) / (i*(1+i)^n)
  
  # compute present value of costs
  P <- A * uswf

  #Compute total present value
   sum(P)
   
  #Send the output
  A
  
}
