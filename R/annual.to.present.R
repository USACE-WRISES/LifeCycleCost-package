#' Calculate present value from uniform annual payment 
#'  
#' \code{annual.to.present} Calculate present value from uniform annual payments 
#'      using uniform series present worth factor (uswf)
#'      
#' @param dr discount rate in percent per year
#' @param span life span in years
#' @param ann series of uniform annual payments
#'
#' @return Cost in present value
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
#'  
#' @examples  
#' # Result: Future Value = $33,3773 
#' annual.to.present(0.08, 30, 3000)  
#' 
#' @export
annual.to.present <- function(dr, span, ann){
  # compute uniform series present worth factor
  uswf <-((1+dr)^span-1) / (dr*(1+dr)^span)
  
  # compute present value of costs
  Pr.value.ann <- ann * uswf

  #Compute total present value
  sum(Pr.value.ann)
}
