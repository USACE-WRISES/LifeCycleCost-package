#' Calculate present value from uniform annual payment 
#'  
#' \code{annual.to.present} Compute present value from uniform annual payments 
#'      using uniform series present worth factor (uswf)
#'      
#' @param dr discount rate in percent per year
#' @param span life span in years
#' @param ann.value series of uniform annual payments
#'
#' @return pr.value
#'
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
#'  
#' @examples  
#' # Result: pr.value = $33,3773 
#' annual.to.present(0.08, 30, 30000)  
#' 
#' @export
annual.to.present <- function(dr, span, ann.value){
  # compute uniform series present worth factor
  uswf <-((1+dr)^span-1) / (dr*(1+dr)^span)
  
  # compute present value of costs
  Pr.value <- ann.value * uswf

  #Compute total present value
   sum(Pr.value.ann)
   
  #Send the output
  ann.value
  
}
