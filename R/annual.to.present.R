#' Compute present values from uniform annual payment 
#' using uniform series present worth factor (uswf) 
#'  
#' \code{Pr.value.ann} calculate present value from uniform series annual value  
#'     
#'Function for computing present value based on:
#'@input discount rate (dr)
#'@input life span in years (yr)
#'@input a series of uniform payment (ann)
#'
#' @return Pr.value.ann 
#'
#'@references
#'Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
#'  
#' @examples  
#' # Result: Pr.value 
#' Pr.value.ann(0.08, 3000, 30)  
#' 
    
Pr.value.ann <- function(dr, ann, yr){
  # compute uniform series present worth factor
  uswf <-((1+dr)^yr-1)/(dr(1+dr)^yr)
  
  # compute present value of costs
  Pr.value.ann <-(ann)(uswf)

  #Compute total present value
  sum(Pr.value.ann)
}
