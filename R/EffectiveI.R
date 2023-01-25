#' Calculate effective annual interest for a known nominal rate and 
#' compounding period per year 
#'  
#' \code{effectiveI} Compute future value nominal rate and 
#' compounding period per year, and frequency.
#'      
#' @param r nominal interest rate in decimal number
#' @param m number of compounding period per year
#'          monthly = 12
#'          Quarterly = 4
#'          yearly = 1
#'
#'
#' @return effectiveI
#' 
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
#'      
#' @examples  
#' # Result: EffectivI
#' EffectiveI (0.08, m = "yearly")  
#' 
#' @export
EffectiveI <- function(r, m = c("yearly", "quarterly", "monthly")){
  
  # compute effective interest in percent
  
  if (m == "yearly") {
   m <- 1
   EffectiveI <- (((1 + r/m) ^ m) - 1) * 100 
} 

   else if (m == "quarterly") {
   m <- 4
   EffectiveI <- ((1 + r/m) ^ m - 1) * 100 
} 
 
  else if (m == "monthly") {
   m <- 12
   EffectiveI <- ((1 + r/m) ^ m - 1) * 100 
  
 }
}
