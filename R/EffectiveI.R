#' Calculate effective annual interest rate for a known nominal rate and 
#' compounding period per year 
#'  
#' \code{effective_i} Compute future value nominal rate and 
#' compounding period per year, and frequency.
#' 
#' @param r nominal interest rate in decimal number
#' @param m number of compounding period per year
#'          monthly = 12
#'          Quarterly = 4
#'          yearly = 1
#' @param i interest rate per period (r/m)
#'
#'
#' @return effective_i
#' 
#' @references
#' Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle. 
#'      Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
#'      
#' @examples  
#' # Result: Effective_i = 
#' effective_i (0.08, m = "yearly")  
#' 
#' @export
effective_i <- function(r, m = c("yearly", "quarterly", "monthly")){
  
  # compute effective interest in percent
  
  if (m == "yearly") {
   m <- 1
   effective_i <- (((1 + r/m) ^ m) - 1) * 100 
} 

   else if (m == "quarterly") {
   m <- 4
   effective_i <- ((1 + r/m) ^ m - 1) * 100 
} 
 
  else if (m == "monthly") {
   m <- 12
   effective_i <- ((1 + r/m) ^ m - 1) * 100 
  
 }
}
