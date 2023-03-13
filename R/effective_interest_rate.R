#' Compute effective annual interest rate for a known nominal rate and 
#' compounding period per year 
#'  
#' \code{effective_interest_rate} Compute future value nominal rate and 
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
#' @return effective_interest_rate
#' 
#' @references
#'  Newnan, D. G., Eschenbach, T. G., Lavelle, J. P., & Oxford, N. Y. 
#'         Engineering Economic Analysis, 14th ed. 
#'         New York, Oxford University Press, 2020    
#' 
#'  David, W., & Terry, R. 
#'        Fundamentals of Engineering Economics and Decision Analysis. 
#'        Springer Nature, 2012
#'      
#' @examples  
#' # Result: effective_i = 
#' effective_interest_rate (0.08, m = "yearly")  
#' 
#' @export
effective_interest_rate <- function(r, m = c("yearly", "quarterly", "monthly")){
  
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
