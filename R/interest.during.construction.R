#' compute interest during construction (idc)
#' 
#Function for computing interest during construction based on:
#'@input first/capital cost (Ccos)
#'@input Construction duration in months (durm)
#'@input annual interest rate (ir)
#'
#'@return  idc.month
#'
#'@references
#'
#' @examples  
#' # Result: idc.month
#' idc.month(0.08, 40000, 25) 

idc <- function(ir, Fcos, durm){
  #Compute monthly discount factor
  dr.month <- (1+ir)^(1/12) - 1
  
  #Compute interest for each month
  idc.month <- (Ccap/durm)*((1+dr.month)^(durm-seq(1,durm))-1)
  
  #Compute total idc 
  sum(idc.month)
}



