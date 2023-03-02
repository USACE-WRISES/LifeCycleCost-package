#' Compute interest during construction 
#'  
#' \code{interest_during_construction} calculate interest during construction 
#'       using monthly discount factor 
#' 
#'@param i interest rate in percent per year
#'@param duration construction duration in months
#'@param capital first/capital cost
#'
#'@return  idc
#'
#'@references
#'
#'
#' @examples  
#' # Result: idc
#' interest_during_construction (0.02758,25,700000)
#' 
#' #' @export 
interest_during_construction <- function (i, duration, capital){
  #Compute monthly discount factor
  dr.month <- (1+i)^(1/12) - 1
  
  #Compute interest for each month
  idc.month <- (capital/duration)*((1+dr.month)^(duration-seq(1,duration))-1)
  
  #Compute total idc 
  idc <- sum(idc.month)
  
  #Send the output
  idc
}



