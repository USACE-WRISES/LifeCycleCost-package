#' Compute interest during construction 
#'  
#' \code{interest.during.construction} calculate interest during construction 
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
#' @examples  
#' # Result: idc
#' interest.during.construction (0.08,25,700000)
#' 
#' #' @export 
interest.during.construction <- function (i, duration, capital){
  #Compute monthly discount factor
  dr.month <- (1+ir)^(1/12) - 1
  
  #Compute interest for each month
  idc.month <- (Ccap/durm)*((1+dr.month)^(durm-seq(1,durm))-1)
  
  #Compute total idc 
  idc <- sum(idc.month)
  
  #Send the output
  idc
}



