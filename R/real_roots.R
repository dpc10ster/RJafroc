#' Test code to exercise testthat
#'
#' @description From website example
#'
#'
#' @usage  real.roots(a, b, c)
#'
#' @param a The seed variable, default is 123; set to NULL for truly random seed
#' @param b The number of non-diseased cases, default is 50
#' @param c The number of diseased cases, default is 50
#'
#' @return The roots of the quadratic defined by a, b, c
#' 
#' 
#' @export

real.roots <- function(a, b, c)
{
  if (a == 0.)
    stop("Leading term cannot be zero")
  
  d = b*b - 4*a*c # discriminant
  
  if (d < 0)
    rr = c()
  else if (d == 0)
    rr = c( -b/(2*a) )
  else
    rr = c( (-b - sqrt(d))/(2*a), 
            (-b + sqrt(d))/(2*a)  )
  
  return(rr)
}