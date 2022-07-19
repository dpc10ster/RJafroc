#' @name funs
#' @aliases sum1
#' @aliases prod1
#'
#' @title Two functions of x and y
#'
#' @param x =X
#' @param y =Y
#'
#' @note \code{funs} is a generic name for the functions documented.
#' \cr
#' If called, \code{funs} returns its own arguments.
#'
#' @rdname funs
#' @export
funs <- function(x,y) {identity(c(x,y))}
#'
#' @rdname funs
#' @return \code{sum1(x,y)} returns x+y
#' @examples
#' sum1(3,4)
#' @export
sum1 <- function(x,y) x+y
#'
#' @rdname funs
#' @return \code{prod1(x,y)} returns x*y
#' @examples
#' prod1(3,4)
#' @export
prod1 <- function(x,y) x*y