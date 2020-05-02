
# rescale --------------------------------------------------------------------------

#' @title Rescale between 0 and 1 with min-max
#'
#' @param x Vector to be rescaled
#'
#' @return Returns rescaled x
#'
#' @examples
#'
#' rescale(c(1, 4, 5, 2, 9.1))
#'
#' @export
#'
rescale  <- function(x, na.rm = TRUE){

  m <- min(x, na.rm = na.rm)
  x <- x - m
  M <- max(x, na.rm = na.rm)
  x <- x / M

  return(x)

}
