
# scattermatrix -----------------------------------------------------------

#' Plot a ScatterMatrix
#'
#' @param data Dataframe
#'
#' @return ggmatrix object that if called, will print
#'
#' @importFrom GGally ggpairs wrap
#' @importFrom ggplot2 aes
#'
#' @export
#'
#' @examples
#'
#' plot_scattermatrix(iris)
#' plot_scattermatrix(mtcars)
#'
plot_scattermatrix <- function(data){

  ggpairs(data,
          lower = list(continuous = points_and_smooth,
                       combo = "facethist",
                       discrete = "facetbar",
                       na = "na"),
          upper = list(continuous = wrap(colour_correlation, colour = "black", alpha = 1)),
          aes(alpha = I(0.3),
              colour = I("steelblue")))


}


#' Points and Smooth
#' @description This is an internal function of \code{\link{plot_scattermatrix}} and
#' it is not supposed to be called directly.
#'
#' @details Function to return points and geom_smooth
#'
#' @importFrom ggplot2 ggplot geom_point geom_smooth
#'
points_and_smooth <- function(data, mapping, method="loess", ...){

  ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=method, ..., colour = "coral", fill = NA)

}

#' Colour Correlation
#'
#' @description This is an internal function of \code{\link{plot_scattermatrix}} and
#' it is not supposed to be called directly.
#'
#' @details Function to return coloured correlations
#'
#' @importFrom GGally eval_data_col ggally_cor
#' @importFrom ggplot2 theme_void theme element_rect
#'
colour_correlation <- function(data, mapping, method="p", use="pairwise", ...){

  # grab data
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)

  # calculate correlation
  corr <- cor(x, y, method=method, use=use)

  # calculate colour based on correlation value
  #  -1    0    1
  # blue white red
  colFn <- grDevices::colorRampPalette(c("blue", "white", "red"), interpolate = 'spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length = 100))]

  ggally_cor(data = data, mapping = mapping, ...) +
    theme_void() +
    theme(panel.background = element_rect(fill = fill))

}
