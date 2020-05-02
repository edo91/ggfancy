


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
  # Here I have set a correlation of minus one to blue,
  # zero to white, and one to red
  colFn <- grDevices::colorRampPalette(c("blue", "white", "red"), interpolate ='spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]

  ggally_cor(data = data, mapping = mapping, ...) +
    theme_void() +
    theme(panel.background = element_rect(fill=fill))

}



# Old Scatter Matrix ------------------------------------------------------

# # make_pairs --------------------------------------------------------------
#
# #' @title Make Pairs for Scatter Matrix
# #'
# #' @param data Dataframe or tibble
# #' @param ...  Variables to select
# #'
# #' @importFrom rlang quos !!!
# #' @importFrom tidyselect vars_select
# #' @importFrom purrr map_lgl map_dfr
# #' @importFrom dplyr bind_cols bind_rows
# #' @importFrom tibble tibble
# #'
# #' @return Returns a reshaped dataframe.
# #'
# #' @examples
# #'
# #' make_pairs(iris)
# #' make_pairs(iris, -Species)
# #' make_pairs(mtcars, cyl, mpg, hp)
# #'
# #' @export
# #'
# make_pairs <- function(data, ...){
#
#   quos <- quos(...)
#
#   # if no variables selected take them all
#   if(length(quos) == 0) pairs_vars <- names(data)
#   else pairs_vars <- unname(vars_select(names(data), !!!quos))
#
#   if(length(pairs_vars) == 0) stop("No variables selected")
#
#   # check if all are numeric, exclude the others
#   check_num <- map_lgl(data[,pairs_vars], is.numeric)
#   if(!all(check_num)){
#
#     no_pairs <- pairs_vars[!check_num]
#     warning("Non numerical variables discarded: ", paste(no_pairs, collapse = ", "), call. = FALSE)
#     pairs_vars <- pairs_vars[check_num]
#
#   }
#
#   if(length(pairs_vars) == 0) stop("No variables selected")
#
#   extra_vars <- setdiff(names(data), pairs_vars)
#
#   grid <- expand.grid(x = pairs_vars, y = pairs_vars, stringsAsFactors = FALSE)
#   grid <- subset(grid, x != y)
#
#   all <- map_dfr(1:nrow(grid),
#                  function(i) {
#                    xcol <- grid[i, "x"]
#                    ycol <- grid[i, "y"]
#                    bind_cols(tibble(xvar = ycol,
#                                     yvar = xcol,
#                                     x = data[[xcol]],
#                                     y = data[[ycol]]),
#                              data[extra_vars])
#                  })
#
#   densities <- map_dfr(pairs_vars,
#                        function(i) bind_cols(tibble(xvar = i, yvar = i, x = data[[i]]),
#                                              data[extra_vars]))
#
#   list(scatter = all, density = densities)
# }
#
#
#
#
# # plot_scattermatrix ------------------------------------------------------
#
# #' @title Plot a ggplot Scatter Matrix
# #'
# #' @param data Dataframe or tibble
# #' @param ...  Variables to select
# #' @param density       Whether you want to have density plots on the diagonal.
# #' @param add_45_line   Whether you want to add a 45 degrees line in the scatter plots.
# #' @param add_smooth    Whether you want to add a geom smooth line in the scatter plots.
# #' @param fixed_ratio   Whether you want to set coord_fixed or have facet_grid >> scale = "free"
# #' @param smooth_method Choose a method for geom_smoooth. Needed only if add_smooth == TRUE. See geom_smooth.
# #' @param title         Set a title
# #' @param subtitle      Set a subtitle
# #'
# #' @importFrom ggplot2 ggplot aes geom_line geom_point geom_smooth stat_density facet_grid theme_light labs coord_fixed
# #'
# #' @return Returns a ggplot Scatter Matrix with density plots on the diagonal.
# #'
# #' @examples
# #'
# #' plot_scattermatrix(iris) # factors and chars are eliminated automatically
# #' plot_scattermatrix(iris, - Species)
# #' plot_scattermatrix(iris, - Species, add_45_line = TRUE, density = FALSE, smooth_method = "lm")
# #' plot_scattermatrix(iris, - Species, add_45_line = FALSE, add_smooth = FALSE, density = FALSE)
# #' plot_scattermatrix(mtcars, cyl, mpg, hp, add_smooth = TRUE, smooth_method = "lm", add_45_line = FALSE, fixed_ratio = FALSE, density = FALSE)
# #' plot_scattermatrix(mtcars, add_smooth = TRUE, smooth_method = "lm", add_45_line = FALSE, density = FALSE)
# #'
# #' @export
# #'
# plot_scattermatrix <- function(data, ...,
#                                density     = TRUE,
#                                add_45_line = TRUE,
#                                add_smooth  = TRUE,
#                                fixed_ratio = FALSE,
#                                smooth_method = "loess",
#                                title = "Scatter Matrix",
#                                subtitle = NULL){
#
#   data <- make_pairs(data, ...)
#
#   # pairs plot
#   gg <- ggplot(data$scatter, aes(x = x, y = y))
#
#
#   if(add_45_line) gg <- gg + geom_line(aes(y = x), colour = "gray40", linetype = 2)
#
#   gg <- gg + geom_point(colour = "springgreen3", alpha = 0.4)
#
#   if(add_smooth) gg <- gg + geom_smooth(method = smooth_method, colour = "coral3", fill = NA)
#
#   if(density) gg <- gg +
#     stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)),
#                  data = data$density,
#                  position = "identity",
#                  colour = "grey20",
#                  geom = "line")
#
#   gg <- gg +
#     theme_light() +
#     labs(title = title,
#          subtitle = subtitle,
#          x = "",
#          y = "")
#
#   if(fixed_ratio) gg <- gg + coord_fixed() + facet_grid(xvar ~ yvar)
#   else  gg <- gg + facet_grid(xvar ~ yvar, scales = "free")
#
#   gg
# }
