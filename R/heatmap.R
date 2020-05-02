# create heatmaps

# plot_heatmap ------------------------------------------------------------

#' @title Plot a Heatmap
#'
#' @param data        Tibble or dataframe. Data to plot.
#' @param order_rows  Logical. Reorder rows?
#' @param order_cols  Logical. Reorder rows?
#' @param title       String. Title of the heatmap.
#'
#' @return Returns a ggplot geom_tile heatmap
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if mutate_all mutate row_number
#' @importFrom tidyr gather
#' @importFrom purrr compose
#' @importFrom lubridate is.Date is.POSIXt
#' @importFrom ggplot2 ggplot aes theme_minimal scale_fill_continuous coord_flip geom_tile labs theme
#'
#' @examples
#' plot_heatmap(iris)
#' plot_heatmap(mtcars, order_rows = TRUE, order_cols = TRUE)
#' plot_heatmap(mtcars)
#' plot_heatmap(mtcars, order_rows = TRUE)
#' plot_heatmap(mtcars, order_cols = TRUE)
#' plot_heatmap(mtcars, order_rows = TRUE, order_cols = TRUE)
#'
#' @export
#'
plot_heatmap <- function(data, order_rows = FALSE, order_cols = FALSE, title = "Heatmap", scalefun = rescale){

  # get names
  nn <- names(data)

  # convert everything to numeric and scale
  data <- data %>%
    mutate_if(is.logical, as.numeric) %>%
    mutate_if(is.factor , as.numeric) %>%
    mutate_if(is.Date   , as.numeric) %>%
    mutate_if(is.POSIXt , as.numeric) %>%
    mutate_if(is.integer, as.numeric) %>%
    mutate_if(is.character, compose(as.numeric, factor))

  if(!is.null(scalefun)) data <- data %>% mutate_all(scalefun)

  # reorder cols
  if(order_cols){
    Colv <- colMeans(data, na.rm = TRUE)
    hcr <- hclust(dist(t(data)))
    ddr <- as.dendrogram(hcr)
    ddr <- reorder(ddr, Colv)
    colInd <- order.dendrogram(ddr)
  } else colInd <- seq_along(data)


  # reorder rows
  if(order_rows){
    Rowv <- rowMeans(data, na.rm = TRUE)
    hcr <- hclust(dist(data))
    ddr <- as.dendrogram(hcr)
    ddr <- reorder(ddr, Rowv)
    rowInd <- order.dendrogram(ddr)
    data <- data[rowInd, ]
  }

  # gather data
  data <- data %>%
    mutate(.row = row_number()) %>%
    gather(var, value, -.row)

  # reoder var
  data$var <- factor(data$var, levels = nn[colInd], ordered = TRUE)

  # plot heatmap
  ggplot(data, aes(var, .row, fill = value)) +
    geom_tile() +
    coord_flip() +
    theme_minimal() +
    labs(title = title,
         y = "Row",
         x = "") +
    theme(legend.position = "none") +
    scale_fill_continuous(low = "#21dd6c", high = "#032812")

}



# plot_na -----------------------------------------------------------------

#' @title Plot Heatmap for NA
#'
#' @param data  Tibble or Dataframe.
#' @param order_rows  Logical. Reorder rows?
#' @param order_cols  Logical. Reorder rows?
#' @param title Title of the graph.
#'
#' @importFrom tibble as_tibble
#'
#' @return Returns a ggplot geom_tile heatmap.
#'
#' @examples
#'
#' x <- tibble::tibble(
#'             a = sample(c(1:10,NA), 100, replace = TRUE),
#'             b = sample(c(1:10,NA), 100, replace = TRUE),
#'             c = sample(c(1:10,NA), 100, replace = TRUE),
#'             d = sample(c(1:10,NA), 100, replace = TRUE),
#'             e = sample(c(1:10,NA), 100, replace = TRUE),
#'             f = sample(c(1:10,NA), 100, replace = TRUE))
#'
#' plot_na(x)
#'
#' @export
#'
plot_na <- function(data, order_rows = FALSE, order_cols = FALSE, title = "Missing Data"){

  plot_heatmap(as_tibble(is.na(data)), order_rows, order_cols, title = title, scalefun = NULL)

}
