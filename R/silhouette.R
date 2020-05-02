# Graph for clustering


# plot_silhouette ---------------------------------------------------------

#' @title Plot Silhouette
#' @description Returns a silhouette plot for cluster analysis. It helps to check for cluster cohesion.
#' @references Wiki link: https://en.wikipedia.org/wiki/Silhouette_(clustering)
#'
#' @param v_clusters An integer vector with k different integer cluster codes or a list with such an x$clustering component. See ?cluster::silhouette
#' @param mx_dist    A dissimilarity object inheriting from class dist. See ?cluster::silhouette
#' @param title      String. Custom title.
#' @param threshold  Numeric. Threshold over which an item is considered to be fitting its own cluster.
#'
#' @return A ggplot object with silhouette plot.
#'
#' @importFrom dplyr mutate mutate_at vars row_number
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid labs ylim theme theme_light element_blank scale_fill_discrete
#' @importFrom cluster silhouette
#' @importFrom forcats fct_reorder
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na
#'
#' @examples
#'
#' kcl <- kmeans(mtcars, centers = 5, nstart = 50)
#' dis <- dist(mtcars)^2
#' plot_silhouette(v_clusters = kcl$cluster, mx_dist = dis, threshold = 0.3)
#'
#' @export
#'
plot_silhouette <- function(v_clusters, mx_dist,
                            title = "Silhouette Graph - Check for cluster cohesion",
                            threshold = 1){

  # create silhouette and force it to matrix
  sil <- structure(silhouette(v_clusters, mx_dist), class = "matrix")

  # prepare dataset to plot
  sil <- sil %>%
    as.data.frame() %>%
    mutate(id = row_number())

  sil <- sil %>%
    mutate(neighbor = ifelse(sil_width > threshold, NA, neighbor))


  sil <- sil %>%
    mutate_at(c("cluster", "neighbor", "id"), as.factor)

  # return plot
  ggplot(sil) +
    geom_bar(aes(x    = fct_reorder(id, sil_width, .desc = TRUE),
                 y    = sil_width,
                 fill = neighbor),
             stat   = "identity",
             width  = 1,
             colour = "gray80") +
    scale_fill_discrete(labels = function(x) replace_na(x, "Fitting")) +
    facet_grid(. ~ cluster, scales = "free_x", space = "free_x") +
    labs(title = title,
         fill  = "Nearest cluster",
         x     = "Items",
         y     = "Silhouette") +
    ylim(-1, 1) + #theorical boundaries
    theme_light() +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())

}
