# plot_pareto -------------------------------------------------------------

#' @title Build a Pareto Chart
#'
#' @description The function builds a Pareto Chart.
#' Each bar corresponds to a different modality of the variable in modality
#' ordered by the sum of the frequencies.
#'
#' @param to_plot tibble of interest.
#' @param modality column you want to show the frequency.
#' @param frequency frequency of modality.
#' @param title title of the chart, default: "Pareto Chart".
#' @param xlab title of x axis, default: "items".
#' @param ylab title of left y axis, default: "".
#' @param ylab2 title of right y axis, default: "% cumulate".
#' @param hline yintercept of horizontal line, default: 0.8.
#' @param bar.color colour of bars, default: "royalblue4".
#' @param bar.fill fill of bars, default: "royalblue".
#' @param bar.alpha transparency of bars, default: 0.4.
#' @param line.color color of cumulate line, default: "green".
#' @param line.size size of cumulate line, default: 1.
#' @param hline.col color of horizontal line, default: "orange"
#' @param group_over number of max modalities to show. Sum the others. Default: NULL
#' @param others_name name to assign to modalities over group_over. Default: "other"
#'
#' @return Returns a Pareto Chart, ggplot2 object
#'
#' @importFrom rlang enquo !! quo_name
#' @importFrom dplyr rename select group_by mutate summarize summarise arrange
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot geom_line geom_bar aes ggtitle xlab ylab scale_y_continuous geom_hline theme_light sec_axis
#' @importFrom forcats fct_lump
#'
#' @examples
#' test <- dplyr::sample_n(tibble::tibble(letters = letters),100, replace=TRUE)
#' test$frequency <- round(runif(100, 1, 5),0)
#' plot_pareto(test, letters, frequency, group_over = 6)
#'
#' @export

plot_pareto <- function(to_plot,
                        modality,
                        frequency,
                        xlab = modality,
                        ylab = "",
                        title = paste("Pareto Chart -", quo_name(xlab)),
                        ylab2 = "% cumulate",
                        hline = 0.80,
                        bar.color = "royalblue4",
                        bar.fill = "royalblue3",
                        bar.alpha = 0.6,
                        line.color = "forestgreen",
                        line.size = 1,
                        hline.col = "coral2",
                        group_over = NULL,
                        others_name = "other"){

  modality  <- enquo(modality)
  frequency <- enquo(frequency)

  to_plot <- to_plot %>%
    rename(modality  = !!modality,
           frequency = !!frequency) %>%
    group_by(modality) %>%
    summarise(frequency = sum(frequency)) %>%
    mutate(modality = reorder(modality, -frequency)) %>%
    arrange(modality)

  if(!is.null(group_over)){

    to_plot <- to_plot %>%
      mutate(modality = fct_lump(modality, n = group_over, w = frequency, other_level = others_name)) %>%
      group_by(modality) %>%
      summarise(frequency = sum(frequency))

  }


  ggplot(data = to_plot, aes(x = modality)) +
    geom_line(aes(y = cumsum(frequency), group = 1), colour = line.color, size = line.size) +
    geom_bar(aes(y = frequency), stat = "identity", colour = bar.color, fill = bar.fill, alpha = bar.alpha)  +

    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    scale_y_continuous(sec.axis = sec_axis(~.*100/sum(to_plot$frequency), name = ylab2)) +
    geom_hline(yintercept = hline*sum(to_plot$frequency), col = hline.col) +
    theme_light() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

}





# plot_pareto_double ------------------------------------------------------

#' @title Pareto Chart for two categorical variables
#'
#' @description This Pareto Chart comes handy when you're calculating the number of times
#' each category of a variable corresponds to a certain number of categories of another variable.
#' For example, You want to know how many clients purchase a certain number of items.
#' With \code{pareto_chart_double(df, client, article)} you will see bars as high as the number of clients
#' that purchase a certain number of items. The bars will be located in corrispondence (on the x line) of the number
#' of items those clients purchase.
#'
#' @param to_plot tibble of interest.
#' @param var1 in the example in description correspons to client.
#' @param var2 in the example in description correspons to article.
#' @param step size of histogram bars.
#' @param title title of the chart, default: "Pareto Chart".
#' @param xlab title of x axis, default: "items".
#' @param ylab title of left y axis, default: "".
#' @param ylab2 title of right y axis, default: "% cumulate".
#' @param hline yintercept of horizontal line, default: 0.8.
#' @param bar.color colour of bars, default: "royalblue4".
#' @param bar.fill fill of bars, default: "royalblue".
#' @param bar.alpha transparency of bars, default: 0.4.
#' @param line.color color of cumulate line, default: "green".
#' @param line.size soze of cumulate line, default: 1.
#' @param hline.col color of horizontal line, default: "orange"
#'
#' @return
#' Returns a Pareto Chart, ggplot2 object
#'
#' @importFrom rlang enquo !!
#' @importFrom dplyr rename select group_by mutate summarize summarise arrange
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot stat_bin geom_histogram ggtitle aes xlab ylab scale_y_continuous geom_hline scale_x_continuous theme_light sec_axis
#'
#' @examples
#' test <- tibble::tibble(clients  = c(rep("John"    , 3),
#'                                     rep("Jack"    , 3),
#'                                     rep("Simon"   , 2),
#'                                     rep("Veronika", 2),
#'                                     rep("Stephen" , 2),
#'                                     rep("Angel"   , 1)),
#'                        articles = letters[1:13])
#'
#' plot_pareto_double(test, clients, articles)
#'
#'
#' @export

plot_pareto_double <- function(to_plot,
                               var1,
                               var2,
                               step = 1,
                               title = "Pareto Chart - Distribution",
                               xlab = paste("Number of", quo_name(var2)),
                               ylab = paste("Number of", quo_name(var1), "for each number of", quo_name(var2)),
                               ylab2 = "% cumulata",
                               hline = 0.80,
                               bar.color = "royalblue4",
                               bar.fill = "royalblue3",
                               bar.alpha = 0.6,
                               line.color = "forestgreen",
                               line.size = 1,
                               hline.col = "coral2"){

  var1 <- enquo(var1)
  var2 <- enquo(var2)

  to_plot <- to_plot %>%
    rename(var1 = !!var1,
           var2 = !!var2) %>%
    distinct(var1, var2) %>%
    group_by(var1) %>%
    summarize(var2 = n())

  ggplot(data = to_plot, aes(x = var2)) +
    stat_bin(aes(y=cumsum(..count..)), breaks = seq(0, max(to_plot$var2), step),
             colour = line.color, size = line.size, geom = "line") +
    geom_histogram(colour = bar.color, fill = bar.fill, breaks = seq(0, max(to_plot$var2), step), alpha = bar.alpha) +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    scale_x_continuous(breaks = seq(0,max(to_plot$var2), 2 * step)) +
    scale_y_continuous(sec.axis = sec_axis(~.*100/nrow(to_plot), name = ylab2)) +
    geom_hline(yintercept = hline*nrow(to_plot), colour = hline.col) +
    theme_light() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

}

