# Waterfall chart

# plot_waterfall ----------------------------------------------------------

#' @title Plot waterfall
#'
#' @param data                 Tibble. See details and examples
#' @param starting_point       Numeric. First column height.
#' @param label_starting_point String. First column label.
#' @param title                String. Title of the plot
#' @param subtitle             String. Subtitle of the plot.
#'
#' @details data must be a tibble with 3 columns:
#' * group: for every group you'llhave an intermediate column
#' * label: name of increase or decrease
#' * value: increase or decrease value
#'
#' @importFrom dplyr mutate group_by select ungroup summarise row_number arrange bind_rows lag if_else
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_rect scale_y_continuous scale_x_continuous scale_fill_manual labs theme_light theme element_blank
#'
#' @return Returns a waterfall plot.
#'
#' @examples
#'
#' aa <- tibble::tribble(
#'   ~ group  , ~ label      , ~ value,
#'   "season1", "loss"       ,     -20,
#'   "season1", "new"        ,      30,
#'   "season1", "gain"       ,      40,
#'   "season2", "loss"       ,     -15,
#'   "season2", "gain"       ,      25,
#'   "season3", "loss"       ,     -10,
#'   "season3", "disappeared",     -15,
#'   "season3", "gain"       ,       5
#'
#' )
#'
#' starting_point <- 100
#'
#' plot_waterfall(aa, starting_point)
#'
#'
#' @export
#'
plot_waterfall <- function(data,
                           starting_point = 0,
                           label_starting_point = "starting_point",
                           title = "Waterfall",
                           subtitle = NULL){

  # force value as double
  data$value <- as.numeric(data$value)

  # add order to keep track
  data <- data %>%
    mutate(n = row_number()) %>%
    group_by(group) %>%
    mutate(ng = row_number()) %>%
    ungroup()

  # calculate intermediate steps for each group
  data_group <- data %>%
    group_by(group) %>%
    summarise(value = sum(value),
              n = max(n),
              ng = max(ng) + 1) %>%
    mutate(value = value + starting_point)

  # bind groups with stating row, groups and original data
  data <- bind_rows(data,
                    data_group,
                    list(group = label_starting_point, value = starting_point, n = 0, ng = 0))


  # arrange in the right order
  data <- data %>%
    arrange(n, ng) %>%
    select(-n , -ng)

  # calc initial value and final value
  data <- data %>%
    mutate(value_f = cumsum(if_else(is.na(label), 0, value)) + starting_point) %>%
    mutate(value_p = if_else(is.na(label), 0, lag(value_f, default = 0)))

  # calc id to keep positions
  data <- data %>%
    mutate(id = row_number())

  # define colour: red down, green up, blue intermediate
  data <- data %>%
    mutate(sign = if_else(is.na(label), "blue", if_else(value > 0, "green", "red")))

  # define label for intermediate bars
  data <- data %>%
    mutate(label = if_else(is.na(label), group, label))


  # waterfall
  ggplot(data, aes(fill = sign)) +
    geom_rect(aes(xmin = id - 0.45,
                  xmax = id + 0.45,
                  ymin = value_p,
                  ymax = value_f)) +

    scale_y_continuous(labels = scales::comma) +

    scale_fill_manual(values = c(red = "#F8766D", green = "#00BA38", blue = "#619CFF")) +

    scale_x_continuous(breaks = data$id,
                       labels = data$label) +

    labs(title    = title,
         subtitle = subtitle,
         x        = "",
         y        = "") +
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "none")

}
