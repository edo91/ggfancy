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
#' * group: for every group you'll have an intermediate column
#' * label: name of increase or decrease
#' * value: increase or decrease value
#'
#' @importFrom dplyr mutate group_by transmute ungroup row_number bind_rows lag if_else slice_tail
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_rect geom_segment scale_y_continuous scale_x_continuous labs theme_light theme element_blank
#' @importFrom purrr map2_dbl negate
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
#'
#' bb <- tibble::tribble(
#'   ~ group  , ~ label      , ~ value,
#'   "season1", "loss"       ,     -20,
#'   "season3", "loss"       ,     -10,
#'   "season1", "new"        ,      30,
#'   "season1", "gain"       ,      40,
#'   NA       , "loss"       ,     -15,
#'   NA       , "gain"       ,      25,
#'   "season3", "disappeared",     -15,
#'   "season3", "gain"       ,       5
#'
#' )
#'
#' starting_point <- 100
#'
#' plot_waterfall(aa, starting_point)
#'
#' # note that groups name may be repeated: it will create its own checkpoint
#' # groups can also be NA
#' plot_waterfall(bb, starting_point)
#'
#'
#' @export
#'
plot_waterfall <- function(data,
                           starting_point = 0,
                           label_starting_point = "starting_point",
                           title = "Waterfall",
                           subtitle = NULL){


  # check out group label value are in data
  stopifnot(c("group", "label", "value") %in% names(data))

  # calculate:
  # - id: row number +1 (for each group)
  # - value_s/_e: starting/ending value
  # - sign: increase or decrease
  data <- data %>%
    mutate(groupnew = map2_dbl(group, lag(group, default = group[1]), negate(identical))) %>%
    mutate(group_  = cumsum(groupnew),
           id      = cumsum(1 + groupnew),
           value_s = cumsum(lag(value, default = 0)) + starting_point,
           value_e = cumsum(value) + starting_point,
           sign    = if_else(value > 0, "#00BA38", "#F8766D")) # green or red

  # calculate group check point
  # - id: completes the skip we forced before
  # - value_s is zero: checkpoint has to start from zero
  # - sign is neutral
  tot_data <- data %>%
    group_by(group_) %>%
    slice_tail() %>%
    ungroup() %>%
    transmute(label   = group,
              id      = id + 1,
              value_s = 0,
              value_e,
              sign    = "#619CFF") # blue

  # create first checkpoint
  start_data <- list(label   = label_starting_point,
                     id      = 0,
                     value_s = 0,
                     value_e = starting_point,
                     sign    = "#619CFF") # blue

  # bind the three dataframe
  data <- bind_rows(start_data, tot_data, data)

  # create the width of bars and segments
  data <- data %>%
    mutate(rmin = id - 0.45, # bar start
           rmax = id + 0.45, # bar end
           smax = rmax + (id != max(id))) # segment end

  # waterfall
  ggplot(data) +

    # bars
    geom_rect(aes(xmin = rmin,
                  xmax = rmax,
                  ymin = value_s,
                  ymax = value_e,
                  fill = I(sign))) +

    # connection segments
    geom_segment(aes(x    = rmin,
                     xend = smax,
                     y    = value_e,
                     yend = value_e),
                 colour = "gray50") +

    # rename ids on x axis
    scale_x_continuous(breaks = data$id,
                       labels = data$label) +

    # make it pretty
    scale_y_continuous(labels = scales::comma) +
    labs(title    = title,
         subtitle = subtitle,
         x        = "",
         y        = "") +
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())


}

