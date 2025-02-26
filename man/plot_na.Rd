% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmap.R
\name{plot_na}
\alias{plot_na}
\title{Plot Heatmap for NA}
\usage{
plot_na(data, order_rows = FALSE, order_cols = FALSE, title = "Missing Data")
}
\arguments{
\item{data}{Tibble or Dataframe.}

\item{order_rows}{Logical. Reorder rows?}

\item{order_cols}{Logical. Reorder rows?}

\item{title}{Title of the graph.}
}
\value{
Returns a ggplot geom_tile heatmap.
}
\description{
Plot Heatmap for NA
}
\examples{

x <- tibble::tibble(
            a = sample(c(1:10,NA), 100, replace = TRUE),
            b = sample(c(1:10,NA), 100, replace = TRUE),
            c = sample(c(1:10,NA), 100, replace = TRUE),
            d = sample(c(1:10,NA), 100, replace = TRUE),
            e = sample(c(1:10,NA), 100, replace = TRUE),
            f = sample(c(1:10,NA), 100, replace = TRUE))

plot_na(x)

}
