% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pareto.R
\name{plot_pareto}
\alias{plot_pareto}
\title{Build a Pareto Chart}
\usage{
plot_pareto(
  to_plot,
  modality,
  frequency,
  xlab = modality,
  ylab = "",
  title = paste("Pareto Chart -", quo_name(xlab)),
  ylab2 = "\% cumulate",
  hline = 0.8,
  bar.color = "royalblue4",
  bar.fill = "royalblue3",
  bar.alpha = 0.6,
  line.color = "forestgreen",
  line.size = 1,
  hline.col = "coral2",
  group_over = NULL,
  others_name = "other"
)
}
\arguments{
\item{to_plot}{tibble of interest.}

\item{modality}{column you want to show the frequency.}

\item{frequency}{frequency of modality.}

\item{xlab}{title of x axis, default: "items".}

\item{ylab}{title of left y axis, default: "".}

\item{title}{title of the chart, default: "Pareto Chart".}

\item{ylab2}{title of right y axis, default: "\% cumulate".}

\item{hline}{yintercept of horizontal line, default: 0.8.}

\item{bar.color}{colour of bars, default: "royalblue4".}

\item{bar.fill}{fill of bars, default: "royalblue".}

\item{bar.alpha}{transparency of bars, default: 0.4.}

\item{line.color}{color of cumulate line, default: "green".}

\item{line.size}{size of cumulate line, default: 1.}

\item{hline.col}{color of horizontal line, default: "orange"}

\item{group_over}{number of max modalities to show. Sum the others. Default: NULL}

\item{others_name}{name to assign to modalities over group_over. Default: "other"}
}
\value{
Returns a Pareto Chart, ggplot2 object
}
\description{
The function builds a Pareto Chart.
Each bar corresponds to a different modality of the variable in modality
ordered by the sum of the frequencies.
}
\examples{
test <- dplyr::sample_n(tibble::tibble(letters = letters),100, replace=TRUE)
test$frequency <- round(runif(100, 1, 5),0)
plot_pareto(test, letters, frequency, group_over = 6)

}
