% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-lex-data.R
\name{plot_lex_coverage}
\alias{plot_lex_coverage}
\title{Plot Coverage}
\usage{
plot_lex_coverage(data,
  start_date = min(lubridate::date(data$capture$DateTimeCapture)),
  end_date = max(lubridate::date(data$capture$DateTimeTagExpire)))
}
\arguments{
\item{data}{The \code{lex_data} object to plot.}

\item{start_date}{A date of the start.}

\item{end_date}{A date of the end.}
}
\description{
Plots the sections with receivers and coverage areas with alpha equivalent to
deployment during period of interest.
}

