% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_figure.R
\name{make_figure}
\alias{make_figure}
\title{Make a figure with experimental data and fitted binding curve}
\usage{
make_figure(fits, probe_concentration = NULL, output_directory = NULL,
  plot_format = "png")
}
\arguments{
\item{fits}{A named list where items are named after the corresponding
experiment and contain a model object from \code{\link[minpack.lm]{nlsLM}}.
The output of \code{\link{fit_binding_model}} can be used directly as
input here.}

\item{probe_concentration}{The fixed concentration of probe molecule. This is
ignored for models obtained with the hyperbolic and hill equations, but
required for models obtained with the quadratic equation.}

\item{output_directory}{An optional directory name where to write plots. This
directory will be created if it does not already exist.}

\item{plot_format}{A character string indicating the file format to use to
save plots. Possible values are \code{"png"} (default value),
\code{"pdf"} and \code{"svg"}.}
}
\value{
A named list where items are named after the corresponding experiment
    and contain a \code{\link[ggplot2]{ggplot}} graph object of a binding
    curve figure.
}
\description{
This function takes a list containing any number of model
    objects from \code{\link{fit_binding_model}} or \code{\link[stats]{nls}}
    and returns a binding curve plot figure for each model.
}
