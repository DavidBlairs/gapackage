% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_add_start_end_2d.R
\name{utility_add_start_end_2d}
\alias{utility_add_start_end_2d}
\title{utility_add_start_end_2d}
\usage{
utility_add_start_end_2d(genotype, par)
}
\arguments{
\item{genotype}{a 1D array of numbers representing a genotype.}

\item{par}{a named list containing initial parameters passed to the gapackage::ga$new function.}
}
\value{
a 1D array representing a genotype.
}
\description{
This function will add two values to the start and end of a 1D genotype (in a 2D context).
It is designed to be used as a dependency function for the utility_fitness_brachistochrone_2d() function.
}
\details{
this function requires no dependencies but takes two parameters which are as follows:
\enumerate{
\item \strong{par$A_y} THe total numeric height of the traversal space.
\item \strong{par$B_x} THe total numeric width of the traversal space.
}
}
