% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_fitness_population_2d.R
\name{utility_fitness_population_2d}
\alias{utility_fitness_population_2d}
\title{utility_fitness_population_2d}
\usage{
utility_fitness_population_2d(self, population, par)
}
\arguments{
\item{self}{a gapackage::ga class}

\item{population}{an array representing a 2D population of genotypes.}

\item{par}{a named list containing initial parameters passed to the gapackage::ga$new function}
}
\value{
The traversal time of the given path.
}
\description{
This function will evaluate a given fitness function along a 2D population array.
}
\details{
This function will also add a metric named 'average_fitness' to the ga object which will
contain the average fitness returned by the fitness_function_single() function.
There are no parameters and there is one dependancy:
\enumerate{
\item \strong{dependents$fitness_function_single} a function that can evaluate a 1D numeric genotype.
}
}
