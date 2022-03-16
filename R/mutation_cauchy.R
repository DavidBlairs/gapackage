#' @title mutation_cauchy
#'
#' @description A function which will add a numeric value taken from a cauchy distribution to each
#' value of the population.
#' The function should never be run explicitly, only passed as a parameter to the add_operator()
#' function of the gapackage::ga class.
#'
#'
#' @param self a gapackage::ga class
#' @param population an array of any dimensions representing a population of genotypes.
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function
#'
#' @details This algorithm will require no dependency functions but does require two parameters to be passed
#' to the gapackage::ga$new function. They are as follows:
#' \enumerate{
#'  \item **par$location** the location parameter of the cauchy distribution.
#'  \item **par$population_size** the scale parameter of the cauchy distribution.
#' }
#'
#' @return an array representing a population of genotypes with the same dimensions as the population parameter.
#' @export
mutation_cauchy <- function(self, population, par){
  change <- array(
    rcauchy(prod(dim(population)), location = par$location, scale = par$mutation_size),
    dim = dim(population)
  );
  return(
    array(as.numeric(population) + as.numeric(change), dim = dim(population))
  )
}
