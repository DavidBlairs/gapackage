#' @title selection_basic_3d
#'
#' @description This function can be used to sort a 3D population array according to a given selection
#' criteria. This function will not run unless the required dependencies are provided.
#' The function should never be run explicitly, only passed as a parameter to the add_operator()
#' function of the gapackage::ga class.
#'
#'
#' @param self a gapackage::ga class
#' @param population an array representing a 3D population of genotypes.
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function
#'
#' @details This algorithm will require 1 direct dependency and 1 indirect dependency. They are listed below respectively:
#' \enumerate{
#'  \item **self$dependents$fitness_function** a function which can evaluate the fitnesses of a 3D population and return a vector of numeric values.
#'  \item **self$dependents$fitness_function_single** a function which can evalue the fitness of a single 2D genotype.
#' }
#'
#' There are three parameters that need to be passed to the parameter arguement of the gapackage::ga$new function. They are as follows:
#' \enumerate{
#'  \item **par$remove_proportion** the number of genotypes to be removed as a percentage of the total population size.
#'  \item **par$population_size** the number of genotypes in the entire population.
#'  \item **par$maximise** a boolean indicating whether the fitness function should be maximised or minimised.
#' }
#'
#' @return an array representing a population of genotypes with the same dimensions as the population parameter.
#' @export
selection_basic_3d <- function(self, population, par){
  number_to_remove <- as.integer(par$remove_proportion * par$population_size);

  fitnesses <- self$dependents$fitness_function(self, population, par);
  fitness_order <- order(fitnesses, decreasing = !par$maximise);

  new_population <- population[, ,fitness_order];
  new_population <- new_population[ , , (number_to_remove + 1):(dim(new_population)[3])];
  return(new_population)
}
