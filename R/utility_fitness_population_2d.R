#' @title utility_fitness_population_2d
#'
#' @description This function will evaluate a given fitness function along a 2D population array.
#'
#' @param self a gapackage::ga class
#' @param population an array representing a 2D population of genotypes.
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function
#'
#' @details This function will also add a metric named 'average_fitness' to the ga object which will
#' contain the average fitness returned by the fitness_function_single() function.
#' There are no parameters and there is one dependancy:
#' \enumerate{
#'  \item **dependents$fitness_function_single** a function that can evaluate a 1D numeric genotype.
#' }
#'
#' @return The traversal time of the given path.
utility_fitness_population_2d <- function(self, population, par){
  fitnesses <- apply(population, MARGIN = 2, FUN = function(genotype){
    return(self$dependents$fitness_function_single(self, genotype, par))
  })
  self$add_metric(name = "average_fitness", value = sum(fitnesses) / length(fitnesses));
  return(fitnesses)
}
