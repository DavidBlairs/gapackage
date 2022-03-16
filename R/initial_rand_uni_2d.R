#' @title initial_rand_uni_2d
#'
#' @description A function to generate an initial population as a 2D array. Each element will be
#' taken from a uniform distribution.
#' The function should never be run explicitly, only passed to the gapackage::ga$new() function
#' as the 'initial' parameter.
#'
#'
#' @param self a gapackage::ga class
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function
#'
#' @details This algorithm will require no dependency functions but does require four parameters to be passed
#' to the gapackage::ga$new function. They are as follows:
#' \enumerate{
#'  \item **par$geno_length** an integer indicating the length of a single genotype (the number of rows in the population array)
#'  \item **par$population_size** the number of genotypes in the entire population.
#'  \item **par$initial_max** the 'max' parameter passed to the runif() function.
#'  \item **par$initial_min** the 'min' parameter passed to the runif() function.
#' }
#'
#' @return an array representing a 2D population of genotypes
#' @export
initial_rand_uni_2d = function(self, par){
  return(
    array(
      runif(
        par$geno_length * par$population_size,
        min = par$initial_min,
        max = par$initial_max
      ),
      dim = c(par$geno_length, par$population_size)
    )
  )
}
