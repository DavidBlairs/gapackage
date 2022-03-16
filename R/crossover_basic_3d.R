#' @title crossover_basic_3d
#'
#' @description A function to perform a basic crossover algorithm on a 3D population.
#' It will split and merge a random selection of genotypes a predetermined number of times.
#' The function should never be run explicitly, only passed as a parameter to the add_operator()
#' function of the gapackage::ga class.
#'
#' @param self a gapackage::ga class
#' @param population an array representing a 3D population of genotypes
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function
#'
#' @details This algorithm will require no dependency functions but does require two parameters to be passed
#' to the gapackage::ga$new function. They are as follows:
#' \enumerate{
#'  \item **par$add_proportion** the number of genotypes be added as a percentage of the total population size.
#'  \item **par$population_size** the number of genotypes in the entire population.
#' }
#'
#' @return an array representing a 2D population of genotypes
crossover_basic_3d <- function(self, population, par){
  # Get the people chosen for crossover
  number_of_offspring <- as.integer(par$add_proportion * par$population_size);
  chosen_people <- population[ , , sample(1:dim(population)[3], size = number_of_offspring * 2, replace = F)];

  offspring <- array(dim = c(dim(population)[1], 2, 0));
  # for each child needed:
  for (genotype in 1:(dim(chosen_people)[3] / 2)){
    # figure out who their parents will be (already randomly sorted)
    gen1 <- chosen_people[, , genotype];
    gen2 <- chosen_people[, , dim(chosen_people)[3] + 1 - genotype];

    # create the child
    current_offspring <- utility_crossover_bread_3d(gen1, gen2);

    offspring <- abind::abind(offspring, current_offspring, along = 3);
  }
  return(abind::abind(population, offspring, along = 3));
}
