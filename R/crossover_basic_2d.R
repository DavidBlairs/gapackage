#' @title crossover_basic_2d
#'
#' @description A function to perform a basic crossover algorithm on a 2D population.
#' It will split and merge a random selection of genotypes a predetermined number of times.
#' The function should never be run explicitly, only passed as a parameter to the add_operator()
#' function of the gapackage::ga class.
#'
#'
#' @param self a gapackage::ga class
#' @param population an array representing a 2D population of genotypes
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
#' @export
crossover_basic_2d <- function(self, population, par){
  number_of_offspring <- as.integer(par$add_proportion * par$population_size);
  chosen_people <- population[,sample(1:ncol(population), size = number_of_offspring * 2, replace = F)];

  offspring <- matrix(ncol = 0, nrow = nrow(population));
  for (genotype in 1:(ncol(chosen_people) / 2)){

    gen1 <- chosen_people[, genotype];
    gen2 <- chosen_people[, ncol(chosen_people) + 1 - genotype];

    current_offspring <- utility_crossover_breed_2d(gen1, gen2) %>% matrix(ncol = 1, nrow = nrow(population));
    offspring <- cbind(offspring, current_offspring);
  }
  return(cbind(population, offspring))
}
