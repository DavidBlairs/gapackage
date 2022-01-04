# perform selection
selection_basic_2d <- function(self, population, par){
  number_to_remove <- as.integer(par$remove * par$population_size);

  fitnesses <- self$dependents$fitness_function(self, population, par);
  fitness_order <- order(fitnesses, decreasing = !par$maximise);

  new_population <- population[, fitness_order];
  new_population <- new_population[ , (number_to_remove + 1):(ncol(new_population))];
  return(new_population)
}


