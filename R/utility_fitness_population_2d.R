# calculate fitness euclidean
utility_fitness_population_2d <- function(self, population, par){
  fitnesses <- apply(population, MARGIN = 2, FUN = function(genotype){
    return(self$dependents$fitness_function_single(self, genotype, par))
  })
  self$add_metric(name = "average_fitness", value = sum(fitnesses) / length(fitnesses))
  return(fitnesses)
}
