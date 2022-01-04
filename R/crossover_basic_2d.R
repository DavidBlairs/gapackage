crossover_basic_2d <- function(self, population, par){
  number_of_offspring <- as.integer(par$remove * par$population_size);
  chosen_people <- population[,sample(1:ncol(population), size = number_of_offspring * 2, replace = F)];

  offspring <- matrix(ncol = 0, nrow = nrow(population));
  for (genotype in 1:(ncol(chosen_people) / 2)){

    gen1 <- chosen_people[, genotype];
    gen2 <- chosen_people[, ncol(chosen_people) + 1 - genotype];

    current_offspring <- crossover_bread(gen1, gen2) %>% matrix(ncol = 1, nrow = nrow(population));
    offspring <- cbind(offspring, current_offspring);
  }
  return(cbind(population, offspring))
}
