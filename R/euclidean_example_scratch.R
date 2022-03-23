# I've wrapped this code in a function to avoid it being accessible from the gapackage. I've only included it here
# to make it easier for you to access. To run it, take the code out of the function and place it in a seperate file.
# Then, click source as you usually would.
euclidean_example_scratch <- function(){
  library(dplyr)
  library(abind)

  # Some global parameters for the problem
  A <- c(10, 5);
  B <- c(40, 20);

  initial_min <- 0;
  initial_max <- 40;

  R                     <- 5;
  population_size       <- 1000;
  remove_add_percentage <- 0.5;
  mutation_size         <- 0.00000007;

  generations <- 200;
  descending <- TRUE;

  # generate an initial population sampled from a uniform distribution
  initial_population_uniform <- function(min, max, genotype_length, number_of_genotypes){
    # Change 'runif' if a different distribution is wanted
    return(
      array(
        runif(
          genotype_length * number_of_genotypes * 2,
          min = min,
          max = max
        ),
        dim = c(genotype_length, 2, number_of_genotypes)
      )
    )
  }

  # genotype length is 2 since we will be adding the start and end coordinates
  # at a later stage. Since these values are known, they don't need to be determined.
  current_population <- initial_population_uniform(
    min                 = min(c(A, B)),
    max                 = max(c(A, B)),
    genotype_length     = 2,
    number_of_genotypes = 1000
  )

  # add on a random number to each element from a cauchy distribution
  mutation_cauchy <- function(population, location, spread){
    change <- array(
      rcauchy(prod(dim(population)), location = location, scale = spread),
      dim = dim(population)
    );
    return(
      array(as.numeric(population) + as.numeric(change), dim = dim(population))
    )
  }

  # bread two genotypes
  crossover_bread <- function(gen_1, gen_2){
    pivot_point <- sample(2:(dim(gen_1)[1] - 1), size = 1);
    return(rbind(gen_1[1:pivot_point, ], gen_2[(pivot_point + 1):dim(gen_2)[1], ]))
  }

  # apply the crossover algorithm
  crossover_basic <- function(population, number_of_offspring){
    # Get the people chosen for crossover
    chosen_people <- population[ , , sample(1:dim(population)[3],
                                            size = number_of_offspring * 2, replace = F)];

    offspring <- array(dim = c(dim(population)[1], 2, 0));
    # for each child needed:
    for (genotype in 1:(dim(chosen_people)[3] / 2)){
      # figure out who their parents will be (already randomly sorted)
      gen1 <- chosen_people[, , genotype];
      gen2 <- chosen_people[, , dim(chosen_people)[3] + 1 - genotype];

      # create the child
      current_offspring <- crossover_bread(gen1, gen2);

      offspring <- abind(offspring, current_offspring, along = 3);
    }
    return(abind(population, offspring, along = 3));
  }

  # add the start and end coordinates
  add_start_end <- function(genotype){
    return(
      rbind(
        array(A, dim = c(1, 2)), genotype,
        array(B, dim = c(1, 2)))
    )
  }

  # function to calculate euclidean distance
  fitness_function_euclidean <- function(genotype){
    genotype_complete <- add_start_end(genotype);

    distance <- 0;
    for (segment_index in 1:(dim(genotype_complete)[1] - 1)){
      diff <- abs(genotype_complete[segment_index, ] - genotype_complete[segment_index + 1, ])
      distance <- distance + sqrt(sum(diff^2));
    }
    return(distance)
  }

  # calculate fitness euclidean
  fitness_function <- function(population){
    fitnesses <- apply(population, MARGIN = 3, FUN = function(genotype){
      return(fitness_function_euclidean(genotype))
    })
    return(fitnesses)
  }

  # apply the selection operator
  selection <- function(population, number_to_remove){
    fitnesses <- fitness_function(population);
    fitness_order <- order(fitnesses, decreasing = descending);
    new_population <- population[, ,fitness_order];
    new_population <- new_population[ , , (number_to_remove + 1):(dim(new_population)[3])];
    return(new_population)
  }

  # create the initial population
  initial_population <- initial_population_uniform(
    min = initial_min,
    max = initial_max,
    genotype_length = R,
    number_of_genotypes = population_size
  );
  current_population <- initial_population;

  fitnesses <- c();

  start.time <- Sys.time();

  # run the simulation 100 times
  for (i in 1:generations){
    print(paste0("Current Generation: ", i, "/", generations))
    # apply each of the operators
    current_population <- current_population %>%
      mutation_cauchy(location = 0, spread = mutation_size) %>%
      crossover_basic(number_of_offspring = as.integer(remove_add_percentage * population_size)) %>%
      selection(number_to_remove = as.integer(remove_add_percentage * population_size));

    # get the best perfoming genotype and its corresponding fitness
    fitest <- current_population[, ,1];
    fitest_fitness <- fitness_function_euclidean(fitest);
    fitnesses <-  append(fitnesses, fitest_fitness);

    # if this is the first generation, save the genotype and its fitness
    if (i == 1){
      initial_fitness <- as.numeric(fitest_fitness);
      initial_member <- add_start_end(fitest);
    }

    # print the fitness of each generation
    print(paste0("Current Fitness: ", fitest_fitness));
  }

  fitest <- add_start_end(fitest)

  # plot the initial best member, the fitnesses and the best member of the
  # last generation
  plot(initial_member[,1], initial_member[,2], type = "l")
  plot(fitest[,1], fitest[,2], type = "l", xlab = "x_values", ylab = "y_values")
  plot(1:length(fitnesses), fitnesses, type = "l", xlab = "generation", ylab = "average fitness")
  abline(a = 33.54, b = 0, lty = 2)
  print("====================================")
  print(paste0("Elapsed Time (seconds): ", Sys.time() - start.time))
  print("====================================")
  print(paste0("Inital Best Fitness: ", initial_fitness))
  print(paste0("Final Best Fitness : ", fitest_fitness))
}

