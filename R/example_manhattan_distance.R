#' @title example_manhattan_distance
#'
#' @description This function will run the manhattan distance example.
#'
#' @details This function is used by the ga_example.
#' @export
example_manhattan_distance <- function(){
  # setup the settings questions
  allowed_settings <- list(
    list(
      name     = "generations",
      question = "How many generations should there be?",
      default  = 200
    )
  )

  # get the user settings
  settings <- get_settings(settings = allowed_settings, label = "Minimising the Manhattan Distance")

  # setup an instance of the ga class object
  brach <- ga$new(
    dim = c(10, 1000),
    parameters = list(
      initial_min       = 0,
      initial_max       = 10,
      manhattan_start   = c(0, 0),
      manhattan_end     = c(10, 10),
      geno_length       = 4,
      remove_proportion = 0.5,
      add_proportion    = 0.5,
      mutation_size     = 0.0000007,
      generations       = as.numeric(settings$generations),
      population_size   = 1000,
      maximise          = FALSE,
      location          = 0
    ),
    store_data = FALSE,
    initial = initial_rand_uni_3d
  );

  # add mutation and crossover operators
  brach$add_operators(
    operators = list(
      mutation_cauchy,
      crossover_basic_3d
    )
  )

  # add dependancies for the selection operator
  brach$add_dependents(
    dependents = list(
      fitness_function_single = utility_fitness_manhattan_3d,
      fitness_function = utility_fitness_population_3d
    )
  )

  # add the selection operator
  brach$add_operators(
    selection_basic_3d
  )

  # run the simulation for 100 generations
  for (i in 1:brach$par$generations){
    cat(noquote((paste0("Current Simulation: ", i))), "\n")

    # run next iteration
    brach$next_iteration()

    # print the average fitness for the current generation
    all_average_fitnesses <- brach$metrics$average_fitness;
    cat(noquote(paste0("Current Average Fitness: ", all_average_fitnesses[length(all_average_fitnesses)])), "\n")
  }

  # plot the fitest member
  fitest_member <- brach$population[,,dim(brach$population)[3]];

  fitest_member <- rbind(brach$par$manhattan_start, fitest_member, brach$par$manhattan_end)
  plot(fitest_member[,1], fitest_member[,2], type = "l", ylab = "Y", xlab = "X", main = "Manhattan Distance")
}
