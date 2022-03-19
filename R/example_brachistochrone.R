#' @title example_brachistochrone
#'
#' @description This function will run the brachistochrone example.
#'
#' @details This function is used by the ga_example.
#' @export
example_brachistochrone <- function(){
  # setup the settings questions
  allowed_settings <- list(
    list(
      name     = "generations",
      question = "How many generations should there be?",
      default  = 200
    )
  )

  # get the user settings
  settings <- get_settings(settings = allowed_settings, label = "Minimising the Euclidean Distance")

  # import the library
  library(gapackage)

  # setup an instance of the ga class object
  brach <- gapackage::ga$new(
    dim = c(10, 1000),
    parameters = list(
      initial_min     = 0,
      B_x             = 180,
      initial_max     = 10,
      A_y             = 40,
      geno_length     = 5,
      g               = 9.81,
      remove          = 0.5,
      add_proportion  = 0.5,
      mutation_size   = 0.0000007,
      generations     = as.numeric(settings$generations),
      population_size = 1000,
      maximise        = FALSE,
      location        = 0
    ),
    store_data = FALSE,
    initial = initial_rand_uni_2d
  );

  # add mutation and crossover operators
  brach$add_operators(
    operators = list(
      mutation_cauchy,
      crossover_basic_2d
    )
  )

  # add dependancies for the selection operator
  brach$add_dependents(
    dependents = list(
      fitness_function_single = utility_fitness_brachistochrone_2d,
      fitness_function = utility_fitness_population_2d
    )
  )

  # add the selection operator
  brach$add_operators(
    selection_basic_2d
  )

  # setup an output plot function
  brachistochrone_plot <- function(self, population, par){
    genotype_vector <- as.vector(population[,ncol(population)]);
    x_values <- 1:length(genotype_vector)
    y_values <- genotype_vector;

    return(list(
      x = x_values,
      y = y_values
    ))
  }

  # add the output graph
  brach$add_output_graph(
    list(
      xlab = "horizontal distance",
      ylab = "vertical distance",
      FUN = brachistochrone_plot
    )
  )

  # run the simulation for 200 generations
  for (i in 1:40){
    print(paste0("Current Generation: ", i))

    brach$next_iteration()

    fitnesses <- brach$metrics$average_fitness;
  }

  # plot the best member
  best <- brach$population[, dim(brach$population)[2]];
  plot(1:length(best), best, type = "l");


}
