#' @title example_non_euclidean_distance_var
#'
#' @description This function will run the non-euclidean distance example with a variable matrix A.
#'
#' @details This function is used by the ga_example.
#' @export
example_non_euclidean_distance_var <- function(){
  # setup the settings questions
  allowed_settings <- list(
    list(
      name     = "generations",
      question = "How many generations should there be?",
      default  = 200
    )
  )

  # get the user settings
  settings <- get_settings(settings = allowed_settings, label = "Minimising the Non-Euclidean Distance (var)")

  cat(noquote("\nThis example will use a matrix A derived from the function f(x, y) = x^2 + y^2.\n"))

  # setup an instance of the ga class object
  brach <- ga$new(
    dim = c(10, 1000),
    parameters = list(
      initial_min          = 0,
      initial_max          = 10,
      non_euclidean_start  = c(10, 0),
      non_euclidean_end    = c(0, 10),
      non_euclidean_bounds = c(0, 1),
      geno_length          = 5,
      remove_proportion    = 0.5,
      add_proportion       = 0.5,
      mutation_size        = 0.0000000007,
      generations          = as.numeric(settings$generations),
      population_size      = 2000,
      maximise             = FALSE,
      location             = 0
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

  # calculate the relative A matrix for the function f(x, y) = x^2 + y^2
  get_A <- function(coordinate){
    x <- coordinate[1];
    y <- coordinate[2];

    return(array(c(
      1 + (4*(x^2)), 4*x*y, 4*x*y, 1 + (4*(y^2))
    ), dim = c(2, 2)))
  }

  # overriding the default function so that it is relative to the location in the space
  utility_fitness_non_euclidean_3d <- function(self, genotype, par){
    complete_genotype <- rbind(par$non_euclidean_start, genotype, par$non_euclidean_end);

    bounds <- par$non_euclidean_bounds;
    curve <- complete_genotype;

    time_interval <- (bounds[2] - bounds[1]) / (dim(curve)[1] - 1);
    differential <- differentiate(curve, time_interval);


    time_stamps   <- ((1:dim(curve)[1]) - 1) * time_interval;

    total_area <- 0;
    for (time_index in 1:(dim(curve)[1] - 1)){
      first_coordinate <- curve[time_index, ];
      second_coordinate <- curve[time_index + 1, ];

      average_coordinate <- (first_coordinate + second_coordinate) / 2;

      current_gradient <- as.vector(differential[time_index, ]);
      A_component <- current_gradient %*% get_A(average_coordinate);
      magnitude   <- sqrt(sum(A_component * current_gradient));

      local_area <- magnitude * time_interval;
      total_area <- total_area + local_area;
    }
    return(total_area)
  }

  # add dependancies for the selection operator
  brach$add_dependents(
    dependents = list(
      fitness_function_single = utility_fitness_non_euclidean_3d,
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

    # plot the fitest member
    fitest_member <- brach$population[,,1];

    if (i %% 10 == 0){
      fitest_member <- rbind(brach$par$non_euclidean_start, fitest_member, brach$par$non_euclidean_end)
      plot(fitest_member[,1], fitest_member[,2], type = "l", ylab = "Y", xlab = "X")
    }
  }

  best_curve <- as.data.frame(fitest_member);
  colnames(best_curve) <- c("X", "Y");

  write.csv(best_curve, file = "best_curve.csv")
}

