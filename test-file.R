library(gapackage)

brach <- gapackage::ga$new(
  dim = c(1000, 10),
  parameters = list(
    initial_min     = 0,
    B_x             = 40,
    initial_max     = 10,
    A_y             = 10,
    geno_length     = 10,
    g               = 9.81,
    remove          = 0.5,
    mutation_size   = 0.0000007,
    generations     = 100,
    population_size = 1000,
    maximise        = FALSE,
    location        = 0
  ),
  initial = initial_rand_uni_2d
);

brach$add_operators(
  operators = list(
    mutation_cauchy,
    crossover_basic_2d
  )
)

brach$add_dependents(
  dependents = list(
    fitness_function_single = utility_fitness_brachistochrone_2d,
    fitness_function = utility_fitness_population_2d
  )
)

brach$add_operators(
    selection_basic_2d
)


