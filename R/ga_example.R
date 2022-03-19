#' @title ga_example
#'
#' @description This function will allow you to run a number of examples
#'
#' @param name the name of the example
#'
#' @details available examples: 'euclidean', 'manhattan', 'minkowski' and 'non-euclidean'
#'
#' @export
ga_example <- function(name){
  cleaned_name <- trimws(tolower(name));

  # if the euclidean simulation is chosen
  if (name == "euclidean"){
    example_euclidean_distance()
  }

  # if the manhattan simulation is chosen
  if (name == "manhattan"){
    example_manhattan_distance()
  }

  # if the minkowski simulation is chosen
  if (name == "minkowski"){
    example_minkowski_distance()
  }

  # if the non-euclidean simulation is chosen
  if (name == "non-euclidean-var"){
    example_non_euclidean_distance_var()
  }

  # if the non-euclidean simulation const is chosen
  if (name == "non-euclidean-const")

  # if the physics simulation is chosen
  if (name == "matter"){
    example_physics_engine()
  }

  # if the brachistochrone problem is chosen
  if (name == "brachistochrone"){
    example_brachistochrone()
  }
}
