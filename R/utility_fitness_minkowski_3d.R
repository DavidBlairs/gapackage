#' @title utility_fitness_minkowski_3d
#'
#' @description A function to calculate the minkowski length of a curve in a 3D context.
#'
#'
#' @param self a gapackage::ga class
#' @param genotype an array representing a 2D genotypes
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function
#'
#' @details This algorithm will require no dependency functions but does require two parameters to be passed
#' to the gapackage::ga$new function. They are as follows:
#' \enumerate{
#'  \item **par$minkowski_start** The starting coordinate to be added to the beginning of the curve.
#'  \item **par$minkowski_end** The final coordinate to be added to the end of the curve.
#'  \item **par$minkowski_p** The parameter p for the minkowski length.
#' }
#'
#' @return a number representing the total distance.
utility_fitness_minkowski_3d <- function(self, genotype, par){
  complete_genotype <- rbind(par$minkowski_start, genotype, par$minkowski_end);

  total_distance <- 0;
  for (segment_index in 1:(nrow(complete_genotype) - 1)){
    first_coordinate <- complete_genotype[segment_index, ];
    second_coordinate <- complete_genotype[segment_index + 1, ];

    diff <- abs(second_coordinate - first_coordinate);

    total_distance <- total_distance + sum(diff^par$minkowski_p)^(1/par$minkowski_p);
  }
  return(total_distance)
}
