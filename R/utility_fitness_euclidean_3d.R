#' @title utility_fitness_euclidean_3d
#'
#' @description A function to calculate the euclidean length of a curve in a 3D context.
#'
#'
#' @param self a gapackage::ga class
#' @param genotype an array representing a 2D  genotypes
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function
#'
#' @details This algorithm will require no dependency functions but does require two parameters to be passed
#' to the gapackage::ga$new function. They are as follows:
#' \enumerate{
#'  \item **par$euclidean_start** The starting coordinate to be added to the beginning of the curve.
#'  \item **par$euclidean_end** The final coordinate to be added to the end of the curve.
#' }
#'
#' @return a number representing the total distance.
#' @export
utility_fitness_euclidean_3d <- function(self, genotype, par){
  complete_genotype <- rbind(par$euclidean_start, genotype, par$euclidean_end);

  total_distance <- 0;
  for (segment_index in 1:(nrow(complete_genotype) - 1)){
    first_coordinate  <- complete_genotype[segment_index, ];
    second_coordinate <- complete_genotype[segment_index + 1, ];

    diff <- abs(second_coordinate - first_coordinate);
    total_distance <- total_distance + sqrt(sum(diff^2))
  }
  return(total_distance)
}
