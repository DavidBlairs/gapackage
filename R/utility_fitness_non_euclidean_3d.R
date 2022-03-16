#' @title utility_fitness_non_euclidean_3d
#'
#' @description A function to calculate the non-euclidean length of a curve in a 3D context.
#'
#' @param self a gapackage::ga class
#' @param genotype an array representing a 2D  genotypes
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function
#'
#' @details This algorithm will require no dependency functions but does require two parameters to be passed
#' to the gapackage::ga$new function. They are as follows:
#' \enumerate{
#'  \item **par$non_euclidean_start** The starting coordinate to be added to the beginning of the curve.
#'  \item **par$non_euclidean_end** The final coordinate to be added to the end of the curve.
#'  \item **par$non_euclidean_bounds** The bounds on the integral.
#'  \item **par$non_euclidean_A** A 2x2 matrix representing the relative transformation to the space.
#' }
#'
#' @return a number representing the total distance.
uility_fitness_non_euclidean_3d <- function(self, genotype, par){
  complete_genotype <- rbind(par$non_euclidean_start, genotype, par$non_euclidean_end);

  if (is.null(par$non_euclidean_A)){
    par$non_euclidean_A <- array(c(1, 0, 0, 1), dim = c(2, 2));
  };

  bounds <- par$non_euclidean_bounds;
  curve <- complete_genotype;

  differential <- differentiate(genotype);

  time_interval <- (bounds[2] - bounds[1]) / (dim(curve)[1] - 1);
  time_stamps   <- ((1:dim(curve)[1]) - 1) * time_interval;

  differential <- array(append(time_stamps, differential), dim = dim(curve));

  total_area <- 0;
  for (time_index in 1:(dim(curve)[1] - 1)){
    current_gradient <- differential[time_index, ];
    A_component <- current_gradient %*% par$non_euclidean_A;
    magnitude   <- sqrt(sum(A_component * current_gradient));

    local_area <- magnitude * time_interval;
    total_area <- total_area + local_area;
  }
  return(total_area)
}
