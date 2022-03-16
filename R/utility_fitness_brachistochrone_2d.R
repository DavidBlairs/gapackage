#' @title utility_fitness_brachistochrone_2d
#'
#' @description This function will calculate the time taken to traverse a path in 2D space. It
#' will use a 1D vector representing the height of points along the path at discrete intervals. It will
#' add two values, par$A_y to the start of the vector and 0 to the end of the vector before calculating the time.
#'
#' @param self a gapackage::ga class
#' @param genotype a 1D vector indicating the height of the curve at discrete intervals.
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function
#'
#' @details There are no dependancies but there are three parameters that need to be passed to the parameter arguement of the gapackage::ga$new function. They are as follows:
#' \enumerate{
#'  \item **par$A_y** The numric height to be added to the beginning of the genotype vector.
#'  \item **par$g** the gravitational constant.
#'  \item **par$B_x** THe total numeric width of the traversal space.
#' }
#'
#' @return The traversal time of the given path.
#' @export
utility_fitness_brachistochrone_2d <- function(self, genotype, par){
  # add the first and last height to the genotype
  complete_genotype <- c(par$A_y, genotype, 0);

  # calculate the drop in height from A_y to each height
  height_drop <- abs(par$A_y - complete_genotype);

  # determine the velocities (loss in GPE is gain in KE)
  velocities <- sqrt(2 * height_drop * par$g);
  total_time <- 0;

  # this is the length of the gap between heights
  x_diff <- abs(par$B_x / (length(genotype) + 1));

  # for each edge in our curve
  for (segment_index in 1:(length(genotype) + 1)){
    # calculate the difference in height between the two heights of the segment
    y_diff <- abs(complete_genotype[segment_index] - complete_genotype[segment_index + 1]);

    # get the coordinates of the segment
    first_coordinate  <- c(x_diff * (segment_index - 1),  complete_genotype[segment_index]);
    second_coordinate <- c(x_diff * (segment_index),  complete_genotype[segment_index + 1]);

    # calculate the distance between the coordinates
    distance <- sqrt(sum(abs(first_coordinate - second_coordinate)**2));

    # calculate acceleration in the direction of the segment
    acceleration <- par$g * (y_diff / x_diff);

    # determine the time according to the equations of motion
    total_time <- total_time + ((2 * distance) / (velocities[segment_index] + velocities[segment_index + 1]));

  }
  return(total_time)
}
