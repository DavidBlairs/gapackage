#' @title utility_fitness_brachistochrone_3d
#'
#' @description This function will calculate the time taken to traverse a path in 2D space. It
#' will use a 2D array representing the coordinates of points along the path. It will
#' add two values, (0, par$A_y) to the start of the array and (par$B_x, 0) to the end of the array before calculating the time.
#'
#' @param self a gapackage::ga class
#' @param genotype a 2D array indicating the coordinates of points on the curve.
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function
#'
#' @details There are no dependancies but there are three parameters that need to be passed to the parameter arguement of the gapackage::ga$new function. They are as follows:
#' \enumerate{
#'  \item **par$A_y** THe total numeric height of the traversal space.
#'  \item **par$g** the gravitational constant.
#'  \item **par$B_x** THe total numeric width of the traversal space.
#' }
#'
#' @return The traversal time of the given path.
utility_fitness_brachistochrone_3d <- function(self, genotype, par){
  genotype <- utility_add_start_end_3d(genotype, par);

  # calculate the drop in height from A_y to each height
  height_drop <- abs(par$A_y - genotype[,2]);

  # determine the velocities (loss in GPE is gain in KE)
  velocities <- sqrt(2 * height_drop * par$g);
  total_time <- 0;

  # for each edge in our curve
  for (segment_index in 1:(dim(genotype)[1] - 1)){
    # calculate the difference in height between the two heights of the segment
    y_diff <- abs(genotype[segment_index, 2] - genotype[segment_index + 1, 2]);
    x_diff <- abs(genotype[segment_index, 1] - genotype[segment_index + 1, 1]);

    # get the coordinates of the segment
    first_coordinate  <- c(genotype[segment_index, 1],  genotype[segment_index, 2]);
    second_coordinate <- c(genotype[segment_index + 1, 1],  genotype[segment_index + 1, 2]);

    # calculate the distance between the coordinates
    distance <- sqrt(sum(abs(first_coordinate - second_coordinate)**2));

    # calculate acceleration in the direction of the segment
    acceleration <- par$g * (y_diff / x_diff);

    # determine the time according to the equations of motion
    total_time <- total_time + ((2 * distance) / (velocities[segment_index] + velocities[segment_index + 1]));

  }
  return(total_time)
}
