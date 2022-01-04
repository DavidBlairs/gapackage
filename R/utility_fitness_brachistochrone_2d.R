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
