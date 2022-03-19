#' @title differentiate
#'
#' @description A function which will numerically differentiate an array of coordinates.
#'
#'
#' @param curve A 2D array with 2 columns where each row is a coordinate. The array is treated as ordered.
#'
#' @return A vector indicating the approximate gradient for each segment of the curve.
#' @export
differentiate <- function(curve, time_interval){
  gradients <- array(dim = c(0, 2));
  for (segment_index in 1:(dim(curve)[1] - 1)){
    first_coordinate  <- curve[segment_index    , ];
    second_coordinate <- curve[segment_index + 1, ];

    gradient <- (second_coordinate - first_coordinate) / time_interval;

    gradients <- rbind(gradients, gradient);
  }
  gradients <- rbind(gradients, gradient);
  return(gradients);
}
