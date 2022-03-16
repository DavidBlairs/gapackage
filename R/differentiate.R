#' @title differentiate
#'
#' @description A function which will numerically differentiate an array of coordinates.
#'
#'
#' @param curve A 2D array with 2 columns where each row is a coordinate. The array is treated as ordered.
#'
#' @return A vector indicating the approximate gradient for each segment of the curve.
#' @export
differentiate <- function(curve){
  gradients <- c();
  for (segment_index in 1:(dim(curve)[1] - 1)){
    first_coordinate  <- curve[segment_index    , ];
    second_coordinate <- curve[segment_index + 1, ];

    y_diff <- (second_coordinate[2] - first_coordinate[2]);
    x_diff <- (second_coordinate[1] - first_coordinate[1]);

    gradient <- y_diff / x_diff;
    gradients <- append(gradients, gradient);
  }
  gradients <- append(gradients, gradient);
  return(gradients);
}
