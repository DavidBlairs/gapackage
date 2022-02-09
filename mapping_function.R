
# function to approximately differentiate a paired vector array
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


# calculate the length of a parametrically defined curve from a
# paired vector array.
curve_length <- function(curve, A = NULL, bounds = c(0, 1)){
  if (is.null(A)){
    A <- array(c(1, 0, 0, 1), dim = c(2, 2));
  };

  time_interval <- (bounds[2] - bounds[1]) / (dim(curve)[1] - 1);
  differential <- differentiate(curve, time_interval);


  time_stamps   <- ((1:dim(curve)[1]) - 1) * time_interval;

  total_area <- 0;
  for (time_index in 1:(dim(curve)[1] - 1)){
    current_gradient <- as.vector(differential[time_index, ]);
    A_component <- current_gradient %*% A;
    magnitude   <- sqrt(sum(A_component * current_gradient));

    local_area <- magnitude * time_interval;
    total_area <- total_area + local_area;
  }
  return(total_area)
}

curve_array <- array(data = c(1:10, 1:10), dim = c(10, 2));
