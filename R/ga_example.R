#' @export
ga_example <- function(name){
  cleaned_name <- trimws(tolower(name));

  if (name == "euclidean"){
    example_euclidean_distance()
  }
}
