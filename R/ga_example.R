#' @title ga_example
#'
#' @description This function will allow you to run a number of examples
#'
#' @param name the name of the example
#'
#' @details available examples: 'euclidean'
#'
#' @export
ga_example <- function(name){
  cleaned_name <- trimws(tolower(name));

  if (name == "euclidean"){
    example_euclidean_distance()
  }
}
