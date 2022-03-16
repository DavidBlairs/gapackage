#' @title utility_crossover_breed_3d
#'
#' @description This function will 'breed' two 2D genotypes using a basic crossover algorithm. It is designed
#' to be used as a dependancy function for the crossover_basic_3d() function.
#'
#' @param gen_1 a 2D array of numbers representing a genotype.
#' @param gen_2 a 2D array of numbers representing a genotype.
#'
#' @details this function requires no parameters or dependancies.
#'
#' @return a vector containing the two resulting children.
#' @export
utility_crossover_bread_3d <- function(gen_1, gen_2){
  pivot_point <- sample(2:(dim(gen_1)[1] - 1), size = 1);
  return(rbind(gen_1[1:pivot_point, ], gen_2[(pivot_point + 1):dim(gen_2)[1], ]))
}
