#' @title utility_add_start_end_2d
#'
#' @description This function will add two values to the start and end of a 1D genotype (in a 2D context).
#' It is designed to be used as a dependency function for the utility_fitness_brachistochrone_2d() function.
#'
#' @param genotype a 1D array of numbers representing a genotype.
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function.
#'
#' @details this function requires no dependencies but takes two parameters which are as follows:
#' \enumerate{
#'  \item **par$A_y** THe total numeric height of the traversal space.
#'  \item **par$B_x** THe total numeric width of the traversal space.
#' }
#'
#' @return a 1D array representing a genotype.
utility_add_start_end_2d <- function(genotype, par){
  return(c(par$A_y, genotype, 0))
}
