#' @title utility_add_start_end_3d
#'
#' @description This function will add two coordinates to the start and end of a 2D genotype (in a 3D context).
#' It is designed to be used as a dependency function for the utility_fitness_brachistochrone_3d() function.
#'
#' @param genotype a 2D array of numbers representing a genotype.
#' @param par a named list containing initial parameters passed to the gapackage::ga$new function.
#'
#' @details this function requires no dependencies but takes two parameters which are as follows:
#' \enumerate{
#'  \item **par$A_y** THe total numeric height of the traversal space.
#'  \item **par$B_x** THe total numeric width of the traversal space.
#' }
#'
#' @return a 2D array representing a genotype.
utility_add_start_end_3d <- function(genotype, par){
  return(
    rbind(
      array(c(0, par$A_y), dim = c(1, 2)), genotype,
      array(c(par$B_x, 0), dim = c(1, 2)))
  )
}
