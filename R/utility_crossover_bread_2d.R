utility_crossover_bread_2d <- function(gen_1, gen_2){
  pivot_point <- sample(2:(length(gen_1) - 1), size = 1);
  return(
    c(gen_1[1:pivot_point], gen_2[(pivot_point + 1):length(gen_2)])
  )
}
