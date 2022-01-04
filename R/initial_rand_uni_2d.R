initial_rand_uni_2d = function(self, par){
  return(
    array(
      runif(
        par$geno_length * par$population_size,
        min = par$initial_min,
        max = par$initial_max
      ),
      dim = c(par$geno_length, par$population_size)
    )
  )
}
