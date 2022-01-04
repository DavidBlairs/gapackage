mutation_cauchy <- function(self, population, par){
  change <- array(
    rcauchy(prod(dim(population)), location = par$location, scale = par$mutation_size),
    dim = dim(population)
  );
  return(
    array(as.numeric(population) + as.numeric(change), dim = dim(population))
  )
}
