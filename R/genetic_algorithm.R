ga <- R6Class(
  classname = "genetic_algorithm",
  public = list(
    par = NULL,
    dim = NULL,
    population = NULL,
    operators = list(),
    dependents = list(),
    metrics = list(),
    time = 0,
    initialize = function(dim, initial, parameters = list()){
      self$dim <- as.vector(dim);
      self$par <- as.list(parameters);

      self$population <- initial(self, self$par);
    },
    add_operators = function(operators){
      if (is.list(operators)){
        for (operator in operators){
          self$add_operator(operator);
        }
      } else {
        self$add_operator(operators);
      }
    },
    add_operator = function(operator){
      self$operators <- append(self$operators, operator);
    },
    add_metric = function(name, value){
      if (!(name %in% names(self$metrics))){
        append_value <- list();
        append_value[[name]] = c(value);
        self$metrics <- append(self$metrics, append_value);
      } else {
        self$metrics[[name]] <- append(self$metrics[[name]], value);
      }
    },
    add_dependents = function(dependents){
      if (is.list(dependents)){
        for (dependent in dependents){
          self$add_dependent(dependents);
        }
      } else {
        self$add_dependent(dependents);
      }
    },
    add_dependent = function(dependent){
      self$dependents <- append(self$dependents, dependent);
    },
    next_operator = function(){
      operator_index <- (self$time %% length(self$operators)) + 1;
      self$population <- self$operators[[operator_index]](self, self$population, self$par);
      self$time <- self$time + 1;
    },
    next_iteration = function(){
      for (operator in self$operators){
        self$next_operator();
      }
    },
    run_ui = function(){
    }
  )
)
