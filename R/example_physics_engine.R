#' @title example_physics_engine
#'
#' @description This function will run the physics engine example.
#'
#' @details This function is used by the ga_example.
#' @export
example_physics_engine <- function(){
  # setup an instance of the ga class object
  brach <- gapackage::ga$new(
    dim = c(10, 1000),
    parameters = list(
      initial_min     = 0,
      B_x             = 180,
      initial_max     = 300,
      A_y             = 500,
      geno_length     = 2,
      g               = 9.81,
      remove          = 0.5,
      add_proportion  = 0.5,
      mutation_size   = 0.0000007,
      generations     = 100,
      population_size = 1000,
      maximise        = FALSE,
      location        = 0
    ),
    store_data = FALSE,
    initial = initial_rand_uni_2d
  );

  downloads_dir <- "C:/Users/david.blair/Downloads/";

  # add mutation and crossover operators
  brach$add_operators(
    operators = list(
      mutation_cauchy,
      crossover_basic_2d
    )
  )


  remove_file <- function(name){
    if (file.exists(name)){
      file.remove(name)
    }
  }

  write_file <- function(name, text){
    remove_file(name = name);

    file_conn <- file(name)
    writeLines(c(text), file_conn)
    close(file_conn)
  }

  get_file_contents <- function(name){
    file_conn <- file(name);
    return(readLines(file_conn))
  }

  get_sim_end_recent <- function(){
    path_root <- paste0(downloads_dir, "simulation_end*.txt");
    matches <- Sys.glob(path_root);
    return(matches[length(matches)])
  }

  # run the simulation for specified heights
  run_simulation <- function(session, heights, timeout = 10){
    # remove existing indicator files
    remove_file(name = "simulation_results")
    start_time <- as.numeric(Sys.time());
    write_file(name = "simulation_start_time", text = as.character(start_time))

    # begin the simulation
    add_curve(session, heights);
    add_ball(session);

    # wait until the simulation ends
    while (TRUE){
      current_duration <- Sys.time() - start_time;
      if (file.exists("simulation_results")){
        break
      }

      if (current_duration > timeout){
        # simulation ends due to a timeout
        write_file("simulation_results", "complete;timeout")
        break
      }

      # check if the timer has ended (long and stupid)
      if (length(Sys.glob(paste0(downloads_dir, "simulation_end*.txt"))) > 0){
        file_name <- get_sim_end_recent();

        start_time <- as.numeric(get_file_contents(name = "simulation_start_time"))
        duration_time <- as.numeric(Sys.time()) - as.numeric(start_time);
        print(duration_time)
        write_file(name = "simulation_results", text = paste0("complete;", duration_time))
        remove_file(file_name)
        break
      }
    }
    Sys.sleep(1)
  }


  utility_fitness_simulation <- function(self, genotype, par){
    # add the first and last height to the genotype
    complete_genotype <- c(par$A_y, genotype, 0);

    # execute the simulation
    run_simulation(par$current_session, complete_genotype, 5);
    # wait for the results
    does_file_exist <- FALSE;
    while (!does_file_exist){
      print(does_file_exist)
      does_file_exist <- file.exists("simulation_results");
    }
    simulation_results <-  get_file_contents("simulation_results")
    duration <- stringr::str_split(simulation_results, stringr::coll(";"))

    if (duration[[1]][2] == "timeout"){
      total_time <- 100;
    } else {
      total_time <- as.numeric(duration[[1]][2]);
    }
    remove_file(name = "simulation_results")
    remove_file(name = "simulation_start_time")
    clear_screen(par$current_session)

    return(total_time)
  }


  # add dependancies for the selection operator
  brach$add_dependents(
    dependents = list(
      fitness_function_single = utility_fitness_simulation,
      fitness_function = utility_fitness_population_2d
    )
  )

  # add the selection operator
  brach$add_operators(
    selection_basic_2d
  )

  add_curve <- function(session, heights){
    session$sendCustomMessage("add_curve", shiny:::toJSON(heights))
  }

  add_ball <- function(session){
    session$sendCustomMessage("add_ball", shiny:::toJSON(c(1)))
  }

  curve_solid <- function(session, heights){
    session$sendCustomMessage("curve_solid", shiny:::toJSON(heights))
  }

  clear_screen <- function(session){
    session$sendCustomMessage("clear_screen", shiny:::toJSON(c(1)))
  }

  change_speed <- function(session, speed){
    session$sendCustomMessage("set_speed", shiny:::toJSON(c(speed)))
  }

  decomp_loc <- system.file("decomp.js", package = "gapackage")
  matter_loc <- system.file("matter.js", package = "gapackage")
  matter_example_loc <- system.file("matter_example.js", package = "gapackage")

  print(decomp_loc)
  print(matter_loc)
  print(matter_example_loc)

  ui <- shiny::fluidPage(
    shiny::includeScript(decomp_loc),
    shiny::includeScript(matter_loc),
    shiny::includeScript(matter_example_loc),
    #shiny::tags$head(shiny::HTML(paste0("<script src='", decomp_loc, "' type='text/javascript'></script>"))),
    #shiny::tags$head(shiny::HTML(paste0("<script src='", matter_loc, "' type='text/javascript'></script>"))),
    #shiny::tags$head(shiny::HTML(paste0("<script src='", matter_example_loc,"' type='text/javascript'></script>"))),
    shiny::HTML("<canvas id = 'matterjs-canvas'></canvas>")
  )

  server <- function(input, output, session) {
    reactive_iteration_timer <- shiny::reactiveTimer(100)

    observe({
      reactive_iteration_timer();
      brach$par <- append(brach$par, list(current_session = session));

      brach$next_iteration();
    })
  }

  shiny::runApp(list(ui = ui, server = server), launch.browser = TRUE)


}
