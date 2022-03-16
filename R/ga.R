#' A class to connstruct, execute and analyse a genetic algorithm.
#'
#' @description
#' This class will store data during the execution of a genetic algorithm. As well as this,
#' it contains a number of methods to interact with the algorithm and UI functionality to
#' view the results and ammend parameters during execution.
#'
#' @details
#' To get started, using the new() method to begin.
#' @export
ga <- R6::R6Class(
  classname = "genetic_algorithm",
  public = list(
    #' @field par a named list containing function parameters
    par = NULL,
    #' @field dim the dimensions of the population array
    dim = NULL,
    #' @field population the population array
    population = NULL,
    #' @field operators an ordered list of the operator functions
    operators = list(),
    #' @field dependents function used by the operators
    dependents = list(),
    #' @field metrics a list containing custom metrics stored during the execution of the simulation
    metrics = list(),
    #' @field output_graphs a list of functions that produce a 2D graph (UI only)
    output_graphs = list(),
    #' @field time the number of operators that have been executed
    time = 0,
    #' @field paused whether the simulation is paused or not (UI only)
    paused = TRUE,
    #' @field speed the number of generations executed every second (UI only)
    speed = 2,
    #' @field store_data boolean which determines the amount of information which is stored
    store_data = FALSE,
    #' @field stored_df data frame used to store metric data
    stored_df = data.frame(),
    #' @field current_generation the number of times that every operators has been executed
    current_generation = 0,
    #' @field shiny_ui the UI code (UI only)
    shiny_ui = shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "gapackage"),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(id = "sidebar_menu",
                                    shinydashboard::menuItem("Parameters", tabName = "tab_parameters", icon = shiny::icon("table")),
                                    shinydashboard::menuItem("Metrics", tabName = "tab_metrics", icon = shiny::icon("table")),
                                    shinydashboard::menuItem("Output", tabName = "tab_output", icon = shiny::icon("table"))
        )
      ),
      shinydashboard::dashboardBody(
        theme = bslib::bs_theme(bootswatch = "darkly"),
        dashboardthemes::shinyDashboardThemes(
          theme = "grey_dark"
        ),
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "tab_parameters",
                                  shiny::fluidRow(shiny::column(12,shiny::uiOutput(outputId = "play_pause_button_ui"))),
                                  shiny::tags$br(),
                                  shiny::uiOutput(outputId = "parameter_list_ui")
          ),
          shinydashboard::tabItem(tabName = "tab_metrics",
                                  shiny::fluidRow(shiny::column(12,shiny::uiOutput(outputId = "download_button_ui"))),
                                  shiny::tags$br(),
                                  shiny::tags$h4(shiny::textOutput(outputId = "current_generation_output_1")),
                                  shiny::uiOutput(outputId = "metric_list_ui")
          ),
          shinydashboard::tabItem(tabName = "tab_output",
                                  shiny::tags$br(),
                                  shiny::tags$h4(shiny::textOutput(outputId = "current_generation_output_2")),
                                  shiny::uiOutput(outputId = "output_list_ui")
          )
        )
      )
    ),
    #' @field shiny_server variable to store the shiny server function
    shiny_server = NULL,
    #' @description
    #' This method allows you to initialize a ga class instance.
    #' @param dim the dimensions of the population array, with the last dimension being the size of the population.
    #' @param initial the function used to generate the initial population.
    #' @param parameters a named list containing parameters used in the operator and dependancy function
    #' @param store_data a boolean value which indicates how much data is stored during the simulation.
    #' @examples
    #' brach <- gapackage::ga$new(
    #' dim = c(10, 1000),
    #' parameters = list(
    #'  initial_min     = 0,
    #'  B_x             = 180,
    #'  initial_max     = 40,
    #'  A_y             = 40,
    #'  geno_length     = 2,
    #'  g               = 9.81,
    #'  remove          = 0.5,
    #'  add_proportion  = 0.5,
    #'  mutation_size   = 0.0000007,
    #'  generations     = 100,
    #'  population_size = 3000,
    #'  maximise        = FALSE,
    #'  location        = 0,
    #'  minkowski_p     = 100
    #'  ),
    #'  store_data = FALSE,
    #'  initial = initial_rand_uni_3d
    #');
    initialize = function(dim, initial, parameters = list(), store_data = FALSE){
      self$dim <- as.vector(dim);
      self$par <- as.list(parameters);

      self$store_data <- store_data;
      self$stored_df <- data.frame(matrix(nrow = 0, ncol = length(self$par) + 3));
      colnames(self$stored_df) <- c("generation", names(self$par), "metric", "value");

      self$population <- initial(self, self$par);
    },
    #' @description
    #' A method to add an operator / operators to the genetic algorithm.
    #' @param operators a single function or a list of functions which have an operator structure.
    #' @examples
    #' brach$add_operators(
    #'   operators = list(
    #'     mutation_cauchy,
    #'     crossover_basic_3d
    #'   )
    #' )
    #'
    #' brach$add_operators(
    #'    mutation_cauchy
    #' )
    add_operators = function(operators){
      if (is.list(operators)){
        for (operator in operators){
          self$add_operator(operator);
        }
      } else {
        self$add_operator(operators);
      }
    },
    #' @description
    #' A method to add single operator to the genetic algorithm
    #' @details
    #' It is recommended that you use the add_operators() function instead.
    #' @param operator a single function which has an operator structure.
    add_operator = function(operator){
      self$operators <- append(self$operators, operator);
    },
    #' @description
    #' A method to add a metric to track during the simulation
    #' @details
    #' This should only be run in either a dependency or an operator function.
    #' @param name the name of the metric you want to track.
    #' @param value the expression corresponding to the value of the metric.
    #' @examples
    #' self$add_metric(name = "average_fitness", value = sum(fitnesses) / length(fitnesses));
    add_metric = function(name, value){
      if (!(name %in% names(self$metrics))){
        append_value <- list();
        append_value[[name]] <- c(value);
        self$metrics <- append(self$metrics, append_value);
      } else {
        self$metrics[[name]] <- append(self$metrics[[name]], value);
      }
    },
    #' @description
    #' A method to add a single dependency or a list of multiple dependencies
    #' @param dependents a single dependency or a list of multiple dependancies
    #' @examples
    #' brach$add_dependents(
    #'   dependents = list(
    #'     fitness_function_single = utility_fitness_brachistochrone_3d,
    #'     fitness_function = utility_fitness_population_3d
    #'   )
    #' )
    add_dependents = function(dependents){
      if (is.list(dependents)){
        for (dependent in dependents){
          self$add_dependent(dependents);
        }
      } else {
        self$add_dependent(dependents);
      }
    },
    #' @description
    #' A method to add a single dependency function.
    #' @param dependent a named list containing one dependent.
    #' @examples
    #' brach$add_dependents(
    #'   dependents = list(
    #'     fitness_function_single = utility_fitness_brachistochrone_3d
    #'   )
    #' )
    add_dependent = function(dependent){
      self$dependents <- append(self$dependents, dependent);
    },
    #' @description
    #' A method to run the next operator in the ordered operators list.
    #' @examples
    #' brach$next_operator()
    next_operator = function(){
      operator_index <- (self$time %% length(self$operators)) + 1;
      self$population <- self$operators[[operator_index]](self, self$population, self$par);
      self$time <- self$time + 1;
    },
    #' @description
    #' A method to run through all the operators once.
    #' @examples
    #' brach$next_iteration()
    next_iteration = function(){
      for (operator in self$operators){
        self$next_operator();
      }
      self$current_generation <- self$current_generation + 1;
      if (self$store_data){
        temp_stored_df <- data.frame(matrix(nrow = 0, ncol = length(self$par) + 3));
        colnames(temp_stored_df) <- c("generation", names(self$par), "metric", "value");

        row_index <- 1;
        for (metric_name in names(self$metrics)){
          metric_data <- self$metrics[[as.character(metric_name)]];
          last_value <- metric_data[length(metric_data)];

          temp_stored_df[row_index, ] <- c(self$current_generation, self$par, metric_name, last_value);
          row_index <- row_index + 1;
        }
        self$stored_df <- rbind(self$stored_df, temp_stored_df);
      }
    },
    #' @description
    #' A method to generate the R Shiny server function (UI Only)
    #' @return
    #' a function intended to be used as an R Shiny server function.
    #' @examples
    #' brach$generate_server()
    generate_server = function(){
      parameter_list <- list();
      for (parameter_index in 1:length(self$par)){
        name <- names(self$par)[parameter_index];
        value <- self$par[[parameter_index]];

        if (is.numeric(value)){
          type_based_element <- shiny::numericInput(
            inputId = paste0("parameter_", parameter_index),
            label = NULL,
            value   = value
          )
        } else if (is.logical(value)){
          type_based_element <- shiny::checkboxInput(
            inputId = paste0("parameter_", parameter_index),
            label = NULL,
            value   = value
          )
        }

        parameter_element <- shiny::fluidRow(
          shiny::column(6,
                        shiny::tags$h4(paste0(name, ": "))
          ),
          shiny::column(6,
                        type_based_element
          )
        );
        parameter_list[[parameter_index]] <- parameter_element;
      }

      return(
        function(input, output, session) {
          output$parameter_list_ui <- shiny::renderUI({
            parameter_list
          })

          shiny::observe({
            if (self$store_data){
              output$download_button_ui <- renderUI({
                shiny::downloadButton(
                  outputId = "download_button", icon = NULL,
                  label = shiny::HTML(paste0("<h4>&nbsp&nbsp&nbspDownload&nbsp&nbsp&nbsp</h4>")),
                  style = paste0("width: 100%; color: #C0C0C0; font-weight: bold; background-color: #899499; border-color: #899499"))
              })
            } else {
              output$download_button_ui <- renderUI({
                shiny::actionButton(
                  inputId = "download_button_blank", icon = NULL,
                  label = shiny::HTML(paste0("<h4>&nbsp&nbsp&nbspDownload&nbsp&nbsp&nbsp</h4>")),
                  style = paste0("width: 100%; opacity: 0.3; color: #C0C0C0; font-weight: bold; background-color: #899499; border-color: #899499"))
              })
            }
          })

          output$download_button <- downloadHandler(
            filename = function() {
              paste("saved_data-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(self$stored_df, file)
            }
          )

          shiny::observeEvent(input$download_button, {
            self$s
          })

          output$metric_list_ui <- shiny::renderUI({
            lapply(1:length(self$metrics), function(i){
              shiny::fluidRow(shiny::plotOutput(paste0("metric_graph_", i), width = "100%"))
            })
          })

          output$output_list_ui <- renderUI({
            lapply(1:length(self$metrics), function(i){
              shiny::fluidRow(shiny::plotOutput(paste0("output_graph_", i), width = "100%"))
            })
          })

          lapply(1:length(self$par), function(i){
            shiny::observeEvent(input[[paste0("parameter_", i)]], {
              self$par[[i]] <- input[[paste0("parameter_", i)]];
            })
          })

          autoInvalidate_1000 <- shiny::reactiveTimer(2000)
          current_generation <- shiny::reactiveValues()

          shiny::observe({
            autoInvalidate_1000()

            if (!self$paused){
              current_generation$value <- self$current_generation;
              for (i in 1:self$speed){self$next_iteration()}
              if (input$sidebar_menu == "tab_metrics"){
                lapply(1:length(self$metrics), function(i){
                  output[[paste0("metric_graph_", i)]] <- shiny::renderPlot({
                    plot(1:length(self$metrics[[i]]), self$metrics[[i]], type = "l",
                         xlab = "Time Index", ylab = names(self$metrics)[i])
                  })
                })
              } else if (input$sidebar_menu == "tab_output"){
                lapply(1:length(self$output_graphs), function(i){
                  output[[paste0("output_graph_", i)]] <- shiny::renderPlot({
                    output_data <- self$output_graphs[[i]][["FUN"]](self, self$population, self$par);

                    plot(output_data$x, output_data$y, type = "l",
                         xlab = self$output_graphs[[i]][["xlab"]], ylab = self$output_graphs[[i]][["ylab"]])
                  })
                })
              }
            }
          })

          output$current_generation_output_1 <- shiny::renderText({
            shiny::HTML(paste0("Current Generation: ", current_generation$value))
          });

          output$current_generation_output_2 <- shiny::renderText({
            shiny::HTML(paste0("Current Generation: ", current_generation$value))
          });

          get_play_pause_button <- function(label, color){
            return(
              shiny::actionButton(
                inputId = "play_pause_button",
                label = shiny::HTML(paste0("<h4>&nbsp&nbsp&nbsp", label, "&nbsp&nbsp&nbsp</h4>")),
                style = paste0("width: 100%; color: white; font-weight: bold; background-color: ", color, " ; border-color: ", color))
            )
          }

          output$play_pause_button_ui <- shiny::renderUI({
            get_play_pause_button(label = "Play", color = "#77dd77")
          })

          shiny::observeEvent(input$play_pause_button, {
            if (self$paused){
              output$play_pause_button_ui <- shiny::renderUI({
                get_play_pause_button(label = "Pause", color = "#ff6961")
              })
            } else {
              output$play_pause_button_ui <- shiny::renderUI({
                get_play_pause_button(label = "Play", color = "#77dd77")
              })
            }
            self$paused <- !self$paused;
          })
        }
      )
    },
    #' @description
    #' a method to add an output graph to the output tab in the UI (UI Only)
    #' @param parameters a named list containing a function and chart output settings
    #' @examples
    #' brach$add_output_graph(
    #'  list(
    #'    xlab = "horizontal distance",
    #'    ylab = "vertical distance",
    #'    FUN = manhattan_plot
    #'  )
    #' )
    add_output_graph = function(parameters){
      self$output_graphs <- append(self$output_graphs, list(parameters));
    },
    #' @description
    #' a method to add multiple output graphs to the output tab in the UI (UI Only)
    #' @param parameters a list containing many named lists which contain multiple function and chart output settings
    #' @examples
    #' brach$add_output_graphs(
    #'  list(
    #'    list(
    #'      xlab = "horizontal distance",
    #'      ylab = "vertical distance",
    #'      FUN = manhattan_plot
    #'    ),
    #'    list(
    #'      xlab = "horizontal distance",
    #'      ylab = "vertical distance",
    #'      FUN = manhattan_plot
    #'    )
    #'  )
    #' )
    add_output_graphs = function(parameters){
      for (output_function in output_functions){
        self$add_output_graph(parameters);
      }
    },
    #' @description
    #' A method to open the UI.
    #' @details
    #' You should only have one localhost open at any particular time.
    run_ui = function(){
      self$next_iteration();
      thematic::thematic_shiny();
      self$shiny_server <- self$generate_server();
      shiny::shinyApp(self$shiny_ui, self$shiny_server)
    },
    #' @description
    #' A method to save the data stored during the simulation
    #' @param path the path of the csv file that will be created.
    save_data = function(path){
      write.csv(self$stored_df, path)
    }
  )
)

