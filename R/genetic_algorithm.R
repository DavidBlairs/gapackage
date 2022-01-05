ga <- R6::R6Class(
  classname = "genetic_algorithm",
  public = list(
    par = NULL,
    dim = NULL,
    population = NULL,
    operators = list(),
    dependents = list(),
    metrics = list(),
    output_graphs = list(),
    time = 0,
    delay = 0,
    shiny_ui = shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "gapackage"),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(id = "sidebar_menu",
            shinydashboard::menuItem("Parameters", tabName = "tab_parameters", icon = icon("table")),
            shinydashboard::menuItem("Metrics", tabName = "tab_metrics", icon = icon("table")),
            shinydashboard::menuItem("Output", tabName = "tab_output", icon = icon("table"))
        )
      ),
      shinydashboard::dashboardBody(
        theme = bslib::bs_theme(bootswatch = "darkly"),
        dashboardthemes::shinyDashboardThemes(
          theme = "grey_dark"
        ),
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "tab_parameters",
             uiOutput(outputId = "parameter_list_ui")
          ),
          shinydashboard::tabItem(tabName = "tab_metrics",
            shiny::actionButton(inputId = "button_play", label = "Play"),
            shiny::actionButton(inputId = "button_pause", label = "Pause"),
            uiOutput(outputId = "metric_list_ui")
          ),
          shinydashboard::tabItem(tabName = "tab_output",
            uiOutput(outputId = "output_list_ui")
          )
        )
      )
    ),
    shiny_server = NULL,
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

          output$metric_list_ui <- shiny::renderUI({
            lapply(1:length(self$metrics), function(i){
              fluidRow(shiny::plotOutput(paste0("metric_graph_", i)))
            })
          })

          output$output_list_ui <- renderUI({
            lapply(1:length(self$metrics), function(i){
              fluidRow(shiny::plotOutput(paste0("output_graph_", i)))
            })
          })

          lapply(1:length(self$par), function(i){
            observeEvent(input[[paste0("parameter_", i)]], {
              self$par[[i]] <- input[[paste0("parameter_", i)]];
            })
          })

          autoInvalidate <- reactiveTimer(1000)

          observe({
            autoInvalidate()
            if (self$delay != 0){
              self$next_iteration()

              if (input$sidebar_menu == "tab_metrics"){
                lapply(1:length(self$metrics), function(i){
                  output[[paste0("metric_graph_", i)]] <- renderPlot({
                    plot(1:length(self$metrics[[i]]), self$metrics[[i]], type = "l",
                         xlab = "Time Index", ylab = names(self$metrics)[i])
                  })
                })
              } else if (input$sidebar_menu == "tab_output"){
                lapply(1:length(self$output_graphs), function(i){
                  output[[paste0("output_graph_", i)]] <- renderPlot({
                    output_data <- self$output_graphs[[i]][["FUN"]](self, self$population, self$par);

                    plot(output_data$x, output_data$y, type = "l",
                         xlab = self$output_graphs[[i]][["xlab"]], ylab = self$output_graphs[[i]][["ylab"]])
                  })
                })
              }
            }
          })

          observeEvent(input$button_play, {
            self$delay <- 0.2;
          })
          observeEvent(input$button_pause, {
            self$delay <- 0;
          })
        }
      )
    },
    add_output_graph = function(parameters){
      self$output_graphs <- append(self$output_graphs, list(parameters));
    },
    add_output_graphs = function(parameters){
      for (output_function in output_functions){
        self$add_output_graph(parameters);
      }
    },
    run_ui = function(){
      self$next_iteration();
      thematic::thematic_shiny();
      self$shiny_server <- self$generate_server();
      shiny::shinyApp(self$shiny_ui, self$shiny_server)
    }
  )
)
