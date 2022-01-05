library(shiny)

## app.R ##
library(shiny)
library(shinydashboard)

server <- function(input, output) { }

ui <- fluidPage()

shinyApp(ui, server)
observe({
  timer_play_event$trigger;
  if (self$delay != 0){
    invalidateLater(self$delay * 1000, session)
    self$next_iteration();

    if (input$sidebar_menu == "tab_metrics"){
      metric_graphs <- list();
      for (metric_index in 1:length(self$metrics)){
        new_metric_index <- as.numeric(metric_index);
        output_id <- paste0("metric_graph_", new_metric_index);

        metric_graphs[[new_metric_index]] <- shiny::plotOutput(output_id);
        metric_data <- self$metrics[[new_metric_index]];

        output[[output_id]] <- shiny::renderPlot({
          plot(1:length(metric_data), metric_data, type = "l",
               xlab = "Time Index", ylab = names(self$metrics)[new_metric_index])
        })
      }
      output$metric_list_ui <- renderUI(metric_graphs)
    } else if (input$sidebar_menu == "tab_output"){
      output_graphs <- list();
      for (output_index in 1:length(self$output_graphs)){
        output_id <- paste0("output_graph_", output_index);

        output_graphs[[output_index]] <- shiny::plotOutput(output_id);
        output_data <- self$output_graphs[[output_index]][["FUN"]](self, self$population, self$par);

        output[[output_id]] <- shiny::renderPlot({
          plot(output_data$x, output_data$y, type = "l",
               xlab = "Time Index", ylab = self$output_graphs[[output_index]][["ylab"]])
        })
      }
      output$output_list_ui <- renderUI(output_graphs)
    }
  }
})
