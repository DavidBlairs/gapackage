#' @title get_settings
#'
#' @description This function will allow you to get user input for a variety of settings
#'
#' @param settings a list of named lists containing information about what questions to ask the user
#' @param label a title to begin the questioning.
#'
#' @details This function is used by all the examples to prompt the user for information.
#'
#' @return a named list indicating the chosen values of the parameters.
#' @export
get_settings <- function(settings, label){
  # print the introduction text with labe
  cat(paste0(paste0(rep("=", nchar(label)), collapse = ""), "\n"))
  cat(noquote(paste0(label, "\n")))
  cat(paste0(paste0(rep("=", nchar(label)), collapse = ""), "\n\n"))

  # check whether they want to use the default settings
  default <- readline(cat(noquote("Would you like to use the default settings? [y/n]")));
  cleaned_default <- tolower(trimws(default));

  return_settings <- list();

  # if they dont want to use the default settings
  if (cleaned_default == "n"){
    for (setting in settings){
      answer <- readline(cat(noquote(setting$question)));
      cleaned_answer <- tolower(trimws(answer));

      return_settings[[setting$name]] <- cleaned_answer;
    }
  } else {
    # if they do want to use the default settings
    cat(noquote("Using the default settings...\n"))

    # set the default settings
    for (setting in settings){
      return_settings[[setting$name]] <- setting$default;
    }
  }

  # print out all the settings
  for (setting_name in names(return_settings)){
    cat(noquote(paste0(" - ", setting_name, ": ", return_settings[[setting_name]])), "\n")
  }
  cat("\n\n")

  return(return_settings)
}
