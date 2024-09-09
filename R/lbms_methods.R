#' @title Lbms class
#'
#' @description
#' Base class providing common interface and parammeters for all algorithms
Lbms <- R6::R6Class("Lbms", public = list( # nolint
  # @formatter:off
  #' @field biological_params biological parameters list
  biological_params = NULL,
  #' @field explotation_params exploitation parameters list
  explotation_params = NULL,
  #' @field catch_data list with a long and wide catch dataframe
  catch_data = NULL,
  #' @description
  #' Initialise the class
  #'
  #' @param biological_params biological parameters list
  #' @param explotation_params exploitation parameters list
  #' @param icatch_data list with a long and wide catch dataframe
  #' @export
  # @formatter:on
  initialize = function(biological_params = NULL, explotation_params = NULL, catch_data = NULL) {
    self$biological_params <- biological_params
    self$explotation_params <- explotation_params
    self$catch_data <- catch_data
  },
  # @formatter:off
  #' @description
  #' It prepares the length frequency data for the algorithm digestion
  #'
  #' @param data a wide dataframe with lengths as rows and timestep as columns
  # @formatter:on
  prepare_catch_data = function(data) { },
  transpose = function(data, row_as_numeric = TRUE, as_matrix = TRUE) {
    if (row_as_numeric) {
      col_names <- unlist(lapply(names(data), function(x) {
        gsub("[^0-9]", "", x)
      }))
      names(data) <- col_names
    }
    data_t <- t(data)
    # Get first row as column names
    colnames(data_t) <- data_t[1, ]
    data_t <- data_t[2:nrow(data_t), ]

    if (as_matrix) {
      return(data_t)
    }
    return(as.data.frame(data_t))
  }
))
