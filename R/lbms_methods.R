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
  #' @description
  #' It prepares the length frequency data for the algorithm digestion
  #'
  #' @param data a wide dataframe with lengths as rows and timestep as columns
  prepare_catch_data = function(data) { }
))
