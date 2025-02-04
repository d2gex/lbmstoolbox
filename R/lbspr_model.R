#' @title LbsprLbms class
#'
#' @description
#' it wraps the LIME algorithm providing a common and similar interface to other lbms methods
LbsprLbms <- R6::R6Class("LbsprLbms", inherit = Lbms, public = list( # nolint
  # @formatter:off
  #' @description
  #' Initialise class.
  #'
  #' @param biological_params biological parameters list
  #' @param catch_data list with a long and wide catch dataframe
  #' @export
  # @formatter:on
  initialize = function(biological_params, catch_data) {
    super$initialize(biological_params = biological_params, explotation_params = NULL, catch_data = catch_data)
  },
  prepare_catch_data = function(data) {
    data <- self$df_to_unname_matrix(data, from_col = 2)
    return(data)
  },
  build_lht_context = function() {
    bio_details <- new("LB_pars")
    bio_details@Linf <- self$biological_params$linf
    bio_details@L50 <- self$biological_params$l50
    bio_details@L95 <- self$biological_params$l95
    bio_details@MK <- self$biological_params$mk
    bio_details@Walpha <- self$biological_params$lwa
    bio_details@Wbeta <- self$biological_params$lwb
    bio_details@FecB <- self$biological_params$fecb
    return(bio_details)
  },

  # @formatter:off
  #' @description
  #' Run LBSPR algorithm
  #'
  #' @returns a list with estimation and simulation results. For the latter, dataframes with
  #' the relative expected catch at length, actual catch, relative fished and un-fished populations are returned.
  #' All vectors are standarised by dividing them by their total sum.
  #' @export
  # @formatter:on
  run = function(units = "cms", bindwidth = 1, verbose = TRUE, simul = TRUE) {
    # Prepare matrix as expected by LIME
    lc_matrix_t <- self$transpose(self$catch_data$wide)
    years <- as.integer(rownames(lc_matrix_t))
    mid_points <- as.numeric(colnames(lc_matrix_t))
    lc_matrix <- self$prepare_catch_data(self$catch_data$wide)

    bio_details <- self$build_lht_context()
    bio_details@L_units <- units
    bio_details@BinWidth <- bindwidth


    # --> Provide algorithms with expected details about length and weight compsositions
    catch_details <- new("LB_lengths")
    catch_details@LMids <- mid_points
    catch_details@LData <- lc_matrix
    catch_details@Years <- years
    catch_details@NYears <- length(years)

    # Run evaluation to fetch estimates
    evaluation_results <- LBSPR::LBSPRfit(LB_pars = bio_details, LB_lengths = catch_details, verbose = verbose)
    output <- list()

    estimates_boundaries <- private$get_confidence_intervals(evaluation_results)
    output$evaluation$estimates <- as.data.frame(evaluation_results@Ests)
    output$evaluation$estimates$years <- evaluation_results@Years

    output$evaluation$estimates$lower_spr <- estimates_boundaries$SPR$lower
    output$evaluation$estimates$upper_spr <- estimates_boundaries$SPR$upper
    output$evaluation$estimates$lower_fm <- estimates_boundaries$FM$lower
    output$evaluation$estimates$upper_fm <- estimates_boundaries$FM$upper

    if (isTRUE(simul)) {
      # Run simulation to fetch fished/unfished population and catch
      iim_sim_lbspr <- LbsprSimulation$new(
        bio_details,
        output$evaluation$estimates,
        self$catch_data
      )
      simulation_results <- iim_sim_lbspr$run(verbose = TRUE)
      output$simulation <- simulation_results
    } else {
      output$simulation <- NULL
    }
    return(output)
  }
), private = list(
  get_confidence_intervals = function(data, access_details) {
    metric_access_details <- c(
      SPR = 4,
      FM = 3
    )
    confidence_intervals <- list()
    for (variable in names(metric_access_details)) {
      offset <- metric_access_details[variable]

      ci_lower <- data@Ests[, variable] - 1.96 * sqrt(data@Vars[, offset])
      ci_upper <- data@Ests[, variable] + 1.96 * sqrt(data@Vars[, offset])
      ci_lower[ci_lower < 0] <- 0
      if (variable == "SPR") {
        # correct bounded parameters - dodgy I know!
        ci_upper[ci_upper > 1] <- 1
      }
      confidence_intervals[[variable]][["lower"]] <- ci_lower
      confidence_intervals[[variable]][["upper"]] <- ci_upper
    }
    return(confidence_intervals)
  }
))
