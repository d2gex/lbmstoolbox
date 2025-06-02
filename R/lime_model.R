#' @title LimeLbms class
#'
#' @description
#' it wraps the LIME algorithm providing a common and similar interface to other lbms methods
LimeLbms <- R6::R6Class("LimeLbms", inherit = Lbms, public = list( # nolint
  initialize = function(biological_params, explotation_params, catch_data) {
    super$initialize(biological_params, explotation_params, catch_data)
  },
  prepare_catch_data = function(data) {
    return(self$transpose(data))
  },
  # @formatter:off
  #' @description
  #' Wrapper over LIME's create_lht_list function that already calculate the starting SL50 AND SL95.
  #'
  #' @returns a list with most LHT-related parameters that LIME needs to run
  #' @export
  # @formatter:on
  build_lht_context = function(extra_lht_details = NULL) {
    bio_selc_details <- list(
      linf = self$biological_params$linf,
      vbk = self$biological_params$k,
      t0 = self$biological_params$t0,
      lwa = self$biological_params$lwa,
      lwb = self$biological_params$lwb,
      M50 = self$biological_params$l50,
      M = self$biological_params$M,
      AgeMax = self$biological_params$max_age,
      S50 = self$explotation_params$s50,
      S95 = self$explotation_params$s95,
      SigmaR = self$biological_params$rec_variability_sd,
      SigmaF = self$explotation_params$sigmaF
    )

    # Find the length class between the first length observed and the modal length in the length frequency matryx
    if (is.null(bio_selc_details$S50)) {
      key_lengths <- private$find_sl50_from_matrix(self$catch_data$long)
      bio_selc_details$S50 <- key_lengths$s50
      bio_selc_details$S95 <- key_lengths$s95
    }

    if (is.null(extra_lht_details)) {
      other_details <- list(
        binwidth = 1,
        selex_input = "length",
        selex_type = c("logistic"),
        maturity_input = "length",
        nseasons = 1,
        nfleets = 1
      )
    } else {
      other_details <- extra_lht_details
    }

    lht_details <- c(bio_selc_details, other_details)
    return(do.call(LIME::create_lh_list, lht_details))
  },
  # @formatter:off
  #' @description
  #' Run LIME algorithm
  #'
  #' @returns a list with estimation and simulation results. For the latter, dataframes with
  #' the relative expected catch at length, actual catch, relative fished and un-fished populations are returned.
  #' While catch is relative to lengths, population details are to ages given that LIME is an age-structured algorithm.
  #' All vectors are standarised by dividing them by their total sum.
  # @formatter:on
  run = function(lht_context, extra_run_context = NULL, simul = TRUE) {
    # Prepare matrix as expected by LIME
    lc_matrix_t <- self$prepare_catch_data(self$catch_data$wide)
    years <- as.integer(rownames(lc_matrix_t))

    # Create inputs for LIME run
    data_inputs <- list(years = years, LF = LIME::LFreq_df(lc_matrix_t))
    lfh_inputs <- LIME::create_inputs(lh = lht_context, input_data = data_inputs)

    # Build run alrguments
    run_args <- list(
      input = lfh_inputs,
      SigRprior = c(self$biological_params$rec_variability_mean, self$biological_params$rec_variability_sd)
    )
    if (is.null(extra_run_context)) {
      run_args <- c(run_args, list(
        modpath = NULL,
        data_avail = "LC",
        derive_quants = TRUE
      ))
    }

    # is it dome-shape? --> we need to fix the curve.
    if (lht_context$selex_type == "dome") {
      run_args$vals_selex_ft <- lht_context$S_fl
    }

    # Run LIME
    result <- do.call(LIME::run_LIME, run_args)
    if (!is.atomic(result$Report)) {
      evaluation <- list()
      simulation <- list()
      # Fetch results
      estimates <- data.frame(
        years = years,
        SL50 = result$Report$S50_f,
        SL95 = result$Report$S95_f,
        SPR = result$Report$SPR_t,
        F = result$Report$F_t,
        R = result$Report$R,
        SSB = result$Report$SB_t
      )

      # calculate confidence intervals
      ci_df <- private$get_confidence_intervals(result)
      estimates <- estimates %>%
        dplyr::full_join(ci_df, by = "years")

      if (run_args$derive_quants) {
        estimates$Fmsy <- result$Report$F_y
        estimates$F30 <- result$Derived$F30
        estimates$F40 <- result$Derived$F40
        estimates$FF30 <- result$Derived$FF30
        estimates$FF40 <- result$Derived$FF40
      }
      if (!is.atomic(result$Sdreport)) {
        estimates$hessian <- result$Sdreport$pdHess
        estimates$convergence <- result$opt$max_gradient <= 0.001 & result$Sdreport$pdHess == TRUE
        estimates$max_gradient <- result$opt$max_gradient
      } else {
        estimates$hessian <- NA
        estimates$convergence <- NA
        estimates$max_gradient <- NA
      }
    } else {
      estimates <- NULL
      logger::log_error("---> LIME could not produce any result")
    }
    if (is.null(estimates)) {
      return(NULL)
    }
    if (simul == TRUE) {
      model_fitting_builder <- LimeSimulation$new(
        self$catch_data,
        result$Inputs$Data$lbhighs,
        result$Inputs$Data$match_ages,
        result$Report$plb,
        result$Report$plb0,
        result$Report$N_ta,
        result$Report$N_ta0
      )
      model_info <- model_fitting_builder$run()
    } else {
      model_info <- NULL
    }

    raw_details <- list(inputs = result$Inputs, report = result$Report, sd_report = result$Sdreport)
    output <- list(
      evaluation = list(estimates = estimates),
      simulation = list(
        model_info = model_info,
        raw_details = raw_details
      )
    )
    return(output)
  }
), private = list(
  get_confidence_intervals = function(data) {
    report <- data$Sdreport
    variables <- c(
      SPR_t = FALSE,
      lR_t = TRUE,
      lF_y = TRUE
    )

    ci_interval_df <- data.frame(
      years = data$input$years
    )
    num_years <- nrow(ci_interval_df)
    for (var_name in names(variables)) {
      is_log <- variables[var_name]
      sd <- summary(report)[which(rownames(summary(report)) == var_name), ]
      sd[, 2][which(is.na(sd[, 2]))] <- 0
      sd <- sd[seq(1, by = 1, length.out = num_years), ]
      ci_data <- LIME::read_sdreport(sd, log = is_log)
      if (var_name == "SPR_t") {
        suffix <- "spr"
      } else if (var_name == "lR_t") {
        suffix <- "rec"
      } else {
        suffix <- "fishing"
      }
      ci_interval_df <- ci_interval_df %>%
        dplyr::mutate(
          !!paste0("lower_", suffix) := ci_data[1:num_years],
          !!paste0("upper_", suffix) := ci_data[(num_years + 1):length(ci_data)]
        )
    }
    return(ci_interval_df)
  }
))
