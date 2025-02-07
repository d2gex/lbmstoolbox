#' @title LbsprSimulation class
#'
#' @description
#' it generates two dataframes containing information about the estimated vs real catch as well as
#' the estimated unfished vs fished population. It does not apply any depletion threshold for the unfished population.
LbsprSimulation <- R6::R6Class("LbsprSimulation", public = list( # nolint
  # @formatter:off
  #' @field biology_params list with the biological parameters of the analysed species
  biology_params = NULL,
  #' @field estimates dataframe with estimates from LBSPR model
  estimates = NULL,
  #' @field catch_data list of long and wide dataframes
  catch_data = NULL,
  #' @description
  #' Initialise the class
  #'
  #' @param biology_params list with the biological parameters of the analysed species
  #' @param estimates dataframe with estimates from LBSPR model
  #' @param catch_data list of long and wide dataframes
  #' @export
  # @formatter:on
  initialize = function(biology_params, estimates, catch_data) {
    self$biology_params <- biology_params
    self$estimates <- estimates
    self$catch_data <- catch_data
  },
  # @formatter:off
  #' @description
  #' Generates the model fitting information (relative catch at length and relative fished and unfished population
  #' at length)
  #'
  #' @returns A list of dataframes with the expected catch and fished/unfished population
  #' @export
  # @formatter:on
  run = function(verbose = TRUE) {
    results <- list()
    # Fetch the raw details thrown by LBSPR
    for (r_offset in seq_len(nrow(self$estimates))) {
      year <- self$estimates$years[r_offset]
      lengths <- unique(catch_data$long$MeanLength)
      bin_min <- floor(min(lengths))
      bin_max <- ceiling(max(lengths))
      estimated_params <- self$biology_params
      estimated_params@SL50 <- self$estimates$SL50[r_offset]
      estimated_params@SL95 <- self$estimates$SL95[r_offset]
      estimated_params@SPR <- self$estimates$SPR[r_offset]
      estimated_params@BinMin <- bin_min
      estimated_params@BinMax <- bin_max
      year_result <- LBSPR::LBSPRsim(estimated_params, verbose = verbose)
      results[["raw_details"]][[as.character(year)]] <- year_result
    }

    # Amalgamate details into a single dataframe
    results[["model_info"]] <- private$result_formatter(results$raw_details)
    return(results)
  }
), private = list(
  result_formatter = function(sim_data) {
    return(
      purrr::reduce(lapply(names(sim_data), function(year) {
        year_ <- as.numeric(year)

        # Get the real catch for actual year
        real_catch <- self$catch_data$long %>%
          dplyr::filter(year == year_)

        # crop simulation data to sought LMids (real catch)
        pop_df <- as.data.frame(sim_data[[year]]@pLPop)
        pop_df <- pop_df %>%
          dplyr::filter((LMids >= min(real_catch$MeanLength)) & (LMids <= max(real_catch$MeanLength)))

        # Standarised the new potential extracted segement of lengths
        pop_df <- pop_df %>% dplyr::mutate(dplyr::across(!LMids, function(x) {
          x / sum(x)
        }))

        # Build dataframe with simulation details
        # VulnUF    VulnF
        df <- data.frame(
          year = year_,
          lengths = pop_df$LMids,
          unfished_pop = pop_df$PopUF,
          fished_pop = pop_df$PopF,
          unfished_vul = pop_df$VulnUF,
          exp_catch = sim_data[[year]]@pLCatch[, 1],
          real_catch = real_catch$catch
        )

        # Make real catch into the same relative scale as simulation data
        df <- df %>% dplyr::mutate(
          relative_catch = real_catch / sum(real_catch)
        )
      }), dplyr::bind_rows)
    )
  }
))
