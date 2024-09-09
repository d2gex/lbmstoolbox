#' @title LimeModelFittingBuilder class
#'
#' @description
#' it generates two dataframes containing information about the estimated vs real catch as well as
#' the estimated unfished vs fished population. It does not apply any depletion threshold for the unfished population.
LimeModelFittingBuilder <- R6::R6Class("LimeModelFittingBuilder", public = list( # nolint
  # @formatter:off
  #' @field catch_data list of long and wide dataframes
  catch_data = NULL,
  #' @field mid_points vector of modelled lengths by LIME
  mid_points = NULL,
  #' @field ages integer vector
  ages = NULL,
  #' @field estimated_catch estimated catch three-dimensional matrix
  estimated_catch = NULL, # plb
  #' @field fished_population estimated fished population matrix
  fished_population = NULL, # N_ta
  #' @field fished_population estimated fished population matrix
  #' @field unfished_population estimated unfished population matrix
  unfished_population = NULL, # Nta0
  #' @description
  #' Initialise the class
  #'
  #' @param catch_data list of long and wide dataframes
  #' @param mid_points vector of modelled lengths by LIME
  #' @param estimated_catch estimated catch three-dimensional matrix
  #' @param fished_population estimated fished population matrix
  #' @param fished_population estimated fished population matrix
  #' @export
  # @formatter:on
  initialize = function(catch_data, mid_points, ages, estimated_catch, fished_population, unfished_population) {
    self$catch_data <- catch_data
    self$mid_points <- mid_points
    self$ages <- ages
    self$estimated_catch <- estimated_catch
    self$fished_population <- fished_population
    self$unfished_population <- unfished_population
  },
  # @formatter:off
  #' @description
  #' Generates the model fitting information (relative catch at length and relative fished and unfished population
  #' at age)
  #'
  #' @returns A list of dataframes with the expected catch and fished/unfished population
  #' @export
  # @formatter:on
  generate_model_info = function() {
    return(list(
      catch = self$build_catch_df(),
      population = self$build_population_df()
    ))
  },
  # @formatter:off
  #' @description
  #' Builds a catch dataframe with the relative and expected catch. Both data are standarised
  #' (divided by their total sum)
  #'
  #' @returns a dataframe with the real standarised relative real and standarised expected catch
  #' @export
  # @formatter:on
  build_catch_df = function() {
    real_catch <- private$build_catch_dataframe()
    exp_catch <- private$build_expected_catch_dataframe()
    exp_catch %>% assertr::verify(nrow(.) == nrow(real_catch))
    return(
      purrr::reduce(list(real_catch, exp_catch), dplyr::full_join, by = c("year", "lengths"))
    )
  },
  # @formatter:off
  #' @description
  #' Builds a population dataframe with expected unfished and fished population. Both data are standarised
  #' (divided by their total sum). The expected values are relative to age rather than length.
  #'
  #' @returns a dataframe with the unfished and fished population
  #' @export
  # @formatter:on
  build_population_df = function() {
    populations <- list(self$fished_population, self$unfished_population)
    pop_col_names <- c("unfished_pop", "fished_pop")
    population_df <-
      purrr::reduce(lapply(seq_along(populations), function(offset) {
        # Get relevant details for each population and convert it to a dataframe
        col_name <- pop_col_names[offset]
        df <- as.data.frame(populations[[offset]])
        names(df) <- self$ages
        df$year <- unique(self$catch_data$long$year)

        # Build a long dataframe with relative population for each population type
        df <- df %>%
          tidyr::pivot_longer(!year, names_to = "age", values_to = col_name) %>%
          dplyr::mutate(age = as.numeric(age)) %>%
          dplyr::mutate(!!col_name := get(col_name) / sum(get(col_name))) # make it relative
      }), dplyr::full_join, by = c("year", "age"))
    return(population_df)
  }
), private = list(
  build_catch_dataframe = function() {
    catch_df <- self$catch_data$long %>%
      dplyr::group_by(year, MeanLength) %>%
      dplyr::summarise(real_catch = sum(catch), .groups = "drop") %>%
      dplyr::mutate(relative_catch = real_catch / sum(real_catch)) %>%
      dplyr::rename(c("lengths" = "MeanLength"))
    return(catch_df)
  },
  build_expected_catch_dataframe = function() {
    # (1) Reconstruct wide dataframe with year and midpoints
    exp_catch_df <- as.data.frame(self$estimated_catch)
    names(exp_catch_df) <- self$mid_points
    exp_catch_df$year <- unique(self$catch_data$long$year)

    # (2) Turn it into a long one with relative estimated catch and cropped mid points to match the real catch
    real_mid_points <- unique(self$catch_data$long$MeanLength)
    min_mid_point <- min(real_mid_points)
    max_mid_point <- max(real_mid_points)

    exp_catch_df <- exp_catch_df %>%
      tidyr::pivot_longer(!year, names_to = "lengths", values_to = "exp_catch") %>%
      dplyr::mutate(lengths = as.numeric(lengths)) %>%
      dplyr::mutate(exp_catch = exp_catch / sum(exp_catch)) %>% # make it relative
      dplyr::filter((lengths >= min_mid_point) & (lengths <= max_mid_point)) # crop

    return(exp_catch_df)
  }
))
