#update score test to have na.rm = F
score_forecasts_eyc_update <- function(
  forecasts,
  truth,
  return_format = "wide",
  use_median_as_point = FALSE
) {
  
  # forecasts data.frame format
  # columns: model, forecast_date, location, horizon, temporal_resolution,
  #          target_variable, target_end_date, type, quantile, value
  forecasts_colnames <- c(
    "model", "forecast_date", "location", "horizon", "temporal_resolution",
    "target_variable", "target_end_date", "type", "quantile", "value"
  )
  
  # validate forecasts
  # as long as forecasts contains the columns above, it will pass the check
  if (missing(forecasts) || is.null(forecasts)) {
    stop("Forecast dataframe missing", call. = TRUE)
  } else if (!any(is.element(colnames(forecasts), forecasts_colnames))) {
    stop("Forecast dataframe columns malformed", call. = TRUE)
  }
  
  # truth data.frame format
  # columns: model, target_variable, target_end_date, location and value
  truth_colnames <- c(
    "model", "target_variable", "target_end_date", "location", "value"
  )
  
  # validate truth
  # as long as forecasts contains the columns above, it will pass the check
  if (missing(truth) || is.null(truth)) {
    stop("Truth dataframe missing", call. = TRUE)
  } else if (!any(is.element(colnames(truth), truth_colnames))) {
    stop("Truth dataframe columns malformed", call. = TRUE)
  }
  
  # validate return_format
  # match.arg returns error if arg does not match choice
  # which is a bit more complicated to deal with
  if (!is.element(return_format, c("long", "wide"))) {
    return_format <- "wide"
  }
  
  # validate use_median_as_point
  if (is.null(use_median_as_point)) {
    stop("use_median_as_point is NULL and should be one of (TRUE,FALSE)")
  }
  
  # match.arg does not like logical input
  if (!(use_median_as_point %in% c(FALSE,TRUE))) {
    stop("use_median_as_point should be one of (TRUE,FALSE)")
  }
  
  if (length(use_median_as_point) != 1) {
    stop("use_median_as_point should only have a length of 1")
  }
  
  if (use_median_as_point==FALSE && !("point" %in% unique(forecasts$type))){
    stop("Want to use point forecast when scoring but no point forecast in forecast data")
  }
  
  # get dataframe into scoringutil format
  joint_df <- dplyr::left_join(x = forecasts, y = truth,
                               by = c("location", "target_variable", "target_end_date")) %>%
    dplyr::select(-c("model.y")) %>%
    dplyr::rename(model = model.x, prediction = value.x, true_value = value.y) %>%
    dplyr::filter(!is.na(true_value))
  
  # score using scoringutil
  observation_cols <- c(
    "model",
    "location",
    "horizon", "temporal_resolution", "target_variable",
    "forecast_date", "target_end_date"
  )
  
  # creates placeholder variables to store the name of the column from scoringutils::eval_forecasts() to
  # take values from (`abs_var`) and the column name to rename as "abs_error" (`abs_var_rename`)
  if (use_median_as_point) {
    abs_var <- "aem"
    abs_var_rename <- "aem_0"
  } else {
    abs_var <- "ae_point"
    abs_var_rename <- "ae_point_NA"
  }
  
  scores <- tibble::tibble(scoringutils::eval_forecasts(data = joint_df,
                                                        by = observation_cols,
                                                        summarise_by = c(observation_cols, "range"),
                                                        ## the below interval_score_arguments should ensure that WIS is computed correctly
                                                        interval_score_arguments = list(weigh = TRUE, count_median_twice=FALSE))) %>%
    tidyr::pivot_wider(id_cols = observation_cols,
                       names_from = c("range"),
                       values_from = c("coverage", "interval_score", abs_var, "sharpness", "overprediction", "underprediction")) %>%
    purrr::set_names(~sub(abs_var_rename, "abs_error", .x)) %>%
    ## need to remove all columns ending with NA to not affect WIS calculations
    dplyr::select(
      -dplyr::ends_with("_NA")
    ) %>%
    ## before next lines: do we need to check to ensure interval_score columns exist?
    ## the following lines ensure that we use denominator for the wis of
    ## (# of interval_scores)-0.5
    ## which is written in the paper and elsewhere as
    ## (# of interval_scores at level >0 ) + 0.5 or (K + 1/2)
    ## to make sure that the median only gets half the weight of the other
    ## intervals, multiply its value by 0.5
    dplyr::mutate(
      n_interval_scores = rowSums(!is.na(dplyr::select(., dplyr::starts_with("interval_score")))),
      exists_interval_score_0 = "interval_score_0" %in% names(.),
      interval_score_0 = ifelse(exists_interval_score_0, 0.5 * interval_score_0, NA_real_),
      sharpness_0 = ifelse(exists_interval_score_0, 0.5 * sharpness_0, NA_real_),
      underprediction_0 = ifelse(exists_interval_score_0, 0.5 * underprediction_0, NA_real_),
      overprediction_0 = ifelse(exists_interval_score_0, 0.5 * overprediction_0, NA_real_)) %>%
    dplyr::mutate(
      wis = rowSums(dplyr::select(., dplyr::starts_with("interval_score")), na.rm = FALSE)/(n_interval_scores-0.5*(exists_interval_score_0)),
    ) %>%
    dplyr::mutate(
      sharpness = rowSums(dplyr::select(., dplyr::starts_with("sharpness")), na.rm = FALSE)/(n_interval_scores-0.5*(exists_interval_score_0)),
      overprediction = rowSums(dplyr::select(., dplyr::starts_with("overprediction")), na.rm = FALSE)/(n_interval_scores-0.5*(exists_interval_score_0)),
      underprediction = rowSums(dplyr::select(., dplyr::starts_with("underprediction")), na.rm = FALSE)/(n_interval_scores-0.5*(exists_interval_score_0))
    ) %>%
    dplyr::select(
      -dplyr::starts_with("aem_"),
      -dplyr::starts_with("ae_point_"),
      -dplyr::starts_with("interval_score"),
      -dplyr::starts_with("sharpness_"),
      -dplyr::starts_with("underprediction_"),
      -dplyr::starts_with("overprediction_")
    ) 
  
  if ("coverage_0" %in% names(scores)) {
    scores <- scores %>% 
      dplyr::select(-c("coverage_0"))
  }
  
  
  # manipulate return format:
  #   eval_forecasts(), by default, returns in wide format
  #   only change if user specifies long return format
  if (return_format == "long") {
    scores <- scores %>%
      tidyr::pivot_longer(
        cols = !any_of(observation_cols),
        names_to = "score_name",
        values_to = "score_value"
      )
  }
  
  
  scores
}