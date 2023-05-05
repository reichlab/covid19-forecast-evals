#Functions for use in evaluation report (query hospitalization,Pairwise comparison, Filter,  Plotting functions, Tables (inactive))

#QUERY HOSPITALIZATION
# mutate_scores: function to clean the datasets and add in columns to count the number of weeks, horizons, and locations
# align_forecasts_one_temporal_resolution: Align forecasts one temporal resolution

# function to clean the datasets and add in columns to count the number of weeks, horizons, and locations
mutate_scores <- function(x) {
  x %>%
    group_by(model, location, horizon, score_name) %>% #Add count of weeks
    mutate(n_weeks = n(),
           n_weeks_3wksPrior = sum(target_end_date >= (last_eval_sat - 2*7) & horizon == "1"),
           n_weeks_10wksPrior =sum(target_end_date >= first_eval_sat & horizon == "1")) %>%
    ungroup() %>%
    group_by(model, location, target_end_date, score_name) %>% #Add count of horizons
    mutate(n_horizons = n()) %>%
    ungroup() %>%
    group_by(model, horizon,  target_end_date, score_name) %>% #Add count of locations
    mutate(n_locations = n()) %>%
    ungroup()  %>%
    mutate(submission_sat = as.Date(calc_target_week_end_date(forecast_date, horizon=0))) 
}

#' @return forecast dataframe augmented by columns reference_date and
#' relative_horizon
align_forecasts_one_temporal_resolution <- function(
    forecasts,
    reference_dates,
    reference_weekday,
    reference_windows,
    drop_nonpos_relative_horizons
) {
  if (length(unique(forecasts$temporal_resolution)) > 1) {
    stop("standardize_forecasts_one_temporal_resolution only supports forecasts at a single temporal resolution.")
  }
  
  if (is.null(reference_windows)) {
    if (reference_weekday == "Saturday") {
      reference_windows <- -4:2
    } else if (reference_weekday == "Monday") {
      reference_windows <- -6:0
    } else {
      stop("Reference windows undefined")
    }
  }
  
  if (!is.list(reference_windows)) {
    reference_windows <- list(reference_windows)
  }
  
  if (!is.null(reference_dates)) {
    # ensure we have dates
    reference_dates <- as.Date(reference_dates)
  } else {
    # every date from that of first forecast - diameter of first window
    # to that of last forecast + diameter of last window
    all_dates <- seq(
      min(forecasts$forecast_date) - (
        max(sort(reference_windows[[1]])) -
          min(sort(reference_windows[[1]]))
      ),
      max(forecasts$forecast_date) + (
        max(sort(reference_windows[[length(reference_windows)]])) -
          min(sort(reference_windows[[length(reference_windows)]])) 
      ),
      by = 1
    )
    
    # keep the dates identified above that are the specified reference_weekday
    reference_dates <- all_dates[weekdays(all_dates) == reference_weekday]
  }
  
  # create a tibble where each row contains:
  # - a possible forecast date
  # - a reference date to which that forecast date should be assigned
  ref_df <- tibble(
    reference_date = reference_dates,
    forecast_date = purrr::map2(
      reference_date, 
      reference_windows, 
      ~.x+.y
    )
  ) %>% unnest(cols = forecast_date)
  
  # ensure that in the tibble constructed above, each forecast date is
  # associated with at most one reference date
  # this could be violated if some windows are overlapping
  reps <- ref_df %>%
    dplyr::group_by(forecast_date) %>%
    dplyr::tally() %>% 
    dplyr::filter(n > 1)
  if (nrow(reps) > 0) {
    stop(paste0(
      "The following forecast dates are associated with multiple reference dates: ",
      paste(reps %>% dplyr::pull(forecast_date), collapse = ", ")
    ))
  }
  
  # join with the reference date lookup table above
  # and calculate the relative horizon
  forecasts <- forecasts %>% 
    dplyr::left_join(ref_df, by = "forecast_date") %>% 
    dplyr::mutate(
      ts_days = ifelse(temporal_resolution == "wk", 7, 1),
      relative_horizon = 
        ceiling(as.numeric((target_end_date - reference_date) / ts_days))
    ) %>%
    dplyr::select(-ts_days)
  
  if (drop_nonpos_relative_horizons) {
    forecasts <- forecasts %>%
      dplyr::filter(relative_horizon > 0)
  }
  
  return(forecasts)
}

#PAIRWISE COMPARIZON FUNCTION
# pairabs_comparison: pairwise comparison function FOR MAE
# pairwise comparison function FOR MAE
pairabs_comparison <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                               permutation_test = FALSE){
  # subsets of available scores for both models:
  subx <- subset(scores, model == mx)
  suby <- subset(scores, model == my)
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("timezero", "location", "horizon"),
               all.x = FALSE, all.y = FALSE)
  ##### catch common problems:
  ##### no overlap between targets covered by x and y:
  if(nrow(sub) == 0){
    warning("No overlap of covered forecast targets for ", mx, "and", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  ##### unavailable scores (likely because a model issues only point forecasts?)
  if(any(is.na(subx$abs_error))){
    warning("Some or all wis values are NA for ", mx, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  if(any(is.na(suby$abs_error))){
    warning("Some or all wis values are NA for ", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  
  # compute ratio:
  
  # matrices to store:
  results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                              nrow = length(models),
                                                              dimnames = list(models, models))
  
  ratio <- sum(sub$abs_error.x) / sum(sub$abs_error.y)
  # perform permutation tests:
  if(permutation_test){
    pval <- permutationTest(sub$abs_error.x, sub$abs_error.y,
                            nPermutation = 999)$pVal.permut
    ##### aggregate by forecast date:
    sub_fcd <- aggregate(cbind(abs_error.x, abs_error.y) ~ timezero, data = sub, FUN = mean)
    # catch error if too many observations
    if(nrow(sub_fcd) > 5){
      pval_fcd <- permutationTest(sub_fcd$abs_error.x, sub_fcd$abs_error.y,
                                  nPermutation = 999)$pVal.permut
    }else{
      warning("Too few observations to compute p-value for ", mx, " and ", my, " with aggregation by forecast date. Returning NA.")
      pval_fcd <- NA
    }
  }else{
    pval <- NULL
    pval_fcd <- NULL
  }
  return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
}


#pairwise comparison function FOR WIS
pairwise_comparison <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                permutation_test = FALSE){
  # subsets of available scores for both models:
  subx <- subset(scores, model == mx)
  suby <- subset(scores, model == my)
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("timezero", "location", "horizon"),
               all.x = FALSE, all.y = FALSE)
  ##### catch common problems:
  ##### no overlap between targets covered by x and y:
  if(nrow(sub) == 0){
    warning("No overlap of covered forecast targets for ", mx, "and", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  ##### unavailable scores (likely because a model issues only point forecasts?)
  if(any(is.na(subx$wis))){
    warning("Some or all wis values are NA for ", mx, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  if(any(is.na(suby$wis))){
    warning("Some or all wis values are NA for ", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  
  # compute ratio:
  
  # matrices to store:
  results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                              nrow = length(models),
                                                              dimnames = list(models, models))
  
  ratio <- sum(sub$wis.x) / sum(sub$wis.y)
  # perform permutation tests:
  if(permutation_test){
    pval <- permutationTest(sub$wis.x, sub$wis.y,
                            nPermutation = 999)$pVal.permut
    ##### aggregate by forecast date:
    sub_fcd <- aggregate(cbind(wis.x, wis.y) ~ timezero, data = sub, FUN = mean)
    # catch error if too many observations
    if(nrow(sub_fcd) > 5){
      pval_fcd <- permutationTest(sub_fcd$wis.x, sub_fcd$wis.y,
                                  nPermutation = 999)$pVal.permut
    }else{
      warning("Too few observations to compute p-value for ", mx, " and ", my, " with aggregation by forecast date. Returning NA.")
      pval_fcd <- NA
    }
  }else{
    pval <- NULL
    pval_fcd <- NULL
  }
  return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
}

#FILTER FUNCTIONS
# by_week_function: Filter WIS by week 
# historical_accuracy_filter: filter for inclusion in historical accuracy 
# historical_coverage_filter: filter for inclusion in historical coverage table 
# recent_accuracy_filter: filter for inclusion in recent accuracy table 
# recent_coverage_filter: filter for inclusion in recent coverage table 

#Filter WIS by week 
#Filter WIS by week 
by_week_function <- function(df, var, loc, nloc) {
  df %>%
    filter(score_name == var & location %in% loc) %>%
    # Calculate the number of US locations forecasted for each combination of
    # model, target_end_date, horizon
    group_by(model, target_end_date, horizon) %>%
    mutate(n_US_location = n()) %>%
    # Keep only those models that forecasted probabilistic forecasts for all 50 states
    ungroup() %>%
    group_by(target_end_date, horizon) %>%
    filter(n_US_location==nloc) %>%
    # Calculate the mean score for each combination of model, target_end_date, horizon
    group_by(model,horizon, target_end_date) %>%
    summarise(mean_score = mean(score_value))
}

##Filter for inclusion in historical accuracy 
historical_accuracy_filter <- function(x) {
  x %>%
    group_by(model, score_name) %>% 
    mutate(n_forecasts_wis = sum(score_name == "wis" & !is.na(score_value)),
           n_forecasts_mae = sum(score_name == "abs_error" & !is.na(score_value))) %>% ungroup() %>%
    filter(n_forecasts_wis >= (max(n_forecasts_wis)*0.5) | n_forecasts_mae >= (max(n_forecasts_mae)*0.5)) %>% 
    filter(!is.na(score_value)) %>% droplevels()
}

##Filter for inclusion in historical coverage 
historical_coverage_filter <- function(x) {
  x %>%
    filter(!is.na(score_value)) %>%  #remove NAs 
    group_by(model, score_name) %>%
    mutate(n_forecasts_50 = sum(score_name == "coverage_50" & !is.na(score_value)),
           n_forecasts_95 = sum(score_name == "coverage_95" & !is.na(score_value))) %>% ungroup() %>%
    filter(n_forecasts_50 >= (max(n_forecasts_50)*0.5) | n_forecasts_95 >= (max(n_forecasts_95)*0.5)) %>% 
    droplevels()
}

#filter for inclusion in recent accuracy table 
##Keep only models that have submitted forecasts for at least half of the number of max WIS forecasts or half the max MAE forecasts 
recent_accuracy_filter <- function(x,y) {
  x %>%
    filter(!is.na(score_value)) %>%  #remove NAs 
    filter(target_end_date >= y) %>% #
    group_by(model, score_name) %>%
    mutate(n_forecasts_wis = sum(score_name == "wis" & !is.na(score_value)),
           n_forecasts_mae = sum(score_name == "abs_error" & !is.na(score_value))) %>% ungroup() %>%
    filter(n_forecasts_wis >= (max(n_forecasts_wis)*0.5) | n_forecasts_mae >= (max(n_forecasts_mae)*0.5)) %>% 
    droplevels()
}

#filter for inclusion in recent coverage table 
recent_coverage_filter <- function(x,y) {
  x %>%
    filter(!is.na(score_value)) %>%  #remove NAs 
    filter(target_end_date >= y) %>% #
    group_by(model, score_name) %>%
    mutate(n_forecasts_50 = sum(score_name == "coverage_50" & !is.na(score_value)),
           n_forecasts_95 = sum(score_name == "coverage_95" & !is.na(score_value))) %>% ungroup() %>%
    filter(n_forecasts_50 >= (max(n_forecasts_50)*0.5) | n_forecasts_95 >= (max(n_forecasts_95)*0.5)) %>% 
    droplevels()
}


#PLOTTING FUNCTIONS 
# plot_by_location_mae: Plot average MAE by location
# plot_by_location_wis: Plot average WIS by location
# plot_byweek_function: Plot WIS by week 
# plot_n_location: Plot number of locations
# plot_truth: Plot truth data at US level
# wis_barplot_function: WIS barplot function
# wis_barplot_function_c: WIS barplot function (control ylim)

#Plot average MAE by location
plot_by_location_mae <- function(df) {
  
  scores <-  df %>%
    filter(horizon %in% c(1:4)) %>%
    filter(score_name == "abs_error") %>%
    filter(n_horizons == max(n_horizons),
           n_weeks >= max(n_weeks)*0.5 | n_weeks_3wksPrior >= 2 ) %>%
    rename(abs_error = score_value) %>%
    left_join(hub_locations %>% select(location = fips, abbreviation, location_name))
  
  
  # the included models and locations:
  models <- unique(scores$model)
  locations <- unique(scores$location)
  location_names <- unique(scores$location_name)
  
  # function for pairwise comparison of models
  pairwise_comparison_location_mae <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                               permutation_test = FALSE){
    
    ############## This line had been deleted, but needs to be included ##########################
    # apply subset:
    scores <- scores[subset, ]
    
    # subsets of available scores for both models:
    subx <- subset(scores, model == mx)
    suby <- subset(scores, model == my)
    
    # merge together and restrict to overlap:
    sub <- merge(subx, suby, by = c("forecast_date", "location", "horizon"),
                 all.x = FALSE, all.y = FALSE)
    
    # compute ratio:
    ratio <- sum(sub$abs_error.x) / sum(sub$abs_error.y)
    
    # perform permutation tests:
    if(permutation_test){
      pval <- permutationTest(sub$abs_error.x, sub$abs_error.y,
                              nPermutation = 999)$pVal.permut
      
      # aggregate by forecast date:
      sub_fcd <- aggregate(cbind(abs_error.x, abs_error.y) ~ forecast_date, data = sub, FUN = mean) 
      pval_fcd <- permutationTest(sub_fcd$abs_error.x, sub_fcd$abs_error.y,
                                  nPermutation = 999)$pVal.permut
    }else{
      pval <- NULL
      pval_fcd <- NULL
    }
    
    return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
  }
  
  
  # compute pairwise and relative abs error for each location separately:
  for(i in seq_along(locations)){
    
    # select location:
    loc <- locations[i]
    loc_name <- location_names[i]
    
    # matrix to store:
    results_ratio_temp <- matrix(ncol = length(models),
                                 nrow = length(models),
                                 dimnames = list(models, models)) 
    
    # run pairwise comparison for chosen location:
    for(mx in seq_along(models)){
      for(my in 1:mx){
        pwc <- pairwise_comparison_location_mae(scores = scores, mx = models[mx], my = models[my],
                                                permutation_test = FALSE, # disable permutation test to speed up things
                                                subset = scores$location == loc) # this will subset to the respective location inside the function
        results_ratio_temp[mx, my] <- pwc$ratio
        results_ratio_temp[my, mx] <- 1/pwc$ratio
      }
    }
    
    # compute the geometric means etc
    ind_baseline <- which(rownames(results_ratio_temp) == "COVIDhub-baseline")
    geom_mean_ratios_temp <- exp(rowMeans(log(results_ratio_temp[, -ind_baseline]), na.rm = TRUE))
    ratios_baseline_temp <- results_ratio_temp[, "COVIDhub-baseline"]
    ratios_baseline2_temp <- geom_mean_ratios_temp/geom_mean_ratios_temp["COVIDhub-baseline"]
    
    # summarize results:
    to_add <- data.frame(model = names(ratios_baseline2_temp),
                         location = loc,
                         location_name = loc_name,
                         relative_mae = ratios_baseline2_temp,
                         log_relative_mae = log(ratios_baseline2_temp))
    
    # append to already stored:
    if(i == 1){ # initialize at first location
      average_by_loc <- to_add
    }else{
      average_by_loc <- rbind(average_by_loc, to_add)
    }
    
    #cat("Finished", loc_name, "\n")
  }
  
  
  average_by_loc_to_plot <- average_by_loc %>%
    filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
    mutate(relative_mae_text = sprintf("%.1f", round(relative_mae, 1)),
           log_relative_mae = log2(relative_mae)) %>%
    filter(!is.na(relative_mae)) 
  
  average_by_loc_to_plot$model <- reorder(average_by_loc_to_plot$model, -average_by_loc_to_plot$log_relative_mae)
  
  # plot:
  avg_by_loc <- ggplot(average_by_loc_to_plot, 
                       aes(x=model, y=location_name, 
                           fill= scales::oob_squish(log_relative_mae, range = c(- 2.584963, 2.584963)))) +
    geom_tile() +
    geom_text(aes(label = relative_mae_text), size = 2.5) + # I adapted the rounding
    scale_fill_gradient2(low = "steelblue", high = "red", midpoint = 0, na.value = "grey50", 
                         name = "Relative MAE", 
                         breaks = c(-2,-1,0,1,2), 
                         labels =c("0.25", 0.5, 1, 2, 4)) + 
    xlab(NULL) + ylab(NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.title.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          title = element_text(size = 9)) 
  
  print(avg_by_loc)
}



#Plot average WIS by location
# select relevant columns:
plot_by_location_wis <- function(df, order, location_order,subt) {
  scores <- df %>% 
    filter(horizon %in% c(1:4)) %>%
    filter(score_name == "wis") %>%
    filter(n_horizons == max(n_horizons),
           n_weeks >= max(n_weeks)*0.5) %>%
    rename(wis = score_value) %>%
    left_join(hub_locations %>% select(location = fips, abbreviation, location_name)) 
  
  
  # the included models and locations:
  models <- unique(scores$model)
  locations <- unique(scores$location)
  location_names <- unique(scores$location_name)
  
  # function for pairwise comparison of models
  pairwise_comparison_location <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                           permutation_test = FALSE){
    
    ############## This line had been deleted, but needs to be included ##########################
    # apply subset:
    scores <- scores[subset, ]
    
    # subsets of available scores for both models:
    subx <- subset(scores, model == mx)
    suby <- subset(scores, model == my)
    
    # merge together and restrict to overlap:
    sub <- merge(subx, suby, by = c("forecast_date", "location", "horizon"),
                 all.x = FALSE, all.y = FALSE)
    
    # compute ratio:
    ratio <- sum(sub$wis.x) / sum(sub$wis.y)
    
    # perform permutation tests:
    if(permutation_test){
      pval <- permutationTest(sub$wis.x, sub$wis.y,
                              nPermutation = 999)$pVal.permut
      
      # aggregate by forecast date:
      sub_fcd <- aggregate(cbind(wis.x, wis.y) ~ forecast_date, data = sub, FUN = mean) 
      pval_fcd <- permutationTest(sub_fcd$wis.x, sub_fcd$wis.y,
                                  nPermutation = 999)$pVal.permut
    }else{
      pval <- NULL
      pval_fcd <- NULL
    }
    
    return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
  }
  
  
  
  # compute pairwise and relative WIS for each location separately:
  for(i in seq_along(locations)){
    
    # select location:
    loc <- locations[i]
    loc_name <- location_names[i]
    
    # matrix to store:
    results_ratio_temp <- matrix(ncol = length(models),
                                 nrow = length(models),
                                 dimnames = list(models, models)) 
    
    # run pairwise comparison for chosen location:
    for(mx in seq_along(models)){
      for(my in 1:mx){
        pwc <- pairwise_comparison_location(scores = scores, mx = models[mx], my = models[my],
                                            permutation_test = FALSE, # disable permutation test to speed up things
                                            subset = scores$location == loc) # this will subset to the respective location inside the function
        results_ratio_temp[mx, my] <- pwc$ratio
        results_ratio_temp[my, mx] <- 1/pwc$ratio
      }
    }
    
    # compute the geometric means etc
    ind_baseline <- which(rownames(results_ratio_temp) == "COVIDhub-baseline")
    geom_mean_ratios_temp <- exp(rowMeans(log(results_ratio_temp[, -ind_baseline]), na.rm = TRUE))
    ratios_baseline_temp <- results_ratio_temp[, "COVIDhub-baseline"]
    ratios_baseline2_temp <- geom_mean_ratios_temp/geom_mean_ratios_temp["COVIDhub-baseline"]
    
    # summarize results:
    to_add <- data.frame(model = names(ratios_baseline2_temp),
                         location = loc,
                         location_name = loc_name,
                         relative_wis = ratios_baseline2_temp,
                         log_relative_wis = log(ratios_baseline2_temp))
    
    # append to already stored:
    if(i == 1){ # initialize at first location
      average_by_loc <- to_add
    }else{
      average_by_loc <- rbind(average_by_loc, to_add)
    }
    
    # cat("Finished", loc_name, "\n")
  }
  
  average_by_loc_to_plot <- average_by_loc %>%
    filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
    mutate(relative_wis_text = sprintf("%.1f", round(relative_wis, 1)),
           log_relative_wis = log2(relative_wis)) %>%
    filter(!is.na(relative_wis)) %>%
    mutate(model = fct_relevel(model, order),
           location_name = fct_relevel(location_name, location_order))
  
  
  
  # plot:
  ggplot(average_by_loc_to_plot, 
         aes(x=model, y=location_name, 
             fill= scales::oob_squish(log_relative_wis, range = c(- 2.584963, 2.584963)))) +
    geom_tile() +
    geom_text(aes(label = relative_wis_text), size = 2.5) + # I adapted the rounding
    scale_fill_gradient2(low = "steelblue", high = "red", midpoint = 0, na.value = "grey50", 
                         name = "Relative WIS", 
                         breaks = c(-2,-1,0,1,2), 
                         labels =c("0.25", 0.5, 1, 2, 4)) +
    ggtitle(paste0(subt)) + 
    xlab(NULL) + ylab(NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.title.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          title = element_text(size = 9)) 
}

plot_byweek_function <- function(df, var, horizon_num,subt) {
  ggplot(data =  df %>% filter(horizon == horizon_num), aes(label = model, 
                                                            labelx = target_end_date,
                                                            labely = mean_score,
                                                            x = target_end_date, 
                                                            y = mean_score, color = model)) +
    geom_line(aes(group = model), alpha=.5) +
    geom_point(aes(group = model), alpha=.5, size = 2) +
    expand_limits(y=0) +
    scale_y_continuous(name = paste("Average",var)) +
    # guides(color=FALSE, group = FALSE) +
    guides(color="none", group = "none") +
    ggtitle(paste0("Average ", horizon_num,"-week ahead ",var," by model",
                   '<br>',
                   '<sup>',
                    subt,
                   '<sup>')) +
    xlab("Target End Date") +
    theme(axis.ticks.length.x = unit(0.5, "cm"),
          axis.text.x = element_text(vjust = 7, hjust = -0.2))
}

#Plot number of locations
plot_n_location <- function(x){
  ggplot(x, aes(y=model, x= submission_sat, fill=n_location)) +
    geom_tile() +
    geom_text(aes(label=n_location), size = 7) +
    scale_fill_steps(low="white", high="blue", name = "Number of Locations") +
    xlab("Submission Saturday") + ylab(NULL) +
    scale_x_date(date_labels = "%Y-%m-%d", breaks = c(x$submission_sat)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
          axis.title.x = element_text(size = 30),
          axis.text.y = element_text(size = 25),
          title = element_text(size = 20)) +
    # guides(fill=FALSE)
  guides(fill="none")
}

#Plot truth data at US level
plot_truth <- function(dat,tar,subtar,ylab) {
  ggplot(data = dat, aes(x = target_end_date, y = value)) +
    #geom_line(color = "black") +
    geom_point() +
    geom_line(color = "black") +
    scale_x_date(name = NULL, date_breaks="4 month", date_labels = "%b %y") +
    ylab(ylab) +
    labs(title = paste(tar),
         subtitle=paste(subtar),
         caption="source: JHU CSSE (observed data)")+
    theme(legend.position = c(.05,.95), legend.justification = c(0,1)) +
    geom_vline(aes(xintercept= c(first_eval_sat -3.5), color = "Recent Start Date"), linetype=2, size=1) +
    geom_vline(aes(xintercept= c(first_eval_sat_hist - 3.5), color = "Historic Start Date"), linetype=6, size=1) + 
    scale_color_manual(name = "", values = c("Recent Start Date" = "blue","Historic Start Date" ="darkgreen"))# , "Submission Date Boundaries" = "red"))
}

#WIS barplot function (without ylim)
wis_barplot_fun <- function(x,y,order) {
  wis_plot <- x %>%
    filter(target_end_date >= first_eval_sat) %>%
    filter(score_name %in% c("dispersion","overprediction", "underprediction")) %>%
    group_by(model, score_name) %>%
    summarise(mean_values = mean(score_value,na.rm = T)) %>%
    mutate(n_forecasts = n()) %>%
    ungroup() %>%
    droplevels()  %>%
    filter(model %in% y$model) %>%
    mutate(score_name=factor(score_name,c("overprediction","dispersion","underprediction")),
           model = fct_relevel(model, order)) %>%
    arrange(model,score_name)

  ggplot(wis_plot, aes(fill=score_name, y=mean_values, x=model)) +
    geom_bar(position="stack", stat="identity", width = .75) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
          legend.title = element_blank(),
          axis.title.x =  element_blank()) +
    labs(y = "WIS components",title="Based on log transformed counts")
}

#WIS barplot function (control ylim)
wis_barplot_function <- function(x,y,order) {
  wis_plot <- x %>% 
    filter(target_end_date >= first_eval_sat) %>%
    filter(score_name %in% c("dispersion","overprediction", "underprediction")) %>% 
    group_by(model, score_name) %>% 
    summarise(mean_values = mean(score_value,na.rm = T)) %>% 
    mutate(n_forecasts = n()) %>%
    ungroup() %>%
    droplevels()  %>%
    filter(model %in% y$model) %>%
    mutate(score_name=factor(score_name,c("overprediction","dispersion","underprediction")),
           model = fct_relevel(model, order)) %>%
    arrange(model,score_name)
  
  #find yaxis limit
  wis_plot1 <- wis_plot %>% 
    group_by(model) %>% 
    summarise(sum_values = sum(mean_values,na.rm = T))
  ylim<-round(quantile(wis_plot1$sum_values,probs=0.95, na.rm = TRUE),digits=-1)
  
  ggplot(wis_plot, aes(fill=score_name, y=mean_values, x=model)) + 
    geom_bar(position="stack", stat="identity", width = .75) +
    coord_cartesian(ylim=c(0, ylim)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
          legend.title = element_blank(),
          axis.title.x =  element_blank()) +
    labs(y = "WIS components",title="Based on raw counts")
}





#TABLE FUNCTIONS (inactive)
# calib_table: 

#Filter for inclusion in recent coverage table 
##Inclusion criteria: 
##1. Must have all quantiles
##2. Must have submitted a forecast for 5 weeks total or at least 2 out of the last 3 weeks
calib_table <- function(x){
  
  historical_calib <-  x  %>% 
    filter(n_weeks >= 5 |  n_weeks_3wksPrior >= 2) %>% 
    filter(!is.na(score_value))  %>%
    filter(score_name %in% c("coverage_50","coverage_95")) %>%
    pivot_wider(names_from = score_name, values_from = score_value) %>%
    group_by(model,horizon) %>%
    summarise(n_forecasts_historical = n(),
              mean_PI50 = round(sum(coverage_50, na.rm = TRUE) / n(),2),
              mean_PI95 = round(sum(coverage_95, na.rm = TRUE) / n(),2)) %>% ungroup() 
  
  
  recent_calib  <- x  %>%
    filter(n_weeks_10wksPrior >= 5 |  n_weeks_3wksPrior >= 2) %>% 
    filter(!is.na(score_value))  %>%
    filter(score_name %in% c("coverage_50","coverage_95")) %>%
    filter(target_end_date >= first_eval_sat) %>%  droplevels() %>%
    pivot_wider(names_from = score_name, values_from = score_value) %>%
    group_by(model,horizon) %>%
    summarise(n_forecasts_recent = n(),
              mean_PI50_recent = round(sum(coverage_50, na.rm = TRUE) / n(),2),
              mean_PI95_recent = round(sum(coverage_95, na.rm = TRUE) / n(),2)) %>%
    ungroup() 
  
  calibration_file <- merge(historical_calib, recent_calib, by = "model")
  
  calibration_table <-  calibration_file  %>%
    select(model, n_forecasts_recent, mean_PI50_recent, mean_PI95_recent, n_forecasts_historical, mean_PI50, mean_PI95) %>%
    arrange(-mean_PI50_recent)
  
  render <- JS(
    "function(data, type, row) {",
    "  if(type === 'sort' && data === null) {",
    "    return 999999;",
    "  }",
    "  return data;",
    "}"
  )
  
  print(datatable(calibration_table, rownames= FALSE, 
                  colnames = c("Model", "n recent forecasts", "Recent 50% coverage", "Recent 95% coverage",
                               "n historical forecasts" , "Historical 50% coverage", "Historical 95% coverage"), 
                  options =  list(pageLength = 5, 
                                  autoWidth = TRUE,
                                  columnDefs = list(list(width = '200px', targets = "_all", render = render)), 
                                  ordering = TRUE),
                  filter = c("top")))
  
}