#Functions for use in weekly report 

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
    group_by(model) %>%
    summarise(n_forecasts_historical = n(),
              mean_PI50 = round(sum(coverage_50, na.rm = TRUE) / n(),2),
              mean_PI95 = round(sum(coverage_95, na.rm = TRUE) / n(),2)) %>% ungroup() 
  
  
  recent_calib  <- x  %>%
    filter(n_weeks >= 5 |  n_weeks_3wksPrior >= 2) %>% 
    filter(!is.na(score_value))  %>%
    filter(score_name %in% c("coverage_50","coverage_95")) %>%
    filter(target_end_date >= first_eval_sat) %>%  droplevels() %>%
    pivot_wider(names_from = score_name, values_from = score_value) %>%
    group_by(model) %>%
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



#filter for inclusion in historical coverage table 
##Keep only models that have submitted forecasts for at least half of the number of max WIS forecasts or half the max MAE forecasts 
recent_accuracy_filter <- function(x) {
  x %>%
    filter(!is.na(score_value)) %>%  #remove NAs 
    filter(target_end_date >= first_eval_sat) %>% #
    group_by(model, score_name) %>%
    mutate(n_forecasts_wis = sum(score_name == "wis" & !is.na(score_value)),
           n_forecasts_mae = sum(score_name == "abs_error" & !is.na(score_value))) %>% ungroup() %>%
    filter(n_forecasts_wis >= (max(n_forecasts_wis)*0.5) | n_forecasts_mae >= (max(n_forecasts_mae)*0.5)) %>% 
    droplevels()
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




#PLOTTING FUNCTIONS 

#Plot truth data at US level
plot_truth <- function(dat,tar) {
  ggplot(data = dat, aes(x = target_end_date, y = value)) +
    #geom_line(color = "black") +
    geom_point() +
    geom_line(color = "black") +
    scale_x_date(name = NULL, date_breaks="4 month", date_labels = "%b %d") +
    ylab("incident cases") +
    labs(title = paste("Weekly reported COVID-19 ", tar),
         caption="source: JHU CSSE (observed data)")+
    theme(legend.position = c(.05,.95), legend.justification = c(0,1)) +
    geom_vline(aes(xintercept= c(first_eval_sat -3.5), color = "Recent Start Date"), linetype=2) +
    geom_vline(aes(xintercept= c(first_eval_sat_hist - 3.5), color = "Histotic Start Date"), linetype=2) + 
    scale_color_manual(name = "", values = c("Recent Start Date" = "blue","Historic Start Date" ="black"))# , "Submission Date Boundaries" = "red"))
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



#Plot average WIS by location
# select relevant columns:
plot_by_location_wis <- function(df, order, location_order) {
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
    xlab(NULL) + ylab(NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.title.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          title = element_text(size = 9)) 
}





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


#WIS barplot function
wis_barplot_function <- function(x,y,order) {
  wis_plot <- x %>% 
    filter(target_end_date >= first_eval_sat) %>%
    filter(score_name %in% c("sharpness","overprediction", "underprediction")) %>% 
    group_by(model, score_name) %>% 
    summarise(mean_values = mean(score_value,na.rm = T)) %>% 
    mutate(n_forecasts = n()) %>%
    ungroup() %>%
    droplevels()  %>%
    filter(model %in% y$model) %>%
    mutate(model = fct_relevel(model, order))
  
  ggplot(wis_plot, aes(fill=score_name, y=mean_values, x=model)) + 
    geom_bar(position="stack", stat="identity", width = .75) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
          legend.title = element_blank(),
          axis.title.x =  element_blank()) +
    labs(y = "WIS components")
}



#Filter and plot WIS by week 
by_week_function <- function(df, var) {
  df %>%
    filter(score_name == var) %>%
    filter(location %in% c(US_fips)) %>%
    group_by(model, target_end_date, horizon) %>%
    mutate(n_US_location = n()) %>%
    ungroup() %>%
    filter(n_US_location == max(n_US_location)) %>%
    group_by(model,horizon, target_end_date) %>%
    summarise(mean_score = mean(score_value))
}

plot_byweek_function <- function(df, var, horizon_num) {
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
    ggtitle(paste0("Average ", horizon_num,"-week ahead ",var," by model")) +
    xlab("Target End Date") +
    theme(axis.ticks.length.x = unit(0.5, "cm"),
          axis.text.x = element_text(vjust = 7, hjust = -0.2))
}