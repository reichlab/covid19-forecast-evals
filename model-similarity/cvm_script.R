## wrapper and functions for calculating pairwise CvM
## a function to format the dataframe
frame_format <- function(zoltr_frame){
  formatted_frame <- zoltr_frame %>%
    dplyr::select("location","target_variable","target_end_date",
                  "type","quantile","model","value") %>%
    tidyr::pivot_wider(names_from = model, values_from = value) %>%
    dplyr::group_by(target_variable,target_end_date,location,type) %>%
    dplyr::arrange(location,target_variable,target_end_date,type,quantile) %>%
    ungroup() 
  return(formatted_frame)
} 

## CvM function (we likely need to fix this)
CvM_pairwise <- function(modelA,modelB,quantiles, spline_method,point_to_interpolate){
  A <- spline(x=quantiles, y=modelA, method = spline_method,xout=point_to_interpolate)
  B <- spline(x=quantiles, y=modelB, method = spline_method,xout=point_to_interpolate)
  CvM_value <- cramer::cramer.test(A$y,B$y,just.statistic=TRUE)$statistic
  return(CvM_value)
}

# from http://estatcomp.github.io/henrique/exer_chap8.html
cvm <- function(x, y){
  n <- length(x)
  m <- length(y)
  v.n <- vector("numeric", n) # Replication vectors
  v.m <- vector("numeric", m)
  z <- c(x, y)
  N <- length(z)
  for (i in 1:n) v.n[i] <- ( x[i] - i )**2
  for (j in 1:m) v.m[j] <- ( y[j] - j )**2
  # Test statistic
  cvm <- ( (n * sum(v.n) + m * sum(v.m)) / (m * n * N) ) -
    (4 * m * n - 1) / (6 * N)
  return(cvm)
}

## a function that takes a data frame with single target-location and calculates CvM for pairwise combinations
## this returns a matrix of CvM for the models for a single target-location
CvM_combination <- function(single_tarloc_frame,spline_method,point_to_interpolate){
  # remove any models with NA for values for this target location (assuming all models have the same quantiles)
  single_tarloc_frame<- single_tarloc_frame[ , colSums(is.na(single_tarloc_frame)) == 0]
  quantiles <- single_tarloc_frame$quantile
  # pairwise column calculation
  tmp <- single_tarloc_frame %>%
    dplyr::select(-c("target_variable","target_end_date","location","type","quantile"))
  nc <- ncol(tmp)
  cnames <- colnames(tmp)
  eg <- expand.grid(1:nc, 1:nc)
  nr <- nrow(eg)
  v <- vector(length=nr)
  for (i in 1:nr) {
    cc <- CvM_pairwise(tmp[,eg[i,1]], tmp[,eg[i,2]],quantiles,spline_method,point_to_interpolate)
    v[i] <- cc
  }
  single_tarloc_cvm <- matrix(v, nc, byrow=TRUE)
  dimnames(single_tarloc_cvm) <- list(cnames, cnames)
  return(single_tarloc_cvm)
}

## A wrapper to create a list of CvM matrices for each forecast_date-target-location combination
## model_dataframe, this should be a union join with NA for non-overlapping target-locations
### single_tarloc_frame is a data frame containing one target-location created in the wrapper function 
### by filter on target and location
build_CvM_frame <- function(model_dataframe, spline_method, target_list,point_to_interpolate){
  library(tidyverse)
  library(tidyr)
  # remove point estimates and forecast_date
  main_frame <- model_dataframe %>% 
      dplyr::filter(type=="quantile",target_variable %in% target_list) 
  if("forecast_date" %in% c(colnames(main_frame))){
    main_frame <- main_frame %>%
      dplyr::select(-"forecast_date")
  }
  ## apply CvM_combination function 
  locations <- unique(main_frame$location)
  CvM_list <- list()
  
  for(loc in locations){
      for(target in target_list){
        name <- paste(loc,target,sep="_") 
        single_matrix <- main_frame %>%
          dplyr::filter(location==loc,
                        target_variable==target) %>%
          dplyr::arrange(target_variable,location,quantile) %>%
          CvM_combination(.,spline_method,point_to_interpolate)
        CvM_list <- c(CvM_list,list(name = single_matrix))
      }
    }
  return(CvM_list)
}

# a function that takes a matrix and plot a heatmap
CvM_heatmap <- function(single_tarloc_cvm){
  dat <- data.frame(var1=rownames(single_tarloc_cvm)[row(single_tarloc_cvm)], 
                    var2=colnames(single_tarloc_cvm)[col(single_tarloc_cvm)], 
                    cvm=c(single_tarloc_cvm))
  x <- dat$var2
  ggplot(dat, aes(var1, var2, fill= cvm)) + 
    geom_tile() +
    theme(axis.text.x=element_text(angle=-90,hjust=1))+
    ylab("")+
    xlab("")+
    geom_text(aes(label=round(cvm,2))) 
}


## comparison
CvM_comparision_pairwise <- function(modelA,modelB,quantiles, spline_method,point_to_interpolate,est_method){
  A <- spline(x=quantiles, y=modelA, method = spline_method,xout=point_to_interpolate)
  B <- spline(x=quantiles, y=modelB, method = spline_method,xout=point_to_interpolate)
  if(est_method==1){
    CvM_value <- cramer::cramer.test(A$y,B$y,just.statistic=TRUE)$statistic
  } 
  else if(est_method==2){
    CvM_value <- twosamples::cvm_stat(A$y,B$y,power=2)
  }
  else if(est_method==3){
    CvM_value <- CDFt::CramerVonMisesTwoSamples(A$y,B$y)
  }
  else if(est_method==4){
    CvM_value <- CvM_value <- cvm(A$y,B$y)
  }
  return(CvM_value)
}

CvM_combination_compare <- function(single_tarloc_frame,spline_method,point_to_interpolate,est_method){
  # remove any models with NA for values for this target location (assuming all models have the same quantiles)
  single_tarloc_frame<- single_tarloc_frame[ , colSums(is.na(single_tarloc_frame)) == 0]
  quantiles <- single_tarloc_frame$quantile
  # pairwise column calculation
  tmp <- single_tarloc_frame %>%
    dplyr::select(-c("target_variable","target_end_date","location","type","quantile"))
  nc <- ncol(tmp)
  cnames <- colnames(tmp)
  eg <- expand.grid(1:nc, 1:nc)
  nr <- nrow(eg)
  v <- vector(length=nr)
  for (i in 1:nr) {
    cc <- CvM_comparision_pairwise(tmp[,eg[i,1]], tmp[,eg[i,2]],quantiles,
                                   spline_method,point_to_interpolate,est_method)
    v[i] <- cc
  }
  single_tarloc_cvm <- matrix(v, nc, byrow=TRUE)
  dimnames(single_tarloc_cvm) <- list(cnames, cnames)
  return(single_tarloc_cvm)
}
build_CvM_frame_compare <- function(model_dataframe, spline_method, target_list,point_to_interpolate,est_method){
  library(tidyverse)
  library(tidyr)
  # remove point estimates and forecast_date
  main_frame <- model_dataframe %>% 
    dplyr::filter(type=="quantile",target_variable %in% target_list) 
  if("forecast_date" %in% c(colnames(main_frame))){
    main_frame <- main_frame %>%
      dplyr::select(-"forecast_date")
  }
  ## apply CvM_combination function 
  locations <- unique(main_frame$location)
  CvM_list <- list()
  
  for(loc in locations){
    for(target in target_list){
      name <- paste(loc,target,sep="_") 
      single_matrix <- main_frame %>%
        dplyr::filter(location==loc,
                      target_variable==target) %>%
        dplyr::arrange(target_variable,location,quantile) %>%
        CvM_combination_compare(.,spline_method,point_to_interpolate,est_method)
      CvM_list <- c(CvM_list,list(name = single_matrix))
    }
  }
  return(CvM_list)
}

## toy functions
sample_quantile <- function(x, prob) {
  if (is.unsorted(x)) x <- sort(x)
  n <- length(x)
  approx(seq(0, 1, length = n), x, prob)$y
}

toy_compare <- function(single_frame,spline_method,point_to_interpolate,est_method){
  # remove any models with NA for values for this target location (assuming all models have the same quantiles)
  quantiles <- single_frame$q
  # pairwise column calculation
  tmp <- single_frame %>%
    dplyr::select(-"q")
  nc <- ncol(tmp)
  cnames <- colnames(tmp)
  eg <- expand.grid(1:nc, 1:nc)
  nr <- nrow(eg)
  v <- vector(length=nr)
  for (i in 1:nr) {
    cc <- CvM_comparision_pairwise(tmp[,eg[i,1]], tmp[,eg[i,2]],quantiles,
                                   spline_method,point_to_interpolate,est_method)
    v[i] <- cc
  }
  single_tarloc_cvm <- matrix(v, nc, byrow=TRUE)
  dimnames(single_tarloc_cvm) <- list(cnames, cnames)
  return(single_tarloc_cvm)
}