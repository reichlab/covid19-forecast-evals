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
distance_pairwise <- function(modelA,modelB,quantiles=NULL, spline_method,
                              point_to_interpolate=NULL, distance){
  if (distance!="approx_cramer"){
    A <- spline(x=quantiles, y=modelA, method = spline_method,xout=point_to_interpolate)
    B <- spline(x=quantiles, y=modelB, method = spline_method,xout=point_to_interpolate)
    if(distance=="CvM"){
      metric <- cramer::cramer.test(A$y,B$y,just.statistic=TRUE)$statistic
      }
    else if (distance=="cramer"){
      metric <- energy::eqdist.e(c(A$y,B$y), rep(length(point_to_interpolate),2))/2
      } 
  } else if (distance=="approx_cramer"){
    metric<- approx_cd(modelA, modelB)
  }
  return(metric)
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

# cramer distance approximation for quantiles from Johannes
approx_cd <- function(q_F, q_G){
  # compute quantile levels from length of provided quantile vectors:
  K <- length(q_F)
  if(length(q_G) != K) stop("q_F and q_G need to be of the same length")
  p <- (1:K)/(K + 1) # function assumes that the quantile levels are equally spaced
  
  # pool quantiles:
  q0 <- c(q_F, q_G)
  # vector of grouping variables, with 1 for values belonging to F, -1 for values 
  # belonging to G
  a0 <- c(rep(1, length(q_F)), rep(-1, length(q_G)))
  
  # re-order both vectors:
  q <- q0[order(q0)]
  a <- a0[order(q0)]
  # and compute "how many quantiles ahead" F or G is at a given segment:
  b <- abs(cumsum(a))
  
  # compute the lengths of segments defined by sorted quantiles:
  diffs_q <- c(diff(q), 0) # zero necessary for indexing below, but we could put 
  # anything (gets multiplied w zero)
  # and approximate CD
  cvm <- sum(diffs_q*b*(b + 1))/(K + 1)/(K)
  return(mean(cvm))
}
## a function that takes a data frame with single target-location and calculates CvM for pairwise combinations
## this returns a matrix of CvM for the models for a single target-location
distance_combination <- function(single_tarloc_frame,spline_method=NULL,
                                 point_to_interpolate=NULL,distance){
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
    cc <- distance_pairwise(tmp[,eg[i,1]], tmp[,eg[i,2]],quantiles,
                            spline_method,
                            point_to_interpolate,
                            distance)
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
build_distance_frame <- function(model_dataframe, spline_method=NULL, 
                                 target_list,point_to_interpolate=NULL, distance){
  library(tidyverse)
  library(tidyr)
  # remove point estimates and forecast_date
  main_frame <- model_dataframe %>% 
      dplyr::filter(type=="quantile",target_variable %in% target_list) 
  if("forecast_date" %in% c(colnames(main_frame))){
    main_frame <- main_frame %>%
      dplyr::select(-"forecast_date")
  }
  ## apply distance_combination function 
  locations <- unique(main_frame$location)
  dist_list <- list()
  for(loc in locations){
      for(target in target_list){
        name <- paste(loc,target,sep="_") 
          single_matrix <- main_frame %>%
            dplyr::filter(location==loc,
                          target_variable==target) %>%
            dplyr::arrange(target_variable,location,quantile) %>%
            distance_combination(.,spline_method,point_to_interpolate, distance)
        dist_list <- c(dist_list,list(name = single_matrix))
      }
    }
  return(dist_list)
}

# a function that takes a matrix and plot a heatmap
distance_heatmap <- function(single_tarloc_cvm){
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
distance_comparision_pairwise <- function(modelA,modelB,quantiles=NULL, spline_method=NULL,
                                          point_to_interpolate=NULL,est_method){
  if(est_method!=6){
  A <- spline(x=quantiles, y=modelA, method = spline_method,xout=point_to_interpolate)
  B <- spline(x=quantiles, y=modelB, method = spline_method,xout=point_to_interpolate)
  if(est_method==1){
    value <- cramer::cramer.test(A$y,B$y,just.statistic=TRUE)$statistic
  } 
  else if(est_method==2){
    value <- twosamples::cvm_stat(A$y,B$y,power=2)
  }
  else if(est_method==3){
    value <- CDFt::CramerVonMisesTwoSamples(A$y,B$y)
  }
  else if(est_method==4){
    value <- cvm(A$y,B$y)
  }
  else if(est_method==5){
    value <- energy::eqdist.e(as.matrix(c(A$y,B$y)), rep(length(point_to_interpolate),2))/2
  }} else {
    value <- approx_cd(modelA, modelB)
  }
  return(value)
}

distance_combination_compare <- function(single_tarloc_frame,spline_method=NULL,
                                    point_to_interpolate=NULL,est_method){
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
    cc <- distance_comparision_pairwise(tmp[,eg[i,1]], tmp[,eg[i,2]],quantiles,
                                   spline_method,point_to_interpolate,est_method)
    v[i] <- cc
  }
  single_tarloc_cvm <- matrix(v, nc, byrow=TRUE)
  dimnames(single_tarloc_cvm) <- list(cnames, cnames)
  return(single_tarloc_cvm)
}
build_distance_frame_compare <- function(model_dataframe, spline_method=NULL, target_list,
                                    point_to_interpolate=NULL,est_method){
  library(tidyverse)
  library(tidyr)
  # remove point estimates and forecast_date
  main_frame <- model_dataframe %>% 
    dplyr::filter(type=="quantile",target_variable %in% target_list) 
  if("forecast_date" %in% c(colnames(main_frame))){
    main_frame <- main_frame %>%
      dplyr::select(-"forecast_date")
  }
  ## apply distance_combination function 
  locations <- unique(main_frame$location)
  CvM_list <- list()
  
  for(loc in locations){
    for(target in target_list){
      name <- paste(loc,target,sep="_") 
      single_matrix <- main_frame %>%
        dplyr::filter(location==loc,
                      target_variable==target) %>%
        dplyr::arrange(target_variable,location,quantile) %>%
        distance_combination_compare(.,spline_method,point_to_interpolate,est_method)
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

toy_compare <- function(single_frame,spline_method=NULL,point_to_interpolate=NULL,est_method){
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
    cc <- distance_comparision_pairwise(tmp[,eg[i,1]], tmp[,eg[i,2]],quantiles,
                                   spline_method,point_to_interpolate,est_method)
    v[i] <- cc
  }
  single_tarloc_cvm <- matrix(v, nc, byrow=TRUE)
  dimnames(single_tarloc_cvm) <- list(cnames, cnames)
  return(single_tarloc_cvm)
}
