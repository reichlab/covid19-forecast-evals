

unit_timezero_forecast_complete <- function(dat, 
    type = c("inc death", "cum death"), 
    horizons = 1:4,
    quantiles = c(0.01, 0.025, seq(0.05, 0.95, by=0.05), 0.975, 0.99)) {
    
    require(tidyverse)
    
    type <- match.arg(type)
    
    tmp_target <- paste(horizons, "wk ahead", type, "death")
    
    all_combos <- crossing(target = tmp_target, location=unique(dat$location), quantile=round(quantiles, digits=3))
    missing_combos <- anti_join(all_combos, dat)

    if(nrow(missing_combos)==0){
        message("prediction complete")
        return(TRUE)
    } else {
        message("prediction is incomplete")
        print(missing_combos, n=10)
        return(FALSE)
    }
}
