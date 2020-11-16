# helper function
next_monday <- function(date){
  nm <- rep(NA, length(date))
  for(i in seq_along(date)){
    nm[i] <- date[i] + (0:6)[weekdays(date[i] + (0:6)) == "Monday"]
  }
  return(as.Date(nm, origin = "1970-01-01"))
}

# load scores:
scores <- read.csv("paper-inputs/inc-scores.csv",  colClasses = list(timezero = "Date"), stringsAsFactors = FALSE)
#scores <- read.csv("../paper-inputs/inc-scores.csv",  colClasses = list(timezero = "Date"), stringsAsFactors = FALSE)

# bring all timezeros to Monday:
scores$timezero <- next_monday(scores$timezero)

# restrict to 1-4 wk ahead state-level (this had been missing previously!)
scores <- subset(scores, target %in% paste(1:4, "wk ahead inc death") &
                   unit != "US")

scores <- scores[, c("model", "timezero", "unit", "target", "abs_error", "wis")]

# the included models:
models <- unique(scores$model)


invisible(library(surveillance)) # contains permutation test

# function for pairwise comparison of models
pairwise_comparison <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                permutation_test = FALSE){
  # apply subset:
  scores <- scores[subset, ]
  
  # subsets of available scores for both models:
  subx <- subset(scores, model == mx)
  suby <- subset(scores, model == my)
  
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("timezero", "unit", "target"),
               all.x = FALSE, all.y = FALSE)
  
  # compute ratio:
  ratio <- sum(sub$wis.x) / sum(sub$wis.y)
  
  # perform permutation tests:
  if(permutation_test){
    pval <- permutationTest(sub$wis.x, sub$wis.y,
                            nPermutation = 999)$pVal.permut
    
    # aggregate by forecast date:
    sub_fcd <- aggregate(cbind(wis.x, wis.y) ~ timezero, data = sub, FUN = mean)
    pval_fcd <- permutationTest(sub_fcd$wis.x, sub_fcd$wis.y,
                                nPermutation = 999)$pVal.permut
  }else{
    pval <- NULL
    pval_fcd <- NULL
  }
  
  return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
}

# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

set.seed(123) # set seed for permutation tests

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                               permutation_test = TRUE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
    results_pval[mx, my] <-
      results_pval[my, mx] <- pwc$pval
    results_pval_fcd[mx, my] <-
      results_pval_fcd[my, mx] <- pwc$pval_fcd
  }
}



ind_baseline <- which(rownames(results_ratio) == "COVIDhub-baseline")
geom_mean_ratios <- exp(rowMeans(log(results_ratio[, -ind_baseline]), na.rm = TRUE))
ratios_baseline <- results_ratio[, "COVIDhub-baseline"]
ratios_baseline2 <- geom_mean_ratios/geom_mean_ratios["COVIDhub-baseline"]

tab <- data.frame(model = names(geom_mean_ratios),
                  geom_mean_ratios = geom_mean_ratios,
                  ratios_baseline = ratios_baseline,
                  ratios_baseline2 = ratios_baseline2)

tab <- tab[order(tab$ratios_baseline2), ]


tab #final column is theta^*_iB
