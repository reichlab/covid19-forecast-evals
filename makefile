# make figures
figures: figures/pi-coverage.jpg figures/model-target-week-wis-avgs.jpg figures/inc-loc-heatmap.jpg figures/long-range.jpg

# processes scores after updates to anomaly dates, eligibility changes or new scores
paper-inputs/inc-scores.csv: data-raw/inc-scores-from-zoltar.csv paper-inputs/model-eligibility-inc.csv paper-inputs/anomaly-reporting-dates.csv code/process-zoltar-scores.R
	Rscript code/process-zoltar-scores.R

## update calibration scores
paper-inputs/inc-calibration.csv: paper-inputs/model-eligibility-inc.csv paper-inputs/anomaly-reporting-dates.csv code/get-calibration-scores-inc.R
	Rscript code/get-calibration-scores-inc.R

#paper-inputs/model-eligibility-inc.csv: code/determine-model-eligibility-inc.R
#	Rscript code/determine-model-eligibility-inc.R

# individual figures
figures/pi-coverage.jpg: code/figure-calibration.R paper-inputs/inc-calibration.csv paper-inputs/inc-scores.csv
	Rscript code/figure-calibration.R

figures/model-target-week-wis-avgs.jpg: code/figure-model-week-target-wis-avgs.R paper-inputs/inc-scores_all.csv paper-inputs/inc-scores.csv
	Rscript code/figure-model-week-target-wis-avgs.R

figures/inc-loc-heatmap.jpg: code/figure-heatmap-locations.R
	Rscript code/figure-heatmap-locations.R

figures/long-range.jpg: code/figure-long-term-comparisons.R paper-inputs/inc-calibration.csv paper-inputs/inc-scores.csv
	Rscript code/figure-long-term-comparisons.R
