# make figures
figures: figures/data-and-forecast.jpg figures/pi-coverage.jpg figures/model-target-week-wis-avgs.jpg figures/inc-loc-heatmap.jpg figures/long-range.jpg figures/fig-wis-location.jpg figures/fig-model-ranks.jpg

# processes scores after updates to anomaly dates, eligibility changes or new scores
paper-inputs/inc-scores.csv: paper-inputs/model-eligibility-inc.csv code/process-zoltar-scores.R
	Rscript code/get_forecasts_covidhubutils.R

paper-inputs/model-eligibility-inc.csv: code/determine-model-eligibility-inc.R code/load-global-analysis-dates.R 
	Rscript code/determine-model-eligibility-inc.R

# individual figures
figures/pi-coverage.jpg: code/figure-calibration.R paper-inputs/inc-calibration.csv paper-inputs/inc-scores.csv
	Rscript code/figure-calibration.R

## update calibration scores
paper-inputs/inc-calibration.csv: paper-inputs/model-eligibility-inc.csv paper-inputs/anomaly-reporting-dates.csv code/get-calibration-scores-inc.R
	Rscript code/get-calibration-scores-inc.R

## this script builds 3 figures: model-target-week-wis-avgs, week-model-target-fig4, overall-wis-boxplot
figures/model-target-week-wis-avgs.jpg: code/figure-model-week-target-wis-avgs.R paper-inputs/inc-scores.csv 
	Rscript code/figure-model-week-target-wis-avgs.R

figures/inc-loc-heatmap.jpg: code/figure-heatmap-locations.R code/load-global-analysis-dates.R
	Rscript code/figure-heatmap-locations.R

figures/long-range.jpg: code/figure-long-term-comparisons.R paper-inputs/inc-calibration.csv paper-inputs/inc-scores.csv 
	Rscript code/figure-long-term-comparisons.R

figures/fig-wis-location.jpg: code/figure-wis_by_location.R paper-inputs/inc-scores.csv
	Rscript code/figure-wis_by_location.R

figures/data-and-forecast.jpg: code/figure-data-and-forecast.R code/load-global-analysis-dates.R
	Rscript code/figure-data-and-forecast.R

figures/fig-model-ranks.jpg: code/figure-model-ranks.R paper-inputs/inc-scores.csv
	Rscript code/figure-model-ranks.R

network-graph: 
	make -Bnd |  ~/Applications/makefile2graph/make2graph | dot -Tpng -o makefile-network-graph.png
