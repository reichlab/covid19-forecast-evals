# make paper-inputs
paper-inputs: figures/data-and-forecast.jpg figures/pi-coverage.jpg figures/model-target-week-wis-avgs.jpg figures/inc-loc-heatmap.jpg figures/fig-by-horizon-week.jpg figures/fig-wis-location.jpg figures/fig-model-ranks.jpg figures/calibration_plot_diagonal.jpg paper-inputs/table-overall-performance.csv

# processes scores after updates to anomaly dates, eligibility changes or new scores
paper-inputs/inc-scores.csv: paper-inputs/model-eligibility-inc.csv code/load-global-analysis-dates.R code/get_forecasts_covidhubutils.R
	Rscript code/get_forecasts_covidhubutils.R

# update model eligibility
paper-inputs/model-eligibility-inc.csv: code/determine-model-eligibility-inc.R code/load-global-analysis-dates.R code/unit_timezero_forecast_complete.R
	Rscript code/determine-model-eligibility-inc.R

# update table for model ranking
paper-inputs/table-overall-performance.csv: paper-inputs/inc-scores.csv code/Table-PI_relative_WIS.R code/Table-PI_relative_WIS_phase.R
	Rscript code/Table-PI_relative_WIS.R
	Rscript code/Table-PI_relative_WIS_phase.R

# update table for sensitivity analysis 2
paper-inputs/sensitivity_table2_update2.csv: paper-inputs/inc-scores.csv 
	Rscript code/Table-PI_relative_WIS-sensitivityAnalysis.R

	
# update calibration scores, after score_forecasts transition, this may not be needed anymore.
# paper-inputs/inc-calibration.csv: paper-inputs/model-eligibility-inc.csv code/get-calibration-scores-inc.R code/load-global-analysis-dates.R
#	Rscript code/get-calibration-scores-inc.R

# individual figures
figures/pi-coverage.jpg: code/figure-calibration.R paper-inputs/inc-scores.csv
	Rscript code/figure-calibration.R

## this script builds 3 figures: model-target-week-wis-avgs, week-model-target-fig4, overall-wis-boxplot
figures/model-target-week-wis-avgs.jpg: code/figure-model-week-target-wis-avgs.R paper-inputs/inc-scores.csv paper-inputs/table-overall-performance.csv
	Rscript code/figure-model-week-target-wis-avgs.R

figures/inc-loc-heatmap.jpg: code/figure-heatmap-locations.R code/load-global-analysis-dates.R paper-inputs/heatmap_data.csv
	Rscript code/figure-heatmap-locations.R

figures/fig-by-horizon-week.jpg: code/figure-horizon-comparison.R paper-inputs/inc-scores.csv code/load-global-analysis-dates.R
	Rscript code/figure-horizon-comparison.R

figures/data-and-forecast.jpg: code/figure-data-and-forecast.R code/load-global-analysis-dates.R
	Rscript code/figure-data-and-forecast.R

figures/fig-model-ranks.jpg: code/figure-model-ranks.R paper-inputs/inc-scores.csv
	Rscript code/figure-model-ranks.R

figures/fig-wis-location.jpg: code/figure-wis_by_location.R paper-inputs/inc-scores.csv code/load-global-analysis-dates.R paper-inputs/table-overall-performance.csv
	Rscript code/figure-wis_by_location.R

figures/calibration_plot_diagonal.jpg: code/figure-calibration_plot_diagonal.R paper-inputs/inc-scores.csv code/load-global-analysis-dates.R
	Rscript code/figure-calibration_plot_diagonal.R

network-graph: 
	make -Bnd |  ~/Applications/makefile2graph/make2graph | dot -Tpng -o makefile-network-graph.png
