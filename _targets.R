library(targets)
library(tarchetypes)
# This _targets.R file defines the {targets} pipeline.
# Run tar_make() to run the pipeline, tar_make(target) to run up to a defined target, and tar_read(target) to view the results.

# Source all functions from the "R" folder
sapply(list.files("R", full.names = TRUE) ,source, .GlobalEnv)



# Set target-specific options such as packages.
tar_option_set(
	packages = c(
		"rio", #For reading various filetypes
		"arrow", #For reading parquet files
		"cli", #For producing command line interface elements such as loading bars
		"zoo", # For rolling windows
		"survival", # For survival analysis including Cox regression
		"magrittr", # To improve readability with pipes
		"haven", # To import Stata .dta
		"readstata13", # To import Stata .dta
		"broom", # To clean regression output
		"epiDisplay", # For data exploration
		"biostat3", # For data exploration
		"lubridate", # To manage dates
		"summarytools", # For data exploration
		"gt", # To create publication-ready tables
		"ggplot2", # To make plots
		"forestplot", # To make forest plots
		"DiagrammeR", # To make flow diagrams
		"DiagrammeRsvg", # To export flow diagrams
		"rsvg", # To export flow diagrams
		"dtplyr", #data.table backend for dplyr
		"qs", #To store targets in qs format for quicker reading and writing
		"tidyverse", # For data management
		"collapse" #For fast data management
	),
	format="qs" #Save all targets in qs format, which should be quicker to read and write than the default RDS
) 



# List of target objects.
list(
	
	# Specifications -------------------------------------------------------------
	
	# Specify outcomes
	tar_target(
		outcome, 
		head(n=99, #TEMPORARILY LIMIT TO SPEED UP RUNTIME
				 c("bisphosphonate",
				 	"fract_composite", 
				 	"calcium_and_vit_d"
				 ))
	),
	
	# Specify exposures
	tar_target(
		exposure,
		head(n=99, #TEMPORARILY LIMIT TO SPEED UP RUNTIME
				 c("pattern",
				 	"cumdays",
				 	"pattern_rt_gap",
				 	"pattern_lagged",
				 	"pattern_itt",
				 	"cumdose"
				 ))
	),
	
	#Specify analyses
	tar_target(
		analysis,
		head(n=99, #TEMPORARILY LIMIT TO SPEED UP RUNTIME
		c("main", #Main analysis
			"sens_all_follow_up", #Sensitivity analysis: Follow up not limited to 1 year
			"sens_shorter_windows", #Sensitivity analysis: Use shorter windows to define exposure
			"sens_no_asthma", #Sensitivity analysis: Exclude people who ever have a record for asthma
			"sens_all_ages") #Sensitivity analysis: don't restrict to only those aged 66+ when they cross the risk threshold for the first time
		)
	),
	
	# Specify models for regression
	tar_target(
		model, 
		head(n=99, #TEMPORARILY LIMIT TO SPEED UP RUNTIME
		c("crude" = "",
			"adjusted_comorb" = "+ agegroup + sex + carstairs + asthma + copd + rheumatoid_arthritis",
			"adjusted_comorb_severity" = "+ agegroup + sex + carstairs + asthma + copd + rheumatoid_arthritis + severity",
			"adjusted_age_sex_carstairs" = "+ agegroup + sex + carstairs",
			"adjusted_comorb_lifestyle" = "+ agegroup + sex + carstairs + asthma + copd + rheumatoid_arthritis + smokstatus + bmi_cat + alc",
			"adjusted_comorb_cumdose" = "+ agegroup + sex + carstairs + asthma + copd + rheumatoid_arthritis + cumdose_cont"
			))
	),
	
	# Specify variables for which rates should be calculated
	tar_target(
		rate_vars, 
		c(exposure, "severity", "asthma", "alc")
	),
	
	
	
	
	# Data paths -----------------------------------------------------------------
	# Main Cohort and static vars
	tar_file(path_main_cohort, "dummy_data/dummy_main_cohort.csv"),
	# Time-updated vars
	tar_file(path_severity, "dummy_data/dummy_severity.csv"),
	tar_file(path_asthma, "dummy_data/asthma.csv"),
	tar_file(path_copd, "dummy_data/copd.csv"),
	tar_file(path_rheumatoid_arthritis, "dummy_data/rheumatoid_arthritis.csv"),
	tar_file(path_prescriptions, "dummy_data/dummy_prescriptions_ped.csv"),
	tar_file(path_alc, "dummy_data/alc.csv"),
	tar_file(path_antabuse, "dummy_data/antabuse.csv"),
	# Outcomes
	tar_file(path_fract_all, "dummy_data/fract_all.csv"),
	tar_file(path_fract_hip, "dummy_data/fract_hip.csv"),
	tar_file(path_fract_spine, "dummy_data/fract_spine.csv"),
	tar_file(path_fract_wrist, "dummy_data/fract_wrist.csv"),
	tar_file(path_fract_pelvis, "dummy_data/fract_pelvis.csv"),
	tar_file(path_bisphosphonate, "dummy_data/bisphosphonate.csv"),
	tar_file(path_calcium_and_vit_d, "dummy_data/calcium_and_vit_d.csv"),
	# Other
	tar_file(path_ped_reference, "ped_reference.csv"),

	
	
	# Data management ------------------------------------------------------------
	tar_target(
		main_cohort, 
		read_csv(path_main_cohort) %>% 
			select(patid, indexdate, enddate, sex, realyob, pracid, eth5, smokstatus, bmi, carstairs)
		),
	tar_target(
		prescriptions_ped, read_csv(path_prescriptions)
	),
	tar_target(
		prescriptions_risk_threshold,
		create_prescriptions_risk_threshold(prescriptions_ped)
	),
	tar_target(
		prescriptions_expanded,
		create_prescriptions_expanded(prescriptions_ped)
	),
	tar_target(
		cohort_eczema, 
		create_cohort_eczema(main_cohort, prescriptions_risk_threshold,
												 prescriptions_expanded, path_fract_all,
												 path_fract_hip, path_fract_spine, path_fract_wrist,
												 path_fract_pelvis, path_alc, path_antabuse, path_smoking, 
												 path_asthma, path_copd, path_rheumatoid_arthritis,
												 path_severity, path_bisphosphonate, path_calcium_and_vit_d)
	),
	tar_target(
		cohort_steroids,
		create_cohort_steroids(cohort_eczema, outcome, analysis),
		pattern = cross(outcome, analysis),
		iteration = "list"
		), 
	
	
	
	# Analysis -------------------------------------------------------------------
	tar_target(
		results_regression, 
		analysis_regression(cohort_steroids, model, exposure),
		pattern = cross(cohort_steroids, model, exposure)
		),
	tar_target(
		results_rates, 
		analysis_rates(datasets=cohort_steroids, rate_vars=rate_vars),
		pattern = cross(cohort_steroids, rate_vars)
		),
	tar_target(
		baseline_characteristics_cohort_eczema, 
		analysis_baseline_characteristics_cohort_eczema(cohort_eczema)
		),
	tar_target(
		baseline_characteristics_cohort_steroids, 
		analysis_baseline_characteristics_cohort_steroids(cohort_steroids[[1]])
	),
	tar_target(
		exposed_counts,
		report_exposed_counts(cohort_steroids, exposure),
		pattern = cross(cohort_steroids, exposure)
	),
	tar_target(
		participant_flow,
		map(cohort_steroids, magrittr::extract, c("counts", "outcome", "analysis")) %>%
			map_df(~ .x$counts %>% mutate(outcome=.x$outcome, analysis=.x$analysis))
	),
	
	
	# Redacted outputs --------------------------------------------------------------------
	tar_target(
		results_rates_redacted,
		results_rates %>% 
			mutate(across(c(pyears, n, event, pyears_unexposed), ~if_else(.x>=10, .x, NA_real_)))
	),
	tar_file(results_rates_redacted_file, write_and_return_path(results_rates_redacted)),
	
	tar_target( #No redaction needed
		results_regression_redacted,
		results_regression
	),
	tar_file(results_regression_redacted_file, write_and_return_path(results_regression_redacted)),
	
	tar_target(
		exposed_counts_redacted,
		exposed_counts %>% 
			mutate(across(c(Freq), ~if_else(.x>=10, .x, NA_integer_)))
	),
	tar_file(exposed_counts_redacted_file, write_and_return_path(exposed_counts_redacted)),
	
	tar_target(
		baseline_characteristics_cohort_eczema_redacted,
		baseline_characteristics_cohort_eczema %>% 
			mutate(across(c(all_eczema), ~if_else(.x>=10, .x, NA_real_))),
	),
	tar_file(baseline_characteristics_cohort_eczema_redacted_file, write_and_return_path(baseline_characteristics_cohort_eczema_redacted)),
	
	tar_target(
		baseline_characteristics_cohort_steroids_redacted,
		baseline_characteristics_cohort_steroids %>% 
			mutate(across(c(intermittent, continuous, all), ~if_else(.x>=10, .x, NA_real_)))
	),
	tar_file(baseline_characteristics_cohort_steroids_redacted_file, write_and_return_path(baseline_characteristics_cohort_steroids_redacted)),
	
	tar_target(
		participant_flow_redacted,
		participant_flow %>% 
			mutate(n=as.double(n), pyrs=as.double(pyrs)) %>% 
			mutate(across(c(n, pyrs), ~if_else(.x>=10, .x, NA_real_)))
	),
	tar_file(participant_flow_redacted_file, write_and_return_path(participant_flow_redacted)),
	
	tar_file(exposure_file, write_lines_and_return_path(exposure)),
	tar_file(outcome_file, write_lines_and_return_path(outcome)),
	tar_file(analysis_file, write_lines_and_return_path(analysis)),
	tar_file(model_file, write_lines_and_return_path(model)),
	tar_file(model_names_file, write_lines_and_return_path(names(model)))
)
