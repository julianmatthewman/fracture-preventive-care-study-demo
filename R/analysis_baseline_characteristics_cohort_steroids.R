#' Get baseline characteristics.
#' @description Calculate descriptive statistics at baseline.
#' @param cohort_steroids_one A list containing the data for the main analysis.
#'
#' @return A dataframes with baseline characteristics.
#' @export
#'
#' @examples
analysis_baseline_characteristics_cohort_steroids <- function(cohort_steroids_one) {
	library(lubridate)
	library(tidyverse)
	library(collapse)
	
	#Make variable if participant ever experiences outcome (in entire follow up)
	#Replaced group_by, ungroup and mutate with "f" faster versions from collapse package
	#Replaced any(x==1) with faster anyv(x,1) from collapse package
	flat_steroids <- cohort_steroids_one$data %>% 
		fgroup_by(patid) %>% 
		fmutate(fract_any_ever=ifelse(anyv(fract_any,1), "ever", "never"),
						fract_composite_ever=ifelse(anyv(fract_composite,1), "ever", "never"),
						fract_hip_ever=ifelse(anyv(fract_hip,1), "ever", "never"),
						bisphosphonate_ever=ifelse(anyv(bisphosphonate,1), "ever", "never")) %>% 
		fungroup() %>% 
		fmutate(fract_any_ever=factor(fract_any_ever, levels = c("never", "ever")),
						fract_composite_ever=factor(fract_composite_ever, levels = c("never", "ever")),
						fract_hip_ever=factor(fract_hip_ever, levels = c("never", "ever")),
						bisphosphonate_ever=factor(bisphosphonate_ever, levels = c("never", "ever")))
	
	#Make dataset with only first row per participant
	flat_steroids <- flat_steroids %>% filter(!duplicated(patid))
	
	
	#Numeric variables
	numvars_steroids <- flat_steroids %>%
		group_by(pattern) %>% 
		summarise(n=n(),
							male=sum(as.numeric(sex[sex==1])),
							age_median=median(age),
							age_IQR_lower=quantile(age, 0.25, na.rm = TRUE),
							age_IQR_upper=quantile(age, 0.75, na.rm = TRUE)) %>%
		select(-pattern) %>% 
		t()
	
	#Make character vector containing all names of factor variables, except for "pattern"
	catvar_names <- names(flat_steroids)[map_lgl(flat_steroids, is.factor)]
	catvar_names <- catvar_names[!catvar_names=="pattern"]
	
	catvars_steroids <- map_dfc(catvar_names,
															~flat_steroids %>%
																group_by(pattern, !!sym(.x)) %>%
																summarise(n=n(), .groups = "drop") %>%
																pivot_wider(names_from = !!sym(.x),
																						values_from = n,
																						names_prefix = .x) %>% 
																column_to_rownames("pattern")) %>% 
		select(!starts_with("pattern")) %>% 
		t()
	
	#Join numvars and catvars
	baseline_characteristics_steroids <- rbind(numvars_steroids, catvars_steroids) %>%
		as.data.frame() %>%
		rownames_to_column() %>%
		mutate(all=intermittent+continuous,
					 intermittent_percent=intermittent/intermittent[rowname=="n"],
					 continuous_percent=continuous/continuous[rowname=="n"],
					 all_percent=all/all[rowname=="n"]) %>% 
		select(rowname, intermittent, intermittent_percent, continuous, continuous_percent, all, all_percent)
	
	baseline_characteristics_steroids
	
	
}

