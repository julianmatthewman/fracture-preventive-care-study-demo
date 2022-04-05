#' Get baseline characteristics.
#' @description Calculate descriptive statistics at baseline.
#' @param cohort_eczema_one A dataframe containing start-stop observations for all people with eczema for the main analysis.
#'
#' @return A dataframes with baseline characteristics.
#' @export
#'
#' @examples
analysis_baseline_characteristics_cohort_eczema <- function(cohort_eczema) {
	library(lubridate)
	library(tidyverse)
	library(collapse)

	#Make variable if participant ever experiences outcome (in entire follow up)
	#Replaced group_by, ungroup and mutate with "f" faster versions from collapse package
	#Replaced any(x==1) with faster anyv(x,1) from collapse package
	flat <- cohort_eczema %>% 
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
	
#Make dataset including only the row at indexdate
flat <- flat %>% fsubset(tstart == indexdate)


#Numeric variables
numvars <- flat %>%
	summarise(n=n(),
						male=sum(as.numeric(sex[sex==1])),
						age_median=median(age),
						age_IQR_lower=quantile(age, 0.25, na.rm = TRUE),
						age_IQR_upper=quantile(age, 0.75, na.rm = TRUE)) %>%
	t()


#Categorical variables
catvars <- map_dfc(names(flat)[map_lgl(flat, is.factor)],
									 ~flat %>%
									 	group_by(!!sym(.x)) %>%
									 	summarise(n=n()) %>%
									 	pivot_wider(names_from = !!sym(.x),
									 							values_from = n,
									 							names_prefix = .x)) %>%
	t()


#Join numvars and catvars
baseline_characteristics_eczema <- cbind(rbind(numvars, catvars)) %>%
	as.data.frame() %>%
	rename(all_eczema=V1) %>%
	rownames_to_column() %>%
	mutate(all_eczema_percent=all_eczema/all_eczema[rowname=="n"])

baseline_characteristics_eczema

}

