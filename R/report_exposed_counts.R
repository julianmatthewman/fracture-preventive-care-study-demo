#' Get counts of exposed and unexposed in each rolling window.
#'
#' @param cohort_steroids 
#' @param exposure 
#'
#' @return A list of dataframes
#' @export
#'
#' @examples
report_exposed_counts <- function(cohort_steroids, exposure) {
	print(exposure)
				
	
	cohort_steroids$data %>% 
				group_by(patid, rolling_window) %>% 
				filter(!duplicated(rolling_window)) %>% 
				ungroup() %>% 
				dplyr::select(rolling_window, all_of(exposure)) %>% 
				table() %>% 
				as.data.frame() %>% 
				mutate(rolling_window=as.numeric(rolling_window),
							 outcome=cohort_steroids$outcome, 
							 analysis=cohort_steroids$analysis,
							 exposure=exposure)
}



