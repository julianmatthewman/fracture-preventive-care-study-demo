#' Calculate rates.
#' @description Calculate rates, person-years and number of fractures for different variables.
#' @param datasets AA lists of lists containing datasets for different outcomes.
#' @param rate_vars A character vector of variables for which rates should be calculated.
#'
#' @return A dataframe containing results.
#' @export
#'
#' @examples
analysis_rates <- function(datasets, rate_vars) {

results_rates <- pyears(outcome_surv ~ eval(as.symbol(rate_vars)), data = datasets$data, scale = 365.25) %>%
			tidy() %>% 
			mutate(rate=(event/pyears)*1000) %>% 
			cbind(tibble(term=paste0(rate_vars, levels(as.factor(datasets$data[[rate_vars]]))), 
									 group=rate_vars, 
									 outcome=datasets$outcome,
									 analysis=datasets$analysis))

#Get unexposed person years in the same row
results_rates <- results_rates %>% 
	group_by(outcome, group) %>%
	mutate(pyears_unexposed=pyears[1],
				 ratio=1/(pyears[2]/pyears[1]),
				 ratio_text=paste0("1:", round(ratio))) %>%
	ungroup()

results_rates

}
