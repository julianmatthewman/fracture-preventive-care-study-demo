#' Run Cox regression
#' @description Run Cox regression, tidy the output and add names of model, outcome and analysis.
#' @param cohort_steroids A list containing a dataset for a specified outcome and analysis.
#' @param model The model formula.
#' @return A dataframe containing results.

analysis_regression <- function(cohort_steroids, model, exposure) {

		 			coxph(formula(paste("outcome_surv ~", exposure, model)), 
		 						data=cohort_steroids$data, 
		 						cluster=pracid, 
		 						timefix=FALSE) %>% 
		 				tidy(exponentiate=TRUE, conf.int=TRUE) %>%
		 				mutate(outcome=cohort_steroids$outcome, 
		 							 exposure=exposure,
		 							 model=names(model),
		 							 analysis=cohort_steroids$analysis)
		 	
}