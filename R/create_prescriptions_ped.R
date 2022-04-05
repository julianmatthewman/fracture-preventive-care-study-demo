#' Calculate prednisolone equivalent dose (PED) for glucocorticoid prescriptions
#' @description Includes algorithm for imputation of missing values.
#' @param path_prescriptions a dataframe containing prescriptions for glucocorticoids
#' @param path_age_sex_gp .dta file containing patient IDs with age, sex and GP practice
#' @param ped_reference .csv file containing drug substance names and a conversion factor to calculate prednisolone equivalent doses
#' @return the prescription file enhanced with PED and start and stop dates

create_prescriptions_ped <- function(prescriptions, path_ped_reference, imputation_condition) {
	library(haven)
	library(zoo)
	library(tidyverse)
	library(lubridate)
	
	# Read PED reference table ---------------------------------------------------------------
	prednisolone_equivalent_doses <- read_csv(path_ped_reference)
	
	
	
	# Only keep prescriptions that occur after the indexdate ------------------

	temp1 <- prescriptions %>% filter(eventdate>=indexdate)
	
	# This sets the start of the covariate assessment window for the risk threshold and cumulative dose to the date a participant joined the eczema cohort.
	# If we want to instead set the start of the covariate assessment window to the earliest possible record, we need to consider the following:
	# 	1. Therneau: "Time dependent covariates that occur before the start of a subject’s follow-up interval or during a gap 
	# 	in time do not generate a new time point, but they do set the value of that covariate for future times."
	#		I.e. if a participant has a prescription of >450 mg PED before the indexdate, their risk threshold would be set to 1.
	#		2. Do we want to consider glucocorticoid prescriptions that were before the eczema diagnosis when these are most likely not prescribed for eczema?
	# So if we want to use all available information, we could for example set riskthreshold=ifelse(rollsumped>=450 & start>=indexdate, 1, 0)
	
	
	
	
	# Clean Data --------------------------------------------------------------

	#If the dosage text contains an amount in MG, daily dose will wrongfully contain that number, instead of the amount of doses per day
		temp1 <- temp1 %>% 
			mutate(daily_dose=ifelse(str_detect(dosage_text, "MG|ML"),
														 NA,
														 daily_dose))
	
		
		
	# Identify substance strength ---------------------------------------------
	
	temp1 <- temp1 %>% mutate(strength_num=str_extract(substancestrength, "\\d+(\\.\\d+)?"))
	temp1 <- temp1 %>% mutate(strength_label=str_replace(substancestrength, "\\d+(\\.\\d+)?", ""))
	temp1 <- temp1 %>% mutate(strength_in_mg=as.numeric(strength_num))
	temp1 <- temp1 %>% mutate(strength_in_mg=ifelse(str_detect(strength_label, "micro"), strength_in_mg/1000, strength_in_mg))
	
	

	# Impute quantity ---------------------------------------------------------

	temp1 <- temp1 %>% 
		group_by(patid) %>% 
		mutate(impute=median(qty, na.rm = TRUE),
					 qty=ifelse(is.na(qty), impute, qty), #Take median of a patients prescription quantity
					 qty=ifelse(is.na(qty), median(temp1$qty, na.rm = TRUE), qty)) %>% #Else take the median of all prescription quantities
		ungroup()
	
	
	# Impute daily_dose -------------------------------------------------------

		# 1. first imputes in same patient, same dose and qty
		temp1 <- temp1 %>% 
			group_by(patid, qty, strength_in_mg) %>% 
			mutate(impute=median(daily_dose, na.rm = TRUE),
						 daily_dose=ifelse(eval(parse(text=imputation_condition)), impute, daily_dose)) %>% 
			ungroup()
		
		# 2. then same patient, same dose and qty group (qty above and below 42)
		temp1 <- temp1 %>% 
			mutate(qty_grp=ifelse(qty>42, 1, 2)) %>% 
			group_by(patid, qty_grp, strength_in_mg) %>% 
			mutate(impute=median(daily_dose, na.rm = TRUE),
						 daily_dose=ifelse(eval(parse(text=imputation_condition)), impute, daily_dose)) %>% 
			ungroup()
		
		# 3. then same patient with same dose
		temp1 <- temp1 %>% 
			group_by(patid, strength_in_mg) %>% 
			mutate(impute=median(daily_dose, na.rm = TRUE),
						 daily_dose=ifelse(eval(parse(text=imputation_condition)), impute, daily_dose)) %>% 
			ungroup()
		
		# 4. then same age group, sex, dose and quantity
		temp1 <- temp1 %>% 
			group_by(age_grp, sex, strength_in_mg, qty) %>% 
			mutate(impute=median(daily_dose, na.rm = TRUE),
						 daily_dose=ifelse(eval(parse(text=imputation_condition)), impute, daily_dose)) %>% 
			ungroup()
		
		# 5. then same age group, sex, dose and quantity group
		temp1 <- temp1 %>% 
			group_by(age_grp, sex, strength_in_mg, qty_grp) %>% 
			mutate(impute=median(daily_dose, na.rm = TRUE),
						 daily_dose=ifelse(eval(parse(text=imputation_condition)), impute, daily_dose)) %>% 
			ungroup()
		
		
	
	# Make duration and prednisolone equivalent dose -----------------------------------------------------------
	
	temp2 <- temp1 %>%
			mutate(duration=qty/daily_dose,
						 daily_ped = strength_in_mg * daily_dose,
						 enddate = eventdate + duration)
	
		
	
	# Drop implausible values --------------------------------------------
	
	temp3 <- temp2 %>%
			filter(daily_ped>0 & daily_ped<100) %>% 
			rename(ped=daily_ped, start=eventdate, end=enddate) %>% 
			select(patid, ped, duration, start, end, sex, realyob, indexdate) %>% 
			arrange(patid, start)
	
	temp3
	
	}




# # CHECKS #######################################################################
# 
# # Check dosage texts ------------------------------------------------------
# 
# #Make list of most common dosage texts
# most_common <- prescriptions %>% 
# 	group_by(dosage_text) %>%
# 	mutate(freq=n()) %>% 
# 	arrange(desc(freq)) %>% 
# 	select(dosage_text, daily_dose) %>% 
# 	unique() %>% 
# 	head(500)
# most_common #Looks good
# 
# #Make list of most common dosage texts that contain ML or MG
# contains_mg <- prescriptions %>% 
# 	filter(str_detect(dosage_text, "MG|ML")) %>% 
# 	group_by(dosage_text) %>%
# 	mutate(freq=n()) %>% 
# 	arrange(desc(freq)) %>% 
# 	select(dosage_text, daily_dose, freq) %>% 
# 	unique() %>% 
# 	head(500)
# contains_mg #For all of these daily_dose should be changed to 0
# 
# #Make list of most common dosage texts that contain ML or MG and TABLETS
# contains_mg_and_tablet <- prescriptions %>% 
# 	filter(str_detect(dosage_text, "MG|ML") & str_detect(dosage_text, "TABLET")) %>% 
# 	group_by(dosage_text) %>%
# 	mutate(freq=n()) %>% 
# 	arrange(desc(freq)) %>% 
# 	select(dosage_text, daily_dose, freq) %>% 
# 	unique() %>% 
# 	head(500)
# contains_mg_and_tablet #For simplicity also set these to 0
# 
# #What proportion of prescriptions do the 500 most common dosage_texts capture
# nrow(prescriptions[prescriptions$dosage_text %in% most_common$dosage_text,])/nrow(prescriptions) #over 90%
# 
# 
# # Check for implausible values --------------------------------------------
# 
# #Check quantiles of PED
# quantile(temp2$daily_ped[temp2$daily_ped>0], na.rm=TRUE, probs = c(0.25, 0.5, 0.75, 0.999)) #99.9% of prescriptions have a daily PED of <100
