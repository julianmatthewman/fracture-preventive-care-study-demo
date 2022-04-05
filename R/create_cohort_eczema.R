#' Create Eczema Cohort.
#' @description Make a start-stop dataset from the cohort and the various files of events. See: Therneau T, Crowson C, Atkinson E. Using Time Dependent Covariates and Time Dependent Coefficients in the Cox Model.

#' @param path_main_cohort Path to file.
#' @param path_prescriptions Path to file.
#' @param path_cprdfract Path to file.
#' @param path_hesfract Path to file.
#' @param path_alc Path to file.
#' @param path_asthma Path to file.
#' @param path_ethn Path to file.
#' @param path_severity Path to file.
#'
#' @return A dataframe containing start-stop observations for all people with eczema.

create_cohort_eczema <- function(main_cohort, prescriptions_risk_threshold,
																 prescriptions_expanded, path_fract_all,
																 path_fract_hip, path_fract_spine, path_fract_wrist,
																 path_fract_pelvis, path_alc, path_antabuse, path_smoking, 
																 path_asthma, path_copd, path_rheumatoid_arthritis,
																 path_severity, path_bisphosphonate, path_calcium_and_vit_d) {

library(arrow)
library(rio)
library(haven)
library(survival)
library(lubridate)
library(tidyverse)
	
# Read data ----------------------------------------------------

#Import other data (and only keep IDs present in the main cohort)
fract_all <- read_csv(path_fract_all) %>%
	filter(patid %in% main_cohort$patid) %>% 
	collect()
fract_hip <- read_csv(path_fract_hip) %>%
	filter(patid %in% main_cohort$patid) %>% 
	collect()
fract_spine <- read_csv(path_fract_spine) %>%
	filter(patid %in% main_cohort$patid) %>% 
	collect()
fract_wrist <- read_csv(path_fract_wrist) %>%
	filter(patid %in% main_cohort$patid) %>% 
	collect()
fract_pelvis <- read_csv(path_fract_pelvis) %>%
	filter(patid %in% main_cohort$patid) %>% 
	collect()
alc <- read_csv(path_alc) %>%
	filter(patid %in% main_cohort$patid) %>% 
	collect()
antabuse <- read_csv(path_antabuse) %>%
	filter(patid %in% main_cohort$patid) %>% 
	collect()
asthma <- read_csv(path_asthma) %>%
	filter(patid %in% main_cohort$patid) %>% 
	collect()
copd <- read_csv(path_copd) %>%
	filter(patid %in% main_cohort$patid) %>% 
	collect()
rheumatoid_arthritis <- read_csv(path_rheumatoid_arthritis) %>%
	filter(patid %in% main_cohort$patid) %>%
	collect()
severity <- import(path_severity) %>%
	filter(patid %in% main_cohort$patid)
bisphosphonate <- read_csv(path_bisphosphonate) %>%
	filter(patid %in% main_cohort$patid) %>%
	collect()
calcium_and_vit_d <- read_csv(path_calcium_and_vit_d) %>%
	filter(patid %in% main_cohort$patid) %>% 
	collect()

# Make composite fracture outcome (hip, spine, wrist, and pelvis)
fract_composite <- rbind(fract_hip[c("patid", "eventdate")], 
												 fract_spine[c("patid", "eventdate")],
												 fract_wrist[c("patid", "eventdate")],
												 fract_pelvis[c("patid", "eventdate")])

# Make composite fracture preventive care outcome (bisphosphonates and calcium and vitamin D)
bp_cal_vit_d <- rbind(bisphosphonate, calcium_and_vit_d)

# Join codes for harmful alcohol use and antabuse prescriptions (to flag people as having harmful alcohol use)
alc <- rbind(alc[c("patid", "eventdate")], antabuse[c("patid", "eventdate")])

#Make a table containing the timegroup cutpoints
timegroups <- tibble(
	patid=rep(unique(main_cohort$patid), each=2),
	eventdate=rep(date(c("2006-01-01", "2013-01-01")), length(unique(main_cohort$patid)))
)	 

#Make a table containing the agegroup cutpoints
agecuts <- c(40, 50, 66, 80)
agegroups <- main_cohort[rep(seq_len(nrow(main_cohort)), each = length(agecuts)), c("patid", "realyob")]
agegroups$eventdate <- agegroups$realyob + agecuts #The eventdate is the date at the agecut
agegroups$eventdate <- ymd(agegroups$eventdate, truncated = 2L)


# Create a start-stop dataset using the tmerge function ------------------------

tmerged <- tmerge(main_cohort, main_cohort, id=patid, tstart = indexdate-(100*365), tstop = enddate) #Therneau: "The first call sets the time range for each subject to be from 0 (default) to last follow-up. If a later call tried to add an event outside that range, at time = -2 say, that addition would be ignored." In our case we want to capture everything before the end of the study, since we will set our follow up window later, so here we set tstart to 100 years before the indexdate.
tmerged <- tmerge(tmerged, prescriptions_risk_threshold, id=patid,  
									 #rollgap=tdc(start, rollgap),
									 #rollmeanped=tdc(start,rollmeanped),
									 rollsumped=tdc(start, rollsumped),
									 cumdose_cont=cumtdc(start, ped*duration, 0),
									 cumdays_cont=cumtdc(start, duration, 0),
									 riskthreshold=tdc(start, riskthreshold, 0))
tmerged <- tmerge(tmerged, prescriptions_expanded %>% mutate(ped=ifelse(is.na(ped), 0, ped)), id=patid, 
									active=tdc(start, active, "not active"),
									daily_ped=tdc(start, ped, 0))
tmerged <- tmerge(tmerged, asthma, id=patid, asthma=tdc(eventdate))
tmerged <- tmerge(tmerged, copd, id=patid, copd=tdc(eventdate))
tmerged <- tmerge(tmerged, rheumatoid_arthritis, id=patid, rheumatoid_arthritis=tdc(eventdate))
tmerged <- tmerge(tmerged, alc, id=patid, alc=tdc(eventdate))
tmerged <- tmerge(tmerged, severity, id=patid, severity=tdc(date, modsevere, 0))
tmerged <- tmerge(tmerged, timegroups, id=patid, timegroup=cumtdc(eventdate))
tmerged <- tmerge(tmerged, agegroups, id=patid, agegroup=cumtdc(eventdate))
tmerged <- tmerge(tmerged, bisphosphonate, id=patid, bisphosphonate=event(eventdate))
tmerged <- tmerge(tmerged, calcium_and_vit_d, id=patid, calcium_and_vit_d=event(eventdate))
tmerged <- tmerge(tmerged, bp_cal_vit_d, id=patid, bp_cal_vit_d=event(eventdate))
tmerged <- tmerge(tmerged, fract_all, id=patid, fract_any=event(eventdate))
tmerged <- tmerge(tmerged, fract_hip, id=patid, fract_hip=event(eventdate))
tmerged <- tmerge(tmerged, fract_spine, id=patid, fract_spine=event(eventdate))
tmerged <- tmerge(tmerged, fract_wrist, id=patid, fract_wrist=event(eventdate))
tmerged <- tmerge(tmerged, fract_pelvis, id=patid, fract_pelvis=event(eventdate))
tmerged <- tmerge(tmerged, fract_composite, id=patid, fract_composite=event(eventdate))

tmerged <- left_join(tmerged, prescriptions_risk_threshold[!duplicated(prescriptions_risk_threshold$patid), c("patid", "riskthreshold_gap", "pattern_rt_gap")])


# Categorise variables and make factors ----------------------------------------------------

data <- tmerged %>% 
	mutate(
		cumdose=case_when(cumdose_cont == 0 ~ "0 g",
											cumdose_cont < 450 ~ "1 to 449 mg",
											cumdose_cont < 900 ~ "450 to 899 mg",
											cumdose_cont < 1500 ~ "900 to 1500 mg",
											cumdose_cont >= 1500 ~ ">1500 mg"),
		cumdose=factor(cumdose, levels = c("0 g", 
																			 "1 to 449 mg", 
																			 "450 to 899 mg", 
																			 "900 to 1500 mg", 
																			 ">1500 mg")),
		cumdays=case_when(cumdays_cont < 90 ~ "few",
											cumdays_cont >= 90 ~ "many"),
		cumdays=factor(cumdays, levels = c("few", "many")),
		dob=ymd(realyob, truncated = 2L), #Assume 1st January as Birthday
		age=as.numeric(tstart-dob)/365.25,
		bmi_cat=case_when(bmi < 18.5 ~ "underweight",
											bmi < 25 ~ "normal",
											bmi < 30 ~ "overweight",
											bmi >= 30 ~ "obese"),
		bmi_cat=factor(bmi_cat, levels = c("underweight", 
																			 "normal", 
																			 "overweight", 
																			 "obese")),
		sex=factor(sex),
		eth5=factor(eth5),
		smokstatus=factor(smokstatus),
		alc=factor(alc),
		asthma=factor(asthma),
		copd=factor(copd),
		rheumatoid_arthritis=factor(rheumatoid_arthritis),
		severity=factor(severity),
		carstairs=factor(carstairs),
		timegroup=factor(timegroup, levels = c(0, 1, 2),labels = c("1997-2005", "2006-20012", "2013-2020")),
		agegroup=factor(agegroup, levels = c(0, 1, 2, 3, 4), labels = c("18-39", "40-49", "50-65", "66-79", "80+"))
		)

data

}
