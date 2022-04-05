#' Define measures for glucocorticoid use 
#' @details Define various measures for glucocorticoid prescriptions including if a prescription crosses a risk threshold of a predefined cumulative prednisolone equivalent dose (PED) using a 6 month rolling window
#' @param prescriptions_ped A dataframe containing patient IDs, event dates, and other information on corticosteroid prescriptions
#' @return The prescriptions dataset enhanced with additional variables on cumulative doses and the risk threshold.
create_prescriptions_risk_threshold <- function(prescriptions_ped) {
	prescriptions_ped %>% 
		group_by(patid) %>%
		mutate(
			peakdose=cummax(ped),
			ped_whole_duration=ped*duration,
			rollmeanped=rollapply(ped,
														width=row_number() - findInterval(start - 180, start),
														FUN=mean,
														align='right'),
			rollsumped=rollapply(ped_whole_duration,
													 width=row_number() - findInterval(start - 180, start),
													 FUN=sum,
													 align='right'),
			riskthreshold=ifelse(rollsumped>=450 & start >= indexdate, 1, 0),
			riskthreshold_gap_stop=if(any(riskthreshold==1)) start[which.max(riskthreshold==1)] else NA,
			riskthreshold_gap_start=if(any(riskthreshold==1)) start[which.max(start>=(riskthreshold_gap_stop-180) & start<=riskthreshold_gap_stop)] else NA,
			riskthreshold_gap=as.numeric(riskthreshold_gap_stop-riskthreshold_gap_start),
			pattern_rt_gap=factor(ifelse(riskthreshold_gap>90, "intermittent", ifelse(riskthreshold_gap<=90, "continuous", NA)), levels=c("intermittent", "continuous"))
		) %>% 
		ungroup()
}

