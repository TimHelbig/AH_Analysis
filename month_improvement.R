##Input: clean_set_aggregation.R DataFrame
#Ouput: Linegraph charting the EMR Time / Encounter over observation months for new providers (Tenure_Time < 6 Months)
month_improvement <- function(emr_cle){
  emr_cle_month <- na.omit(emr_cle)
  emr_cle_month <- emr_cle_month[emr_cle_month$tenure_emr <= 6,]
  emr_cle_month$time_per_encounter <- emr_cle_month$emr_aggregate_time/emr_cle_month$encounters
  emr_cle_month <- emr_cle_month[emr_cle_month$time_per_encounter < 20,]
  keep_id <- table(emr_cle_month$unique_id)
  keep_vals <- keep_id[keep_id == 3]
  unique_id_list <- rownames(keep_vals)
  emr_cle_month <- emr_cle_month[emr_cle_month$unique_id %in% unique_id_list,]
  month_4 <- subset(emr_cle_month, obs_month == 4, c(unique_id, time_per_encounter))
  names(month_4)[2] <- "time_per_encounter_4"
  month_5 <- subset(emr_cle_month, obs_month == 5, c(unique_id, time_per_encounter))
  names(month_5)[2] <- "time_per_encounter_5"
  month_6 <- subset(emr_cle_month, obs_month == 6, c(unique_id, time_per_encounter))
  names(month_6)[2] <- "time_per_encounter_6"
  m1 <- merge(month_4, month_5, by = "unique_id")
  merged <- merge(m1, month_6, by = "unique_id")
  pdf(file="month_improvement.pdf", height = 8, width = 7)
  with(merged, plot(rep(4,44), merged[,2], xlim = c(3,7), ylim = c(0,10),
                    xlab="Observation Month", 
                    ylab="EMR Time (minutes) / Encounter",
                    main="EMR Time / Encounter Change over Observation Months\nAmong Providers w/ <= 6 Months EMR Tenure"))
  with(merged, points(rep(5,44), merged[,3]))
  with(merged, points(rep(6,44), merged[,4]))
  segments(rep(4,44), merged[,2], rep(5,44), merged[,3])
  segments(rep(5,44), merged[,3], rep(6,44), merged[,4])
  dev.off()
}