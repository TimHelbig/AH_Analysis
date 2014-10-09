#Input: clean_set_aggregation.R DataFrame
#Output: Histogram of EMR Time per encounter
Time_per_enc_log_box <- function(final_clean_df){
  pdf(file="time_per_enc_hist.pdf", height = 6, width = 5)
  hist(final_clean_df$time_per_encounter, col="red", breaks=20,
          xlab="EMR Time (minutes) / Encounter", 
          main="EMR Time / Encounter \nAmong All Processed  Providers")
  dev.off()
}