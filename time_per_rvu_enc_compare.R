#Input: clean_set_aggregation.R DataFrame
#output: Regression of EMR Time / encounter and EMR Time / rvu
time_per_rvu_enc_compare <- function(final_clean_df){
  pdf(file="rvu_enc_compare.pdf", height = 4.5, width = 6)
  with(final_clean_df, plot(time_per_encounter, 
                       time_per_rvu, xlim=c(0,25), 
                       ylim=c(0,20), main = "Provider EMR Time/Encounter vs. EMR Time/RVU",
                       xlab = "EMR time (minutes) / Encounter", ylab="EMR time (minutes) / RVU"))
  model <- lm(time_per_rvu ~ time_per_encounter, final_cle)
  abline(model, lwd=2)
  dev.off()
}