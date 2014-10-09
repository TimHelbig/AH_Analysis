#Input: clean_set_aggregation.R DataFrame
#Output: Regression of EMR Time / Encounter and Tenure Time (First 6 months)
tenure_reg_2 <- function(fin_cle){
  ten_clean <- na.omit(fin_cle)
  ten_clean <- ten_clean[ten_clean$time_per_encounter < 20,]
  ten_clean_6 <- ten_clean[ten_clean$tenure_emr <= 6,]
  pdf(file="tenure_reg_2.pdf", height = 6, width = 8)
  with(ten_clean_6, plot(tenure_emr, time_per_encounter, xlab="EMR Tenure (Months)",
                       ylab="EMR Time (minutes) / Encounter"))
  model <- lm(time_per_encounter ~ tenure_emr, ten_clean_6)
  abline(model, lwd=2, col="red")
  dev.off()
}  