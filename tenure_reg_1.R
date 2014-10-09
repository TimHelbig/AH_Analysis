##Input: clean_set_aggregation.R DataFrame
#Ouput: Regression of Emr time / encounter and tenure_time  
tenure_reg_1 <- function(fin_cle){
  ten_clean <- na.omit(fin_cle)
  ten_clean <- ten_clean[ten_clean$time_per_encounter < 20,]
  pdf(file="tenure_reg_1.pdf", height = 6, width = 8)
  with(ten_clean, plot(tenure_emr, time_per_encounter, xlab="EMR Tenure (Months)",
                       ylab="EMR Time (minutes) / Encounter"))
  model <- lm(time_per_encounter ~ tenure_emr, ten_clean)
  abline(model, lwd=2, col="red")
  dev.off()
}  