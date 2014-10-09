##Input: clean_set_aggregation.R DataFrame
#Output: Scatterplot of EMR Time / Encounter with keystrokes / Encounter
keystroke_anal <- function(fin_cle){
  fin_cle <- fin_cle[fin_cle$time_per_encounter < 20,]
  pdf(file="keystroke_anal.pdf", height = 6, width = 8)
  with(fin_cle, plot(keystrokes_per_encounter, time_per_encounter, xlab="Keystrokes / Encounter",
                         ylab="EMR Time (minutes) / Encounter",
                     main="Linear Regression of Keystrokes / Encounter and  EMR Time / Encounter"))
  model <- lm(time_per_encounter ~ keystrokes_per_encounter, fin_cle)
  abline(model, lwd=2, col="red")
  dev.off()
}