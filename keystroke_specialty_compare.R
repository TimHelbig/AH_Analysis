##Input: clean_set_aggregation.R DataFrame
#Ouput: Boxplot of keystrokes / encounter among provder speecializations
keystroke_specialty_compare <- function(fin_cle){
  MD_Set <- fin_cle[fin_cle$provider_level == "MD" & fin_cle$time_per_encounter < 20,]
  pdf(file="keystroke_specialty_compare.pdf", height = 6, width = 6)
  boxplot(keystrokes_per_encounter ~ specialty_group, data = MD_Set,
          col = "red", xlab="Provider Specialty", 
          ylab="Keystrokes / Encounter",
          main="Keystroke / Encounter Comparison \nAmong Specialty Group MDs")
  dev.off()
}