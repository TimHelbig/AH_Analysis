#Input: clean_set_aggregation.R DataFrame
#Output: Boxplot comparison of keystrokes / encounter between provider levels of "Specialists"
specialist_keystroke_compare <- function(final_cle){
  specialist_set <- final_cle[final_cle$specialty_group == "Specialist" & final_cle$time_per_encounter < 20,]
  MDs <- specialist_set[specialist_set$provider_level == "MD",]
  Mids <- specialist_set[specialist_set$provider_level == "MIDLEVEL",]
  MD_Vals <- MDs$time_per_encounter
  Mids_Vals <- Mids$time_per_encounter
  pdf(file="specialist_keystroke_box.pdf", height = 6, width = 7)
  #par(mar=c(5,5,3,1))
  boxplot(keystrokes_per_encounter ~ provider_level, data = specialist_set,
          col = "red", xlab="Provider Level", 
          ylab="Keystrokes / Encounter",
          main="Comparison of Keystrokes / Encounter of \nProvider Levels among 'Specialists'")
  dev.off()
}