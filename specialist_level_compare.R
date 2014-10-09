##Input: clean_set_aggregation.R DataFrame
#Output: Boxplot of EMR Time / encounter among levels of specialists
specialist_level_compare <- function(final_cle){
  specialist_set <- final_cle[final_cle$specialty_group == "Specialist",]
  MDs <- specialist_set[specialist_set$provider_level == "MD",]
  Mids <- specialist_set[specialist_set$provider_level == "MIDLEVEL",]
  MD_Vals <- MDs$time_per_encounter
  Mids_Vals <- Mids$time_per_encounter
  pdf(file="specialist_compare_box.pdf", height = 6, width = 5)
  #par(mar=c(5,5,3,1))
  boxplot(time_per_encounter ~ provider_level, data = specialist_set,
          col = "red", xlab="Provider Level", 
          ylab="EMR Time (minutes) / Encounter",
          main="Comparison of EMR Time / Encounter of \nProvider Levels among 'Specialists'")
  dev.off()
}