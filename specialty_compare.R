#Input: clean_set_aggregation.R DataFrame
#Output: EMR Entry Time / Encounter Boxplots among Specialization 
specialty_compare <- function(fin_cle){
  MD_Set <- fin_cle[fin_cle$provider_level == "MD" & fin_cle$time_per_encounter < 20,]
  pdf(file="specialty_compare.pdf", height = 6, width = 6)
  boxplot(time_per_encounter ~ specialty_group, data = MD_Set,
          col = "red", xlab="Provider Specialty", 
          ylab="EMR Time (minutes) / Encounter",
          main="EMR Entry Time / Encounter Comparison \nAmong Specialty Group MDs")
  dev.off()
}