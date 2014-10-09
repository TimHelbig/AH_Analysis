#Input: clean_set_aggregation.R DataFrame
#Output: Barchart of processed provider specializations and levels
provider_specialization_barplot_post <- function(final_data){
  counts <- table(final_clean$provider_level, final_clean$specialty_group)
  pdf(file="provider_spec_post.pdf", height = 4, width = 6)
  barplot(counts, main="Provider Specialty and Level Post Data Processing", 
          ylab="# Providers w/ Processed Data Available", xlab="Provider Specialty", 
          beside = TRUE, 
          col = c("red", "blue"))
  legend("topright",legend=rownames(counts), col=c("red","blue"), pch = 15)
  dev.off()
}