#Input: Providers text file, clean_set_aggregation.R DataFrame
#Output: Boxplot comparison of EMR Time / Encounter among different practice sizes
prac_size_analysis <- function(provider_file,fin_cle){
  providers <- read.table(provider_file, header=TRUE, fill=TRUE)
  providers$unique_id <- providers$practiceid + providers$providerid/1000.00
  
  new <- providers[,1:2]
  new$providerid <- 1
  practice_sum <- aggregate(cbind(providerid) ~ practiceid, sum, data = new)
  names(practice_sum)[2] <- "practice_size"
  prac_size_merge <- merge(providers, practice_sum, by="practiceid")
  fin_cle <- merge(fin_cle,prac_size_merge,by="unique_id")
  
  fin_cle$prac_size <- cut(fin_cle$practice_size, breaks=c(0.9,10,100,1000))
  fin_cle <- fin_cle[fin_cle$time_per_encounter < 20,]
  pdf(file="prac_size_analysis.pdf", height = 6, width = 6)
  boxplot(time_per_encounter ~ prac_size, data = fin_cle,
          col = "red", xlab="Practice Size", 
          ylab="EMR Time (minutes) / Encounter",
          main="EMR Entry Time / Encounter Comparison \nAmong Various Practice Sizes")
  dev.off()
}