#Input: Provider File
#Output: Histogram of practice size focused on smaller practices
provider_histogram_2 <- function(provider_file){
  providers <- read.table(provider_file, header=TRUE, fill=TRUE)
  providers$unique_id <- providers$practiceid + providers$providerid/1000.00
  
  new <- providers[,1:2]
  new$providerid <- 1
  practice_sum <- aggregate(cbind(providerid) ~ practiceid, sum, data = new)
  practice_sum_small <- practice_sum[practice_sum$providerid < 40,]
  
  pdf(file="provider_hist_2.pdf", height = 4, width = 6)
  hist(practice_sum_small$providerid, breaks = 20, main="# of Providers per Practice (Smaller Practice Focus)", ylab="# of Practices", xlab="# of Providers", col = "red")
  dev.off()
}