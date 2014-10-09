#Input: "providers.txt" file
#Output: Histogram of practice size
provider_histogram_1 <- function(provider_file){
  providers <- read.table(provider_file, header=TRUE, fill=TRUE)
  providers$unique_id <- providers$practiceid + providers$providerid/1000.00
  
  new <- providers[,1:2]
  new$providerid <- 1
  practice_sum <- aggregate(cbind(providerid) ~ practiceid, sum, data = new)
  
  pdf(file="provider_hist_1.pdf", height = 4, width = 6)
  hist(practice_sum$providerid, breaks = 20, main="# of Providers per Practice", ylab="# of Practices", xlab="# of Providers", col = "red")
  dev.off()
}