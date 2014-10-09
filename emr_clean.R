##Input: 4 tab delimited text files of Athena EMR data
##Output: DF with rows of provider data split by observation  month (4, 5, 6)
emr_clean <- function(providers_file, work_file, emr_touch_file, emr_time_file){
  providers <- read.table(providers_file, header=TRUE, fill=TRUE)
  providers$unique_id <- providers$practiceid + providers$providerid/1000.00
  providers <- providers[,3:6]
  provider_ID_list_df <- data.frame(providers$unique_id)
  
  emr_touches <- read.table(emr_touch_file, header=TRUE, fill=TRUE)
  emr_touches$unique_id <- emr_touches$practiceid + emr_touches$providerid/1000.00
  emr_touches <- emr_touches[,3:6]
  merge_1 <- merge(providers, emr_touches, by="unique_id")
  
  emr_time <- read.table(emr_time_file, header=TRUE, fill=TRUE)
  emr_time$unique_id <- emr_time$practiceid + emr_time$providerid/1000.00
  emr_time <- emr_time[,3:6]
  emr_time_aggregate <- aggregate(cbind(value) ~ 
                                    unique_id + obs_month, 
                                  sum, 
                                  data = emr_time)
  colnames(emr_time_aggregate)[3] <- "emr_aggregate_time"
  merge_2 <- merge(merge_1, emr_time_aggregate, by=c("unique_id", "obs_month"))
  
  work <- read.table(work_file, header=TRUE, fill=TRUE)
  work$unique_id <- work$practiceid + work$providerid/1000.00
  work <- work[,3:6]
  work_split <- split(work, list(work$variable))
  enc_sub <- subset(work_split[[1]], select=-c(variable))
  colnames(enc_sub)[2] <- "encounters"
  rvu_sub <- subset(work_split[[2]], select=-c(variable))
  colnames(rvu_sub)[2] <- "rvus"
  work_merge <- merge(enc_sub, rvu_sub, by=c("unique_id", "obs_month"), all = TRUE)
  
  final_merge <- merge(merge_2, work_merge, by=c("unique_id", "obs_month"))
  final_merge <- final_merge[final_merge$rvus >= 0,]
}
