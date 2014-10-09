#Input: DF from emr_clean.R and "providers.txt" file
#Output: Dataframe with each row containing a unique provider that has been processed and contains a complete dataset
clean_set_aggregation <- function(clean_df, providers_file){
  clean_aggregate <- aggregate(cbind(keystrokes,mouseclicks,emr_aggregate_time,encounters,rvus) ~ 
                                    unique_id, 
                                  sum, 
                                  data = clean_df)
  providers <- read.table(providers_file, header=TRUE, fill=TRUE)
  providers$unique_id <- providers$practiceid + providers$providerid/1000.00
  providers <- providers[,3:6]
  
  final_dataset <- merge(providers, clean_aggregate, by="unique_id")
  final_dataset$time_per_encounter <- final_dataset$emr_aggregate_time/final_dataset$encounters
  final_dataset$time_per_rvu <- final_dataset$emr_aggregate_time/final_dataset$rvus
  final_dataset$keystrokes_per_encounter <- final_dataset$keystrokes/final_dataset$encounters
  final_dataset$keystrokes_per_rvu <- final_dataset$keystrokes/final_dataset$rvus
  final_dataset$mouseclicks_per_encounter <- final_dataset$mouseclicks/final_dataset$encounters
  final_dataset$mouseclicks_per_rvu <- final_dataset$mouseclicks/final_dataset$rvus
  final_dataset$keystrokes_per_time <- final_dataset$keystrokes/final_dataset$emr_aggregate_time
  final_dataset
  final_dataset<-final_dataset[final_dataset$time_per_encounter < 20 & final_dataset$time_per_encounter > 0.01,]
}