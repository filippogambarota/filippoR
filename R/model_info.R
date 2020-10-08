model_info<-function(model){
  summary<-summary(model)$fixed

  summary_model<-data.frame(max_Rhat=max(summary[,5]),
                            min_Bulk_ESS=min(summary[,6]),
                            min_Tail_ESS=min(summary[,7]),
                            stringsAsFactors = F)
  return(summary_model)
}
