R2ComputeDF<-function(df){
  library(dplyr)
  df = as.data.frame(df)
  IPC_pair = df %>% select(IPC_zone,Actual)
  TA_pair = df %>% select(TA,Actual)
  cluster_pair = df %>% select(cluster,Actual)
  IPC_value_only_pair = df %>% select(IPC_value_only,Actual)
  
  # make pairs then apply R2Compute
  IPCR2 = R2Compute(IPC_pair)
  TAR2 = R2Compute(TA_pair)
  
  clusterR2 = R2Compute(cluster_pair)
  IPC_value_onlyR2 = R2Compute(IPC_value_only_pair)
  
  
  
   R2list<- list(IPCR2,TAR2,clusterR2,IPC_value_onlyR2)  
  return(R2list)
}