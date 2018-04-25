R2ComputeDF<-function(df){
  library(dplyr)
  df = as.data.frame(df)
  IPC_pair = df %>% select(IPC_zone,Actual)
  TA_pair = df %>% select(TA,Actual)
  cluster_pair = df %>% select(cluster,Actual)

  # make pairs then apply R2Compute
  IPCR2 = R2Compute(IPC_pair)
  TAR2 = R2Compute(TA_pair)
  
  clusterR2 = R2Compute(cluster_pair)

  
  
   R2list<- list(IPCR2,TAR2,clusterR2)  
  return(R2list)
}