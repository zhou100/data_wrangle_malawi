FCS_data_model3 =  FCS_data 


FCS_data_model2<-cbind(predict_df$clust_logFCS_ipczone_predict_m2,predict_df$clust_logFCS_TA_predict_m2,predict_df$clust_logFCS_clust_predict_m2,predict_df$clust_logFCS,logFCS_IPC$clust_logFCS_predict_ipc)
colnames(FCS_data_model2)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")



FCS_data_model1<-cbind(predict_df$clust_logFCS_ipczone_predict_m1,predict_df$clust_logFCS_TA_predict_m1,predict_df$clust_logFCS_clust_predict_m1,predict_df$clust_logFCS,logFCS_IPC$clust_logFCS_predict_ipc)
colnames(FCS_data_model1)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")

FCS_List<-list(FCS_data_model1,FCS_data_model2,FCS_data_model3)

HDDS_data_model3 =  HDDS_data 


HDDS_data_model2<-cbind(predict_df$clust_HDDS_ipczone_predict_m2,predict_df$clust_HDDS_TA_predict_m2,predict_df$clust_HDDS_clust_predict_m2,predict_df$clust_HDDS,HDDS_IPC$clust_HDDS_predict_ipc)
colnames(HDDS_data_model2)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")



HDDS_data_model1<-cbind(predict_df$clust_HDDS_ipczone_predict_m1,predict_df$clust_HDDS_TA_predict_m1,predict_df$clust_HDDS_clust_predict_m1,predict_df$clust_HDDS,HDDS_IPC$clust_HDDS_predict_ipc)
colnames(HDDS_data_model1)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")

HDDS_List<-list(HDDS_data_model1,HDDS_data_model2,HDDS_data_model3)


RCSI_data_model3 =  RCSI_data 


RCSI_data_model2<-cbind(predict_df$clust_RCSI_ipczone_predict_m2,predict_df$clust_RCSI_TA_predict_m2,predict_df$clust_RCSI_clust_predict_m2,predict_df$clust_RCSI,RCSI_IPC$clust_RCSI_predict_ipc)
colnames(RCSI_data_model2)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")



RCSI_data_model1<-cbind(predict_df$clust_RCSI_ipczone_predict_m1,predict_df$clust_RCSI_TA_predict_m1,predict_df$clust_RCSI_clust_predict_m1,predict_df$clust_RCSI,RCSI_IPC$clust_RCSI_predict_ipc)
colnames(RCSI_data_model1)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")

RCSI_List<-list(RCSI_data_model1,RCSI_data_model2,RCSI_data_model3)
source("function/R2Compute.R")
source("function/R2ComputeDF.R")


r2mat_RCSI<- sapply(RCSI_List,R2ComputeDF)
r2mat_HDDS<- sapply(HDDS_List,R2ComputeDF)
r2mat_FCS<- sapply(FCS_List,R2ComputeDF)

colnames(r2mat_RCSI) = c("m1","m2","m3")
colnames(r2mat_HDDS) = c("m1","m2","m3")
colnames(r2mat_FCS) = c("m1","m2","m3")

rownames(r2mat_RCSI) = c("IPC_zone","TA","cluster","IPC_value_only")
rownames(r2mat_HDDS) = c("IPC_zone","TA","cluster","IPC_value_only")
rownames(r2mat_FCS) = c("IPC_zone","TA","cluster","IPC_value_only")


r2matlist<-list(r2mat_FCS,r2mat_HDDS,r2mat_RCSI)

r2matlist<-lapply(r2matlist,as.data.frame)
r2matlist<-lapply(r2matlist,function(x){tibble::rownames_to_column(x,var = "level")})

r2matlist<-lapply(r2matlist,function(x){tidyr::gather(data= x,key=model,2:4,value = rsquares)})

r2matlist[[1]]["measure"] ="FCS"
r2matlist[[2]]["measure"] ="HDDS"
r2matlist[[3]]["measure"] ="RCSI"

r2 = bind_rows(r2matlist[[1]],r2matlist[[2]],r2matlist[[3]])
colnames(r2)[2] = "Model"
colnames(r2)[3] = "RSquared"
colnames(r2)[4] = "Measures"


r2$level[r2$level=="IPC_zone"]<-"IPC Zone"
r2$level[r2$level=="cluster"]<-"Cluster"
r2$Model[r2$Model=="m1"]<-"Class 1 data"
r2$Model[r2$Model=="m2"]<-"Class 1 data + Class 2 data"
r2$Model[r2$Model=="m3"]<-"Class 1 data + Class 2 data + Class 3 data"

ord <- c("IPC_value_only","IPC Zone","TA","Cluster")
r2$level <- factor(r2$level,levels=ord)

r2$RSquared <-as.numeric(r2$RSquared)
