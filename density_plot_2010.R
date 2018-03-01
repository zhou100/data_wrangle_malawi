rm(list=ls())

library("reshape2")
library("ggplot2")
library("zoo")


csv_list <- list.files(path="data/all_predict", 
                       pattern = "csv$",
                       full.names=TRUE)
dfnames<-csv_list

# remove irregulars in the file names, 
pattern<-c("data/all_predict/","clust_",".csv")
for (i in 1:length(pattern)) {
  dfnames <- gsub(pattern[i],"", dfnames)
}
dfnames


list2env(
  lapply(setNames(csv_list, make.names(dfnames)), 
         function(i){read.csv(i)}), envir = .GlobalEnv)

predict_df<-cbind(logFCS_predict_m3[1],RCSI_predict_m3[1],HDDS_predict_m3[1])

for (i in 1:length(dfnames)){
  # exclude unneeded cols  
  temp <- get(dfnames[i])
  predict_df<-cbind(predict_df,temp[2])
}




malawi<- read.csv("data/cluster_fs.csv")

logFCS_IPC <- read.csv("data/all_ipc/logFCS_predict_CLUST_IPC.csv")
HDDS_IPC <- read.csv("data/all_ipc/HDDS_predict_CLUST_IPC.csv")
RCSI_IPC <- read.csv("data/all_ipc/RCSI_predict_CLUST_IPC.csv")


# predict_df$clust_logFCS_clust_predict_m3

# plot_density <- function(data, measure, arg3=2, ...) {
#   newVar <- sin(arg1) + sin(arg2)  # do Some Useful Stuff
#   newVar / arg3   # return value 
# }



FCS_data<-cbind(predict_df$clust_logFCS_ipczone_predict_m3,predict_df$clust_logFCS_TA_predict_m3,predict_df$clust_logFCS_clust_predict_m3,predict_df$clust_logFCS,logFCS_IPC$clust_logFCS_predict_ipc)
colnames(FCS_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_fcs<- melt(FCS_data,na.rm=TRUE)
colnames(long_fcs)<-c("no","level","logFCS")
plot_long<- long_fcs[long_fcs$level != "IPC_value_only",]
plot_long_ipc<- long_fcs[long_fcs$level == "IPC_value_only",]


ggplot(as.data.frame(plot_long),aes(x=logFCS,color=level)) + geom_density(alpha=0.1) +xlim(2.8, 4.9) +
  geom_vline(xintercept=log(28),linetype=2) + geom_vline(xintercept=log(42),linetype=2)+
  stat_density(data=plot_long_ipc,aes(x=logFCS, y=..scaled..*3.5,color=level)) + scale_y_continuous(sec.axis = sec_axis(~.*28, name = " density (IPC value only) ")) +
  theme_bw()


ggsave("FCS_2013_full.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 2000, limitsize = TRUE)


# with only the ipc and actual 
FCS_data<-cbind(predict_df$clust_logFCS_ipczone_predict_m3,predict_df$clust_logFCS_TA_predict_m3,predict_df$clust_logFCS_clust_predict_m3,predict_df$clust_logFCS,logFCS_IPC$clust_logFCS_predict_ipc)
colnames(FCS_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_fcs<- melt(FCS_data,na.rm=TRUE)
colnames(long_fcs)<-c("no","level","logFCS")
plot_actual<- long_fcs[long_fcs$level == "Actual",]
plot_long_ipc<- long_fcs[long_fcs$level == "IPC_value_only",]

ggplot(as.data.frame(plot_actual),aes(x=logFCS, color=level)) + geom_density(alpha=0.1) +xlim(2.8, 4.9) +
  geom_vline(xintercept=log(28),linetype=2) + geom_vline(xintercept=log(42),linetype=2)+
  stat_density(data=plot_long_ipc,aes(x=logFCS, y=..scaled..*3.5,color=level)) + scale_y_continuous(sec.axis = sec_axis(~.*28, name = "density (IPC value only) ")) +
  theme_bw()

ggsave("FCS_2013_ipc_vs_actual.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 2000, limitsize = TRUE)

