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




actuals<-cbind(logFCS_predict_m3[1],RCSI_predict_m3[1],HDDS_predict_m3[1])
colnames(actuals)<-c("logFCS_cluster","RCSI_cluster","HDDS_cluster")

pred<-cbind(logFCS_predict_m3[2],RCSI_predict_m3[2],HDDS_predict_m3[2])
colnames(pred)<-c("logFCS_pred","RCSI_pred","HDDS_pred")


#geom_point(x=actuals[1],y=pred[1],aes(x=logFCS_cluster) ) 

d<-cbind(actuals,pred)
ggplot(d, aes(logFCS_cluster, logFCS_pred )) +
  geom_point(shape = 16, size = 3, show.legend = FALSE) +
  theme_minimal() + geom_abline(intercept = 0, slope = 1)+
  scale_color_brewer(palette="Dark2") +annotate(geom = "text", size = 5,x = 4.2, y = 3.5, label = "R squares = 0.591") 

ggsave("logFCS_scatter.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 1000, limitsize = TRUE)


ggplot(d, aes(RCSI_cluster, RCSI_pred )) +
  geom_point(shape = 16, size = 3, show.legend = FALSE) +
  theme_minimal() + geom_abline(intercept = 0, slope = 1)+
  scale_color_brewer(palette="Dark2")+
  annotate(geom = "text", size = 5, x = 11, y = 1.5, label = "R squares = 0.426")
  


ggsave("RCSI_scatter.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 1000, limitsize = TRUE)

ggplot(d, aes(HDDS_cluster, HDDS_pred )) +
  geom_point(shape = 16, size = 3, show.legend = FALSE) +
  theme_minimal() + geom_abline(intercept = 0, slope = 1)+
  scale_color_brewer(palette="Dark2") +
  annotate(geom = "text", size = 5, x = 6.3, y = 4.3, label = "R squares =  0.606")



ggsave("HDDS_scatter.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 1000, limitsize = TRUE)


