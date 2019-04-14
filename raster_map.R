rm(list=ls())

source("function/linear.R")
source("function/formula.R")
source("function/R2Compute.R")
source("function/R2ComputeDF.R")

library("dplyr")
library("ggplot2")
require(grid)

# malawi <- read.csv("data/malawi_data_new.csv")

# read data
clust_2010 <- read.csv("data/clust_2010.csv")
clust_2013 = read.csv("data/clust_2013.csv")


# levels 
levels<-c("ipczone","TA","clust")

# variables 
weather<-c("L12raincytot","L12day1rain","L12maxdays","floodmax")
access<-c("lag_price","lag_thinn")
asset1 <-c("roof","cells_own")
land<-c("percent_ag","elevation","nutri_reten_constrained")
distance<-c("dist_road","dist_admarc")
demo<-c("hhsize","hh_age","hh_gender","asset")

model3_variables<-c(weather,access,asset1,land,distance,demo)
model2_variables<-c(weather,access,asset1,land,distance)
model1_variables<-c(weather,access,land,distance)

# goal : combine variables at different levels using pastes 
# output: variables lists at different levels, TA_vars, ipczone vars, etc. 

for (level in levels){
  # assign levels of variables group
  group_var_name<-paste(level,"vars",sep="_")
  assign(group_var_name,c())
  
  for(var in model3_variables){
    temp<-paste(level,var,sep = "_")
    new<-append(get(group_var_name),temp)
    assign(group_var_name,new)
  }
}

### 1. Linear/tobit Results 

# Create the formulas using the formula_compose function. 

rcsi_formula<-formula_compose("clust_RCSI",clust_vars)
logFCS_formula<-formula_compose("clust_logFCS",clust_vars)
HDDS_formula<-formula_compose("clust_HDDS",clust_vars)

rcsi_predictions<-linear_fit(rcsi_formula,clust_2010,clust_2013)
logFCS_predictions<-linear_fit(logFCS_formula,clust_2010,clust_2013) 
HDDS_predictions<-linear_fit(HDDS_formula,clust_2010,clust_2013) 

rcsi_pair <- cbind(clust_2013$clust_RCSI, rcsi_predictions)
logFCS_pair <- cbind(clust_2013$clust_logFCS, logFCS_predictions)
HDDS_pair <- cbind(clust_2013$clust_HDDS, HDDS_predictions)

colnames(rcsi_pair) <- c("actual","predict")
colnames(logFCS_pair) <- c("actual","predict")
colnames(HDDS_pair) <- c("actual","predict")


linear_list<- list(rcsi_pair,logFCS_pair,HDDS_pair)


r2list<- sapply(linear_list,R2Compute)
r2list= format(r2list, digits=3, nsmall=2)



map13 = clust_2013 %>% select(case_id,LAT_DD_MOD,LON_DD_MOD )
map13["rcsi"] =  as.data.frame(linear_list[[1]])["predict"]
map13["logfcs"] =  as.data.frame(linear_list[[2]])["predict"]
map13["hdds"] =  as.data.frame(linear_list[[3]])["predict"]


write.csv(map13,"map13.csv",row.names = FALSE)


library(raster)
library(sp)
library(rgdal)

gridmap <- raster("raster2.tif")
gridmap

plot(gridmap)

gridmap <- setMinMax(gridmap)

 


fun <- function(x) { (x[1]+rnorm(1,20,8) )}
ndii <- calc(gridmap, fun)


plot(ndii)
writeRaster(ndii,"40.tiff")
