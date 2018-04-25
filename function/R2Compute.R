####################################### #############  
# Goals
#  drop nas,  compute a r2 between acutal and predict 

# Inputs
#   (1) df: a pair of actual (column 1) and predicted value (column 2)
#   
# Outputs
#   (1) a dataframe containing the predicated value of y variable in the training and testing set. 
############# ############# ############# ############# ############# ############# ############# 

library(dplyr)
R2Compute <- function(pair){
  pair = pair %>%
    na.omit() 
    
    r2 = cor(pair[,1],pair[,2])^2
  
  
  return(r2)
  }