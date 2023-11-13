##Read in libraries
library(tidyverse)
library(tidymodels)
library(patchwork)

#Read the data in
trainCsv <- read_csv("train.csv")

testCsv <- read_csv("test.csv")

trainCsv

nStores <- max(trainCsv$store)
nItems <- max(trainCsv$item)
nItems
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- trainCsv %>%
    filter(store==s, item==i)
    storeItemTest <- testCsv %>%
    filter(store==s, item==i)
    
    ## Fit storeItem models here

  
    ## Predict storeItem sales
    
    ## Save storeItem predictions
 
       if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}


storeItemTrain <- trainCsv %>% 
  filter(store==1,item==1) 
plot1 <- storeItemTrain %>%
  pull(sales) %>%
  forecast::ggAcf()
plot1

storeItemTrain <- trainCsv %>% 
  filter(store==2,item==2) 
plot2 <- storeItemTrain %>%
  pull(sales) %>%
  forecast::ggAcf()
plot2

storeItemTrain <- trainCsv %>% 
  filter(store==8,item==50) 
plot3 <- storeItemTrain %>%
  pull(sales) %>%
  forecast::ggAcf()
plot3

storeItemTrain <- trainCsv %>% 
  filter(store==1,item==4) 
plot4 <- storeItemTrain %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)
plot4

(plot1 + plot2) /(plot3 + plot4)


