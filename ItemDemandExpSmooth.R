##Read in libraries
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(tidyverse)
library(tidymodels)
library(mosaic)
library(embed)


#Read the data in
trainCsv <- read_csv("train.csv")

testCsv <- read_csv("test.csv")

storeItemTrain <- trainCsv %>% 
  filter(store==8,item==20)

##Splits the data and graphs it
cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)

cv_split %>%
 tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE, .legend_show = FALSE)



es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

## Visualize CV results
plot3 <- cv_results %>%
modeltime_forecast(
                   new_data = testing(cv_split),
                   actual_data = storeItemTrain
) %>%
plot_modeltime_forecast(.interactive=TRUE,.legend_show = FALSE)

## Evaluate the accuracy
cv_results %>%
modeltime_accuracy() %>%
table_modeltime_accuracy(
                         .interactive = FALSE
)

# Refit to all data then forecast
es_fullfit <- cv_results %>%
modeltime_refit(data = storeItemTrain)

es_preds <- es_fullfit %>%
modeltime_forecast(h = "3 months") %>%
rename(date=.index, sales=.value) %>%
select(date, sales) %>%
full_join(., y=testCsv, by="date") %>%
select(id, sales)


plot4 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = storeItemTrain) %>%
  plot_modeltime_forecast(.interactive=FALSE, .legend_show = FALSE)

plot3 

plot4

stopCluster(cl)
plotly:: subplot(plot1,plot2,plot3,plot4, nrows = 2)
