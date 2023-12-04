##Read in libraries
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(tidyverse) 
library(tidymodels) 
library(mosaic) 
library(embed) 
library(forecast) 


#Read the data in
trainCsv <- read_csv("train.csv")

testCsv <- read_csv("test.csv")

storeItemTrain <- trainCsv %>% 
  filter(store==8,item==20)


prophet_recipe <- recipe(sales ~  date, storeItemTrain) %>%
  step_date(date, features="doy") %>%
  step_date(date, features = "month") %>%
  step_date(date, features = "dow") %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(sales)) 

##Splits the data and graphs it
cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE, .legend_show = FALSE)

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))


## Calibrate (i.e. tune) workflow
## Cross-validate to tune model
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))

## Visualize CV results
plot1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = storeItemTrain
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE,.legend_show = FALSE)
plot1

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

# Refit to all data then forecast
prophet_fullfit <- cv_results %>%
  modeltime_refit(data = storeItemTrain)

prophet_preds <- prophet_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=testCsv, by="date") %>%
  select(id, sales)
prophet_preds


#Plots the forecast
plot2 <- prophet_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = storeItemTrain) %>%
  plot_modeltime_forecast(.interactive=FALSE, .legend_show = FALSE)

plot2


#Combines the plots into graphs
plotly::subplot(plot1,plot3,plot2,plot4, nrows = 2)

