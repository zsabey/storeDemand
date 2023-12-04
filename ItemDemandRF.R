##Read in libraries
library(tidyverse)
library(tidymodels)
library(embed)
library(discrim)


#Read the data in
trainCsv <- read_csv("train.csv")

testCsv <- read_csv("test.csv")

storeItemTrain <- trainCsv %>% 
  filter(store==8,item==20)


##Recipe
my_recipe <- recipe(sales ~ date, storeItemTrain) %>%
  step_date(date, features="doy") %>%
  step_date(date, features = "month") %>%
  step_date(date, features = "dow") %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(sales))
  #step_range(date_doy, min=0, max=pi) %>%
  #step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) #%>%
  #step_lag(lag = 365)
prepped <- prep(my_recipe)  
bake(prepped, new_data = NULL)


##CV
boost_model <- boost_tree(tree_depth=tune(),
                          trees=tune(),
                          learn_rate=tune()) %>%
  set_engine("lightgbm") %>% #or "xgboost" but lightgbm is faster
  set_mode("classification")

## Create a workflow with model & recipe
boost_wf <- workflow() %>%
  add_recipe(boost_recipe) %>%
  add_model(boost_model)



boost_tuneGrid <- grid_regular(tree_depth(),
                               trees(),
                               learn_rate(),
                               levels=5)
  
  
  
  ## Set up grid of tuning values
  


## Set up K-fold CV
folds <- vfold_cv(storeItemTrain, v = 3, repeats=1)

## Run the CV
CV_results <- rf_workflow %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(smape)) #Or leave metrics NULL

## Find best tuning parameters
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="smape") %>%
  ggplot(data=., aes(x=mtry, y=min_n, color=factor(mtry))) +
  geom_line()

collect_metrics(CV_results)

CV_results
## Find Best Tuning Parameters
cv_metric <- collect_metrics(CV_results) %>%
  filter(mtry ==3, min_n == 21) %>%
  pull(mean)

cv_metric


     