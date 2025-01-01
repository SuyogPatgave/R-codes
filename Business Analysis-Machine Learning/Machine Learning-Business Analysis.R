library(tidymodels)
library(vip)
library(rpart.plot)

levels(telecom_df$canceled_service)

#Data Resampling

set.seed(345)

telecom_split <- 
  initial_split(telecom_df, prop = 0.75, strata = canceled_service)

telecom_training <- 
  telecom_split %>% 
  training()

telecom_test <- 
  telecom_split %>% 
  testing()

#Feature Engineering

telecom_recipe<- recipe(canceled_service~., data= telecom_training)%>%
  step_YeoJohnson(all_numeric(), -all_outcomes())%>%
  step_normalize(all_numeric(), -all_outcomes())%>%
  step_dummy(all_nominal(), -all_outcomes())

telecom_recipe %>%
  prep(training= telecom_training)%>%
  bake(new_data= NULL)


# Logistic Regression -----------------------------------------------------

#specifying the model
logistic_model <- logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode('classification')
#creating workflow
telecom_wf<- 
  workflow()%>%
  add_model(logistic_model)%>%
  add_recipe(telecom_recipe)
#fitting the model to training data
telecom_logistic_fit<- telecom_wf%>%
  fit(data= telecom_training)

#extracting the trained model and understanding important and influenctial variables
telecom_trained_model<- telecom_logistic_fit %>%
  extract_fit_parsnip()
vip(telecom_trained_model)

# Logistic Regression Performance Evaluation

prediction_categories<- predict(telecom_logistic_fit, new_data= telecom_test)
#prediction_categories

prediction_probabilities<- predict(telecom_logistic_fit, new_data= telecom_test, type='prob')
#prediction_probabilities

#we will now combime the predictions along with their probabilities (decimal values)

telecom_pred_result<- telecom_test %>%
  select(canceled_service)%>%
  bind_cols(prediction_categories)%>%
  bind_cols(prediction_probabilities)
telecom_pred_result


#confusion_matrix
conf_mat(telecom_pred_result, 
         truth= canceled_service,
         estimate= .pred_class)
#accuracy
accuracy(telecom_pred_result,
         truth = canceled_service, 
         estimate = .pred_class)
#sensitivity 
sens(telecom_pred_result,
     truth = canceled_service, 
     estimate = .pred_class)
#specificity
specificity(telecom_pred_result,
            truth= canceled_service,
            estimate= .pred_class)
#roc_auc 
roc_auc(telecom_pred_result,
        truth = canceled_service, 
        estimate = .pred_yes)

#roc diagram
roc_curve(telecom_pred_result, 
          truth= canceled_service,
          estimate= .pred_yes)%>%
  autoplot(roc_auc)


# Decision Tree -----------------------------------------------------------

#adding folds for cross fold validation for hyperparameter tuning
set.seed(290)
telecom_folds<- vfold_cv(telecom_training, v=5)
#specifying models with three types of hyperparameters
# cost complexity, tree depth and min_n
telecom_tree_model<- decision_tree(cost_complexity = tune(),
                                   tree_depth= tune(),
                                   min_n= tune())%>%
  set_engine('rpart')%>%
  set_mode('classification')
#creating workflow
telecom_tree_workflow<- workflow()%>%
  add_model(telecom_tree_model)%>%
  add_recipe(telecom_recipe)

#getting optimal values for hyperparameter tuning
telecom_tree_grid<- grid_regular(extract_parameter_set_dials(telecom_tree_model), levels = 2)
telecom_tree_grid
#best values for hyperparameter tuning with respect to roc_auc
set.seed(290)
telecom_tree_tuning<- telecom_tree_workflow %>% 
  tune_grid(resamples= telecom_folds, grid= telecom_tree_grid)
telecom_tree_tuning %>% show_best('roc_auc')
#opting the best values
best_telecom_tree<- telecom_tree_tuning %>%
  select_best(metric = 'roc_auc')
best_telecom_tree

#final telecom tree workflow
final_telecom_tree_workflow<- telecom_tree_workflow%>%
  finalize_workflow(best_telecom_tree)
#fitting the model to understand important variables
telecom_tree_wk_fit<- final_telecom_tree_workflow%>%
  fit(data= telecom_training)
#Model 2/Decision Trees VIP
telecom_tree_fit<- telecom_tree_wk_fit%>%
  extract_fit_parsnip()
vip(telecom_tree_fit)

#collecting metrics and roc curve
telecom_tree_last_fit<- final_telecom_tree_workflow%>%
  last_fit(telecom_split)
telecom_tree_last_fit%>% collect_metrics()

#roc curve
telecom_tree_last_fit%>%
  collect_predictions()%>%
  roc_curve(truth = canceled_service, estimate= .pred_yes)%>%
  autoplot()

telecom_tree_predictions <- telecom_tree_last_fit %>% collect_predictions()

conf_mat(telecom_tree_predictions, truth = canceled_service, estimate = .pred_class)


# Random Forest -----------------------------------------------------------

#forming model with hyperparameters
forest_model<- rand_forest(mtry= tune(),
                           trees= tune(),
                           min_n= tune())%>%
  set_engine('ranger', importance= 'impurity') %>%
  set_mode('classification')
#setting workflow
forest_workflow<- workflow()%>%
  add_model(forest_model)%>%
  add_recipe(telecom_recipe)
#values for hyperparameters
set.seed(290)
#in this case the recipe dataset table contains 21 predictors 
#hence, here we have chosen minimum predictors =4 and max=20 
#creating 15 random combinations
forest_grid<- grid_random(mtry() %>% range_set(c(4, 20)),
                          trees(),
                          min_n(),
                          size=15)
forest_grid


#using tune_grid for hyperparameter tuning
set.seed(290)

forest_tuning<- forest_workflow %>%
  tune_grid(resample= telecom_folds, grid= forest_grid)
#find the best w.r.t roc_auc metrics and choosing the optimal
forest_tuning %>% show_best('roc_auc')

best_forest<- forest_tuning %>%
  select_best(metric= 'roc_auc')
best_forest

#finalizing the workflow and fitting the model
final_forest_workflow<- forest_workflow %>%
  finalize_workflow(best_forest)

#fitting the model
forest_workflow_fit<- final_forest_workflow %>%
  fit(data= telecom_training)

#Performance Analysis
#knowing the important variables 
forest_fit<- forest_workflow_fit %>%
  extract_fit_parsnip()
vip(forest_fit)

#train and evaluation
forest_last_fit<- final_forest_workflow %>%
  last_fit(telecom_split)
#review of performance
forest_last_fit %>% collect_metrics()

#performance and confusion matrix

forest_predictions<- forest_last_fit %>% collect_predictions()
conf_mat(forest_predictions, truth= canceled_service, estimate= .pred_class)

#roc curve
forest_last_fit %>% collect_predictions() %>%
  roc_curve(truth= canceled_service, estimate= .pred_yes)%>%
  autoplot()
