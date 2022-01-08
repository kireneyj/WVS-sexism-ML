## Models -----------------------------------------------------------------------

## packages 
library(glmnet)
library(glmnetUtils)
library(randomForest)
library(rsample)
library(modelr)
library(tidyverse)
library(skimr)

library(ranger)
library(vip)
library(pdp)
library(tree)
library(rpart)
library(onehot)
library(janitor)

library(tensorflow)
use_python("/usr/bin/python")
library(keras)

## dataset 
imputed_cart_dat <- read_rds("imputed_cart_comp.rds") %>% 
  rename(id = .id) %>% 
  select(-.imp) %>% 
  mutate(sex = as_factor(sex))

## data split ---------------------------------
set.seed(7081991)
ss <- sample(1:3,
             size=nrow(imputed_cart_dat),
             replace=TRUE,
             prob=c(0.6,0.2,0.2))
train <- imputed_cart_dat[ss==1,]
test <- imputed_cart_dat[ss==2,]
eda <- imputed_cart_dat[ss==3,]

# for model building, we use the "train" part only
train2 <- train %>% 
  select(-id, -men_more_right_jobs, -men_better_poli_leaders, -men_better_biz_exec, 
         -uni_edu_boys, -women_making_more_problem, -dem_women_same_right_men, 
         -religion)

# scale the dataset 
train_scaled <- train2 %>% 
  mutate(sex = as.numeric(sex)) %>% 
  select(-countryname, -continent, -sub_region, -sexism) %>% 
  scale() %>% 
  as_tibble()

# one-hot encode the dataset 
ohe_train <- train2 %>%
  mutate(continent = as.factor(continent), 
         sub_region = as.factor(sub_region)) %>% 
  select(continent, sub_region) %>% 
  onehot(max_levels = 40) %>%
  predict(as_tibble(train2)) %>%
  as_tibble() %>% 
  clean_names()

# combine the target variable (sexism), scaled dataset, and one-hot encoded dataset
sexism <- train2 %>% pull(sexism)
train3 <- base::cbind(train_scaled, ohe_train) %>% 
  mutate(sexism = sexism)

# get the test dataset ready 
test2 <- test %>% 
  select(-id, -men_more_right_jobs, -men_better_poli_leaders, -men_better_biz_exec, 
         -uni_edu_boys, -women_making_more_problem, -dem_women_same_right_men, 
         -religion)

# scale the dataset 
test_scaled <- test2 %>% 
  mutate(sex = as.numeric(sex)) %>% 
  select(-countryname, -continent, -sub_region, -sexism) %>% 
  scale() %>% 
  as_tibble()

# one-hot encode the dataset 
ohe_test <- test2 %>%
  mutate(continent = as.factor(continent), 
         sub_region = as.factor(sub_region)) %>% 
  select(continent, sub_region) %>% 
  onehot(max_levels = 40) %>%
  predict(as_tibble(test2)) %>%
  as_tibble() %>% 
  clean_names()

# combine the target variable (sexism), scaled dataset, and one-hot encoded dataset
sexism_test <- test2 %>% pull(sexism)
test3 <- base::cbind(test_scaled, ohe_test) %>% 
  mutate(sexism = sexism_test)

## save the scaled, ohe dataset 
saveRDS(train3, "train.rds")
saveRDS(test3, "test.rds")
traintest <- rbind(train3, test3)
saveRDS(traintest, "traintest.rds")
traintest2 <- rbind(train_scaled, test_scaled)
saveRDS(traintest2, "traintest2.rds")
saveRDS(eda, "eda.rds")

# Final outcome: train3 (we use this one!)

## Model BUilding --------------------------------
# 1. OLS ------------------------
## (1) all-inclusive model 
ols_mod1 <- train3 %>% 
  lm(sexism ~ ., 
     data = .)
ols_mod1 %>% summary()

# make predictions using this model
ols_mod1_prediction <- predict(ols_mod1, 
                               newdata = test3)
ols_mod1_prediction <- cbind(test3, ols_mod1_prediction) %>% 
  mutate(error = (sexism - ols_mod1_prediction)^2) #%>% 
  #pull(error) %>% summary() #MSE: 13.732


# saving the results
saveRDS(ols_mod1, "ols_mod1.rds")
saveRDS(ols_mod1_prediction, "ols_mod1_prediction.rds")

## (2) religion/religiosity-related variables 
ols_mod2 <- train3 %>% 
  lm(sexism ~ religion_important + religion_childquality + attendreligion + sciencevsreligion + 
       toomuchsciencelessfaith + religiousperson + godimportant + believeinhell, data = .)

# predictions
ols_mod2_prediction <- predict(ols_mod2, 
                               newdata = test3)
ols_mod2_prediction <- cbind(test3, ols_mod2_prediction) %>% 
  mutate(error = (sexism - ols_mod2_prediction)^2) #%>% 
 # pull(error) %>% summary() # MSE: 17.744

saveRDS(ols_mod2, "ols_mod2.rds")
saveRDS(ols_mod2_prediction, "ols_mod2_prediction.rds")

# religious bigotry model (fundamentalists)
ols_mod3 <- train3 %>% 
  lm(sexism ~ religion_important + religion_childquality + attendreligion + sciencevsreligion + 
       toomuchsciencelessfaith + godimportant + believeinhell + 
       teachallreligion + onlyacceptablereligion + differentreligionmoral + religiousauthoritylaw + 
       neighbor_diffreligion + trust_diffreligion, 
     data = .)
# predictions
ols_mod3_prediction <- predict(ols_mod3, 
                               newdata = test3)
ols_mod3_prediction <- cbind(test3, ols_mod3_prediction) %>% 
  mutate(error = (sexism - ols_mod3_prediction)^2) #%>%
  #pull(error) %>% summary() # MSE: 16.720

saveRDS(ols_mod3, "ols_mod3.rds")
saveRDS(ols_mod3_prediction, "ols_mod3_prediction.rds")

## (3) demographic variables 
names(train3)
# the very basic
ols_mod4 <- train3 %>% 
  lm(sexism ~ sex + education + age + income + ses, 
     data = .)

# make predictions using this model
ols_mod4_prediction <- predict(ols_mod4, 
                               newdata = test3)
ols_mod4_prediction <- cbind(test3, ols_mod4_prediction) %>% 
  mutate(error = (sexism - ols_mod4_prediction)^2) #%>% 
  #pull(error) %>% summary() #MSE: 19.131

saveRDS(ols_mod4, "ols_mod4.rds")
saveRDS(ols_mod4_prediction, "ols_mod4_prediction.rds")

# with continent only
ols_mod5 <- train3 %>% 
  lm(sexism ~ sex + education + age + income + ses + 
       continent_americas + continent_africa + continent_asia + continent_europe + continent_oceania, 
     data = .)
summary(ols_mod5)


# make predictions using this model
ols_mod5_prediction <- predict(ols_mod5, 
                               newdata = test3)
ols_mod5_prediction <- cbind(test3, ols_mod5_prediction) %>% 
  mutate(error = (sexism - ols_mod5_prediction)^2) #%>% 
  #pull(error) %>% summary() #MSE: 16.771

saveRDS(ols_mod5, "ols_mod5.rds")
saveRDS(ols_mod5_prediction, "ols_mod5_prediction.rds")

# with subregions 
ols_mod6 <- train3 %>% 
  lm(sexism ~ sex + education + age + income + ses + 
       sub_region_australia_and_new_zealand + sub_region_caribbean + sub_region_central_america + 
       sub_region_central_asia + sub_region_eastern_africa + sub_region_eastern_africa + 
       sub_region_eastern_asia + sub_region_eastern_europe + sub_region_northern_africa + 
       sub_region_northern_america + sub_region_northern_europe + sub_region_south_eastern_asia + 
       sub_region_south_america + sub_region_southern_africa + sub_region_southern_asia + sub_region_southern_europe, 
     data = .)
summary(ols_mod6)

# make predictions using this model
ols_mod6_prediction <- predict(ols_mod6, 
                               newdata = test3)
ols_mod6_prediction <- cbind(test3, ols_mod6_prediction) %>% 
  mutate(error = (sexism - ols_mod6_prediction)^2) #%>% 
  #pull(error) %>% summary() #MSE: 16.920

saveRDS(ols_mod6, "ols_mod6.rds")
saveRDS(ols_mod6_prediction, "ols_mod6_prediction.rds")

## (4) tradition/conservatism-related variables 
names(train_scaled)
ols_mod7 <- train3 %>% 
  lm(sexism ~ just_sexbeforemarriage + neighbor_unmarriedcouple + neighbor_homosexual + 
       just_homosexuality + traditionimportant + workingmomchildsuffer + divorcejustifiable + 
       parentsproudlifegoal + housewifefulfilling + adventurerisk + behaveproperlyimpt + abortionjustifiable + 
       respectforelder + respectforauthority + obedienceimportant + boss30yrold, 
     data = .)

# make predictions using this model
ols_mod7_prediction <- predict(ols_mod7, 
                               newdata = test3)
ols_mod7_prediction <- cbind(test3, ols_mod7_prediction) %>% 
  mutate(error = (sexism - ols_mod7_prediction)^2) #%>% 
  #pull(error) %>% summary() #MSE: 15.716

saveRDS(ols_mod7_prediction, "ols_mod7_prediction.rds")
saveRDS(ols_mod7, "ols_mod7.rds")

# 2. LASSO/Ridge ----------------
## (1) Lasso 
lambda_grid <- 10^seq(-10, 10, length  = 300)

set.seed(1234)
lasso_cv <- train3 %>%
  cv.glmnet(formula = sexism ~ ., 
            data = ., 
            alpha = 1, 
            nfolds = 10, 
            lambda = lambda_grid)

plot(lasso_cv)

lasso_cv_lambda_min <- lasso_cv$lambda.min # 0.00143612
lasso_cv_lambda_1se <- lasso_cv$lambda.1se # 0.04253658

lasso_min <- glmnetUtils::glmnet(sexism ~ ., 
                                 data = train3, 
                                 alpha = 1, 
                                 lambda = lasso_cv_lambda_min)
lasso_min$beta # not a lot of variables are zeroed out 

lasso_1se <- glmnetUtils::glmnet(sexism ~ ., 
                                 data = train3, 
                                 alpha = 1, 
                                 lambda = lasso_cv_lambda_1se)
lasso_1se$beta

lasso_min_prediction <- predict(lasso_min, 
                                newdata = test3)
lasso_1se_prediction <- predict(lasso_1se, 
                                newdata = test3)

# predictions of the lasso 
lasso_min_pred <- cbind(test3, lasso_min_prediction) %>% 
  mutate(error = (sexism - lasso_min_prediction)^2) #%>% 
 # pull(error) %>% summary() # 13.731

lasso_1se_pred <- cbind(test3, lasso_1se_prediction) %>% 
  mutate(error = (sexism - lasso_1se_prediction)^2) #%>% 
  #pull(error) %>% summary() # 13.801

saveRDS(lasso_min_pred, "lasso_min_pred.rds")
saveRDS(lasso_1se_pred, "lasso_1se_pred.rds")

saveRDS(lasso_cv, "lasso_cv.rds")
saveRDS(lasso_1se, "lasso_1se.rds")
saveRDS(lasso_min, "lasso_min.rds")

## (2) ridge
set.seed(1234)
ridge_cv <- train3 %>%
  cv.glmnet(formula = sexism ~ ., 
            data = ., 
            alpha = 0, 
            nfolds = 10, 
            lambda = lambda_grid)

plot(ridge_cv)

ridge_cv_lambda_min <- ridge_cv$lambda.min # 0.04253658
ridge_cv_lambda_1se <- ridge_cv$lambda.1se # 0.925881

# prediction based on the ridge 
ridge_min <- glmnetUtils::glmnet(sexism ~ ., 
                                 data = train3, 
                                 alpha = 0, 
                                 lambda = ridge_cv_lambda_min)
ridge_min$beta # not a lot of variables are zeroed out 

ridge_1se <- glmnetUtils::glmnet(sexism ~ ., 
                                 data = train3, 
                                 alpha = 0, 
                                 lambda = ridge_cv_lambda_1se)

## Prediction 
ridge_min_prediction <- predict(ridge_min, 
                                newdata = test3)
ridge_1se_prediction <- predict(ridge_1se, 
                                newdata = test3)

## MSE based on ridge predictions --
ridge_min_pred <- cbind(test3, ridge_cv_lambda_min) %>% 
  mutate(error = (sexism - ridge_min_prediction)^2) #%>% 
  #pull(error) %>% summary() # 13.732

ridge_1se_pred <- cbind(test3, ridge_cv_lambda_1se) %>% 
  mutate(error = (sexism - ridge_1se_prediction)^2) #%>% 
  #pull(error) %>% summary() # 13.795

saveRDS(ridge_min_pred, "ridge_min_pred.rds")
saveRDS(ridge_1se_pred, "ridge_1se_pred.rds")

saveRDS(ridge_cv, "ridge_cv.rds")
saveRDS(ridge_1se, "ridge_1se.rds")
saveRDS(ridge_min, "ridge_min.rds")


# 3. Random Forest -------------------------
# helper function
fitRF_wvs <- function(data, mtry){
  return(randomForest(sexism ~ ., 
                      data = data, 
                      ntree = 50, 
                      mtry = mtry,
                      importance = TRUE))
}

model_def <- tibble(mtry = 1:(ncol(train3) - 1))

## fit the random forest model
# NOTE: RANDOM FOREST DOES NOT RUN ON THE DATASET WITH CHARACTER VARIABLES
# RANDOM FOREST TAKES SO LONG - PROBABLY JUST TAKE A RANDOM SAMPLE (E.G., 10% OF THE DATASET) 

## fit the random forest model 
set.seed(1234)
train4 <- sample_frac(train3, size = 0.1, replace = FALSE)

# randomness check: seems that approximately randomly distributed indeed
train4 %>% skim_without_charts()
train3 %>% skim_without_charts()


rf_10fold <- train4 %>%
  crossv_kfold(10, id = "fold") %>%
  crossing(model_def) %>% 
  mutate(
    model_fit = map2(train, mtry, fitRF_wvs))

# obtain the MSE
rf_10fold <- rf_10fold %>% 
  mutate(fold_mse = map2_dbl(model_fit, test, modelr::mse))

# arrange to see the optimal value of mtry 
rf_10fold %>% 
  group_by(mtry) %>% 
  summarize(test_mse = mean(fold_mse)) %>%
  arrange(test_mse)

rf_10fold  %>% 
  group_by(mtry) %>% 
  summarize(test_mse = mean(fold_mse)) %>%
  ggplot(aes(x = mtry, y = test_mse)) +
  geom_line() +
  geom_point() 

## Determine the node size --
model_def_nsize <- tibble(min_nsize = c(10, 15, 20, 25, 30, 35, 
                                        40, 45, 50, 55, 60, 65, 
                                        70, 75, 80))

fit_rf_nsize_1 <- function(data, min_nsize){
  return(randomForest(sexism ~ ., 
                      data = data, 
                      mtry = 24, # fix the mtry at 30, based on previous CV runs 
                      ntree = 50, 
                      nodesize = min_nsize,
                      importance = TRUE))
}


# Perform the 10-fold CV to find the optimal min node size
set.seed(1234)

rf_10fold24 <- train4 %>% 
  crossv_kfold(10, id = "fold") %>%
  # Create 10 folds for each unique value of min node size
  crossing(model_def_nsize) %>%
  # Fit the models and compute the fold MSE 
  mutate(model_fit = map2(train, min_nsize, fit_rf_nsize_1),
         fold_mse = map2_dbl(model_fit, test, modelr::mse))

# Display the results (to see which size of nodes give the least MSE)
rf_10fold24 %>% 
  group_by(min_nsize) %>% 
  summarize(test_mse = mean(fold_mse)) %>%
  arrange(test_mse)
# node size = 25

rf_10fold24 %>% 
  group_by(min_nsize) %>% 
  summarize(test_mse = mean(fold_mse)) %>% 
  ggplot(aes(x = min_nsize, y = test_mse)) + 
  geom_line() + 
  geom_point()

## save the results 
saveRDS(rf_10fold, "randomforest_10.rds")
saveRDS(rf_10fold24, "randomforest_nodes.rds")

test_pred_trf <- tibble(train = train4 %>% list,
                        test = test3 %>% list) %>% 
  mutate(mod_fit = map2(.x = train, .y = 25, .f = fit_rf_nsize_1), # .y = min node size
         pred_values = map2(.x = mod_fit, .y = test, .f = predict)) %>% 
  select(pred_values) %>% 
  unnest(pred_values) 

saveRDS(test_pred_trf, "randomforest_prediction.rds")

vip(rf_10fold)


# variable importance 
rf_wvs = ranger(sexism ~ ., 
                           data = train4, 
                           mtry = 24,
                           importance = "impurity", 
                           splitrule = "variance")
vip(rf_wvs)




## combine with the test set (cbind) and compute MSE 
cbind(test3, test_pred_trf) %>% 
  select(sexism, pred_values) %>% 
  mutate(
    error = (sexism - pred_values)^2
  ) %>% 
  select(sexism, pred_values, error) %>% pull(error) %>% summary(error) 
# MSE: 13.761

### PICTURE




# 4. Neural Network ------------------------
# set the data in needed formats: one-hot encode & scaled
# make sure that it's in matrix format
train4 <- train3 %>% 
  select(-sexism) %>% 
  as.matrix()

test4 <- test3 %>% 
  select(-sexism) %>% 
  as.matrix()

## three hidden layers, each with 64 nodes
model <- keras_model_sequential() %>% 
  layer_dense(units = 64, activation = "relu", 
              # input_shape = number of columns (number of variables)
              input_shape = dim(train4)[[2]]) %>%
  # since these are dense layers, relu can allow dropping of some connections
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>% 
  # this last layer will be the output layer 
  layer_dense(units = 1)

model %>% compile(
  optimizer = "rmsprop", 
  loss = "mse", 
  metrics = c("mse")
)

neuralnet <- model %>% 
  fit(train4, sexism, 
      epochs = 100)

testresults <- model %>% 
  evaluate(test4, sexism_test)

saveRDS(neuralnet, "neuralnet.rds")
saveRDS(testresults, "neuralnet_test.rds")

## Neural Network 2. Three hidden layers, each with 32 nodes, and each with the activation function of “relu” 
model2 <- keras_model_sequential() %>% 
  layer_dense(units = 32, activation = "relu", 
              # input_shape = number of columns (number of variables)
              input_shape = dim(train4)[[2]]) %>%
  # since these are dense layers, relu can allow dropping of some connections
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>% 
  # this last layer will be the output layer 
  layer_dense(units = 1)

model2 %>% compile(
  optimizer = "rmsprop", 
  loss = "mse", 
  metrics = c("mse")
)

neuralnet2 <- model2 %>% 
  fit(train4, sexism, 
      epochs = 100)
plot(neuralnet2)

testresults2 <- model2 %>% 
  evaluate(test4, sexism_test)

saveRDS(neuralnet2, "neuralnet2.rds")
saveRDS(testresults2, "neuralnet_test2.rds")


## Neural network 3. Two hidden layers, each with 64 nodes, and each with the activation function of “relu” 
model3 <- keras_model_sequential() %>% 
  layer_dense(units = 64, activation = "relu", 
              # input_shape = number of columns (number of variables)
              input_shape = dim(train4)[[2]]) %>%
  # since these are dense layers, relu can allow dropping of some connections
  layer_dense(units = 64, activation = "relu") %>%
  # this last layer will be the output layer 
  layer_dense(units = 1)

model3 %>% compile(
  optimizer = "rmsprop", 
  loss = "mse", 
  metrics = c("mse")
)

neuralnet3 <- model3 %>% 
  fit(train4, sexism, 
      epochs = 100)

plot(neuralnet3)

testresults3 <- model3 %>% 
  evaluate(test4, sexism_test)

saveRDS(neuralnet3, "neuralnet3.rds")
saveRDS(testresults3, "neuralnet_test3.rds")
