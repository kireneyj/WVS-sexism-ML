## Model Validation (note that this time we are using test set)
# packages

# dataset

# Models -------------------

# 1. OLS 


# 2. Lasso/ridge 
# (with lambda values trained from the cross-validation run on the training set)


# 3. Random forest
# saveRDS(test_pred_trf, "randomforest_prediction.rds") -> read in this data and compare


# 4. Neural network 

f_glance <- function(model){
  outcome <- broom::glance(model)
  return(outcome %>% list())
}


tibble(
  model_name = c("ols_mod1", "ols_mod2", "ols_mod3", "ols_mod4", "ols_mod5", "ols_mod6", "ols_mod7"),
  model = list(ols_mod1, ols_mod2, ols_mod3, ols_mod4, ols_mod5, ols_mod6, ols_mod7)) %>% 
  mutate(
    glance = map(model, f_glance)
  ) %>% 
  select(glance)



library(broom)
glance(ols_mod1)
