####--SERVER DATA--------------------------------------------------------------------------------------------

#
# DATA
#



#### Datasets ####

Rdata <- reactive({
  read_RData()
})

#
# ML data
#

ml_data <- reactive({
  df <- Rdata()
  
  df$y <- df$class == 'GLY'
  df$class <- NULL
  
  train_ratio <- input$model_train_size/100
  train_size <- floor(train_ratio * nrow(df))
  train_split <- sample(1:nrow(df), size = train_size)
  
  ml_data <- list(train = df[train_split,], test = df[-train_split,])
  ml_data  
})

#
# Treatment plan set up
#

treat_plan <- reactive({
  ml_data <- ml_data()
  df_train <- ml_data$train
  my_vars <- names(df_train %>% select(-y))
  designTreatmentsZ(df_train, my_vars, verbose = FALSE)
})

#
# Treatment variables
#

treat_vars <- reactive({
  treat_plan <- treat_plan()
  scoreFrame <- treat_plan$scoreFrame %>% select(varName, origName, code)
  scoreFrame %>% 
    filter(code %in% c("clean", "lev")) %>%
    use_series(varName)  
})

#
# XGB Data
#

RF_data <- reactive({
  ml_data <- ml_data()
  treat_plan <- treat_plan()
  treat_vars <- treat_vars()
  
  df_train <- ml_data$train
  df_test <- ml_data$test
  
  df_train.treat <- prepare(treat_plan, df_train, varRestriction = treat_vars)
  df_test.treat <- prepare(treat_plan, df_test, varRestriction = treat_vars)
  
  dtrain <- xgb.DMatrix(label = df_train$y, data = as.matrix(df_train.treat))  
  dtest <- xgb.DMatrix(label = df_test$y, data = as.matrix(df_test.treat))  
  
  RF_data <- list(train = dtrain, test = dtest)
  RF_data
})

#
# RF model and validation
#

RF_model_and_validation <- reactive({
  
  ml_data <- ml_data()
  RF_data <- RF_data()
  
  y_test <- ml_data$test$y
  RF_train <- RF_data$train
  RF_test <- RF_data$test
  
  model_iterations <- input$model_iterations
  model_depth <- input$model_depth
  model_rate <- input$model_rate
  model_gamma <- input$model_gamma
  model_weight <- input$model_weight
  model_subsample <- input$model_subsample
  model_colsample <- input$model_colsample
  

  
  list(model = new_model, validation_errors = validation_errors)
})

#
# RF results
#

RF_results <- reactive({
  
  ml_data <- ml_data()
  RF_model_and_validation <- RF_model_and_validation()
  RF_data <- RF_data()
  
  model <- RF_model_and_validation$model
  validation_errors <- RF_model_and_validation$validation_errors
  
  # Validation results
  RF_test <- RF_data$test
  ml_test <- ml_data$test
  ml_train <- ml_data$train
  y_test <- ml_test$y
  
  model_prediction <- predict(model, RF_test)
  
  error_validation <- min(validation_errors)
  accuracy_validation <- 1 - error_validation

})


