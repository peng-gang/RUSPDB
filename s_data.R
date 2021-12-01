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

xgb_data <- reactive({
  ml_data <- ml_data()
  treat_plan <- treat_plan()
  treat_vars <- treat_vars()
  
  df_train <- ml_data$train
  df_test <- ml_data$test
  
  df_train.treat <- prepare(treat_plan, df_train, varRestriction = treat_vars)
  df_test.treat <- prepare(treat_plan, df_test, varRestriction = treat_vars)
  
  dtrain <- xgb.DMatrix(label = df_train$y, data = as.matrix(df_train.treat))  
  dtest <- xgb.DMatrix(label = df_test$y, data = as.matrix(df_test.treat))  
  
  xgb_data <- list(train = dtrain, test = dtest)
  xgb_data
})

#
# XGB model and validation
#

xgb_model_and_validation <- reactive({
  
  ml_data <- ml_data()
  xgb_data <- xgb_data()
  
  y_test <- ml_data$test$y
  xgb_train <- xgb_data$train
  xgb_test <- xgb_data$test
  
  model_iterations <- input$model_iterations
  model_depth <- input$model_depth
  model_rate <- input$model_rate
  model_gamma <- input$model_gamma
  model_weight <- input$model_weight
  model_subsample <- input$model_subsample
  model_colsample <- input$model_colsample
  
  run_model <- function(xgb_model = NULL){
    xgboost(
      xgb_model = xgb_model,
      data = xgb_train, # the data
      verbose = 0,
      nround = 1, # max number of boosting iterations
      max.depth = model_depth, 
      eta = model_rate,
      gamma = model_gamma,
      min_child_weight = model_weight,
      subsample = model_subsample,
      colsample_bytree = model_colsample,
      objective = "binary:logistic",
      eval_metric = "aucpr"
    )  # the objective function    
  }
  
  # Define a vector of validation errors
  validation_errors <- numeric(model_iterations)
  
  # Run initial model
  initial_model <- run_model()
  initial_prediction <- predict(initial_model, xgb_test)
  validation_error <- mean(as.numeric(initial_prediction > 0.5) != y_test)
  validation_errors[1] <- validation_error
  
  # Run the rest of the models
  if(model_iterations>1){
    previous_model <- initial_model
    for(i in 2:model_iterations){
      new_model <- run_model(previous_model)
      new_prediction <- predict(new_model, xgb_test)
      validation_error <- mean(as.numeric(new_prediction > 0.5) != y_test)
      validation_errors[i] <- validation_error
      previous_model <- new_model
    }  
  }
  
  list(model = new_model, validation_errors = validation_errors)
})

#
# XGB results
#

xgb_results <- reactive({
  
  ml_data <- ml_data()
  xgb_model_and_validation <- xgb_model_and_validation()
  xgb_data <- xgb_data()
  
  model <- xgb_model_and_validation$model
  validation_errors <- xgb_model_and_validation$validation_errors
  
  # Validation results
  xgb_test <- xgb_data$test
  ml_test <- ml_data$test
  ml_train <- ml_data$train
  y_test <- ml_test$y
  
  model_prediction <- predict(model, xgb_test)
  
  error_validation <- min(validation_errors)
  accuracy_validation <- 1 - error_validation
  
  # Baseline results
  n_false <- ml_train %>% filter(y == FALSE) %>% nrow()
  n_total <- ml_train %>% nrow()
  n_true <- n_total - n_false
  
  validate(
    need(n_total > 0, "Validation data is missing")
  )
  
  baseline_prediction <- n_true > n_false
  error_baseline <- mean(baseline_prediction != y_test)
  accuracy_baseline <- 1 - error_baseline
  
  #accuracy_baseline <- max(n_false/n_total, n_true/n_total)
  #error_baseline <- 1 - accuracy_baseline
  
  # Training results
  df <- model$evaluation_log
  
  df[['train_error']] <- 1- df$train_aucpr
  error_train <- df$train_error[df$iter == max(df$iter)]
  accuracy_train <- 1 - error_train
  
  results <- list(
    accuracy_validation = accuracy_validation,
    error_validation = error_validation,
    accuracy_baseline = accuracy_baseline, 
    error_baseline = error_baseline,
    accuracy_train = accuracy_train
  )
  
  results
})


