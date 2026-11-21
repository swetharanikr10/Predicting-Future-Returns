# Predicting Future Returns and Optimizing SPY Options Selection
# Submitted by : Swetha Rani Keshava Ravi
# Date : October 18, 2026
# Subject : Predictive Analysis

# 1. Loading Libraries and Setting Configuration Parameters

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(ranger)
library(gbm)

DATA_PATH <- "C:/Users/sweth/Desktop/Sem 3/Operational Analytics DSCI-726/project new.xlsx"
TRAIN_FRACTION    <- 0.70   
CV_FOLDS          <- 3      
VIF_LIMIT         <- 10     
CORRELATION_LIMIT <- 0.90   
FOREST_TREES      <- 200
BOOSTING_TREES    <- 300
BOOTSTRAP_RUNS    <- 500
RANDOM_SEED       <- 726

CANDIDATE_FEATURES <- c(
  "IS_CALL", "LOG_DTE", "LOG_MONEYNESS", "DELTA", "GAMMA", "VEGA", "THETA",
  "IV", "LOG_VOLUME", "VOLUME_MISSING", "IV_MISSING",
  "LOG_MID_PRICE", "REL_SPREAD", "LAST_TO_MID_GAP"
)


# 2. Data Preparation and Feature Engineering

load_wide_data <- function(path) {
  wide <- read_excel(path)
  wide[["[QUOTE_DATE]"]]  <- as.Date(wide[["[QUOTE_DATE]"]])
  wide[["[EXPIRE_DATE]"]] <- as.Date(wide[["[EXPIRE_DATE]"]])
  is_date <- names(wide) %in% c("[QUOTE_DATE]", "[EXPIRE_DATE]")
  wide[!is_date] <- lapply(wide[!is_date], as.numeric)
  wide
}

extract_side <- function(wide, prefix, option_type) {
  side <- function(name) wide[[paste0("[", prefix, "_", name, "]")]]
  tibble(
    QUOTE_DATE      = wide[["[QUOTE_DATE]"]],
    EXPIRE_DATE     = wide[["[EXPIRE_DATE]"]],
    STRIKE          = wide[["[STRIKE]"]],
    UNDERLYING_LAST = wide[["[UNDERLYING_LAST]"]],
    DTE             = wide[["[DTE]"]],
    OPTION_TYPE     = option_type,
    PRICE  = side("LAST"),  BID   = side("BID"),   ASK    = side("ASK"),
    DELTA  = side("DELTA"), GAMMA = side("GAMMA"), VEGA   = side("VEGA"),
    THETA  = side("THETA"), IV    = side("IV"),    VOLUME = side("VOLUME")
  )
}

stack_calls_puts <- function(wide) {
  bind_rows(
    extract_side(wide, "C", "Call"),
    extract_side(wide, "P", "Put")
  )
}

add_next_quote <- function(options_long) {
  options_long %>%
    arrange(OPTION_TYPE, STRIKE, EXPIRE_DATE, QUOTE_DATE) %>%
    group_by(OPTION_TYPE, STRIKE, EXPIRE_DATE) %>%
    mutate(
      NEXT_DATE  = lead(QUOTE_DATE),
      NEXT_PRICE = lead(PRICE),
      NEXT_BID   = lead(BID),
      NEXT_ASK   = lead(ASK)
    ) %>%
    ungroup()
}

keep_valid_returns <- function(with_next_quote) {
  with_next_quote %>%
    mutate(
      genuine_zero  = NEXT_PRICE == 0 & NEXT_BID == 0 & NEXT_ASK == 0,
      valid_return  = !is.na(PRICE) & PRICE > 0 & !is.na(NEXT_PRICE) &
        (NEXT_PRICE > 0 | genuine_zero %in% TRUE),
      FUTURE_RETURN = (NEXT_PRICE - PRICE) / PRICE * 100
    ) %>%
    filter(valid_return, is.finite(FUTURE_RETURN))
}


engineer_features <- function(df) {
  df %>%
    mutate(
      IS_CALL         = as.integer(OPTION_TYPE == "Call"),
      MID_PRICE       = (BID + ASK) / 2,
      REL_SPREAD      = if_else(MID_PRICE > 0, (ASK - BID) / MID_PRICE, NA_real_),
      LAST_TO_MID_GAP = if_else(MID_PRICE > 0, (PRICE - MID_PRICE) / MID_PRICE, NA_real_),
      LOG_MID_PRICE   = if_else(MID_PRICE > 0, log(MID_PRICE), NA_real_),
      LOG_MONEYNESS   = log(STRIKE / UNDERLYING_LAST),
      LOG_DTE         = log1p(DTE),
      LOG_VOLUME      = log1p(coalesce(VOLUME, 0)),
      VOLUME_MISSING  = as.integer(is.na(VOLUME)),
      IV_MISSING      = as.integer(is.na(IV))
    )
}


# 3. Splitting and preprocessing 

# Chronological Split
split_chronologically <- function(returns_data, train_fraction) {
  dates  <- sort(unique(returns_data$QUOTE_DATE))
  cutoff <- dates[floor(length(dates) * train_fraction)]
  list(
    cutoff = cutoff,
    train  = filter(returns_data, QUOTE_DATE <= cutoff, NEXT_DATE <= cutoff),
    test   = filter(returns_data, QUOTE_DATE > cutoff)
  )
}

# Rolling-Origin Folds
make_time_folds <- function(train, n_folds) {
  dates <- sort(unique(train$QUOTE_DATE))
  block <- cut(seq_along(dates), breaks = n_folds + 1, labels = FALSE)
  lapply(seq_len(n_folds), function(i) {
    validation_dates <- dates[block == i + 1]
    start <- min(validation_dates)
    list(
      fit      = which(train$QUOTE_DATE < start & train$NEXT_DATE < start),
      validate = which(train$QUOTE_DATE %in% validation_dates)
    )
  })
}

# Fitting Preprocessor
fit_preprocessor <- function(train, features) {
  is_continuous <- vapply(train[features], function(x) n_distinct(x) > 2, logical(1))
  columns <- features[is_continuous]
  column_stat <- function(stat) vapply(train[columns], stat, numeric(1))
  list(
    columns = columns,
    lower   = column_stat(function(x) unname(quantile(x, 0.01, na.rm = TRUE))),
    upper   = column_stat(function(x) unname(quantile(x, 0.99, na.rm = TRUE))),
    middle  = column_stat(function(x) median(x, na.rm = TRUE))
  )
}

# Appling Preprocessing
apply_preprocessor <- function(df, preprocessor) {
  for (column in preprocessor$columns) {
    capped <- pmin(pmax(df[[column]], preprocessor$lower[[column]]),
                   preprocessor$upper[[column]])
    df[[column]] <- if_else(is.na(capped), preprocessor$middle[[column]], capped)
  }
  df
}

# Target Capping
cap_target <- function(returns, training_returns) {
  limits <- quantile(training_returns, c(0.01, 0.99))
  pmin(pmax(returns, limits[[1]]), limits[[2]])
}

# Preparing Datasets
prepare_datasets <- function(fit_data, evaluate_data, features) {
  preprocessor <- fit_preprocessor(fit_data, features)
  finish <- function(df) {
    df <- apply_preprocessor(df, preprocessor)
    df$TARGET <- cap_target(df$FUTURE_RETURN, fit_data$FUTURE_RETURN)
    as.data.frame(df)
  }
  list(fit = finish(fit_data), evaluate = finish(evaluate_data))
}


# 4. Collinearity and predictor removal

# VIF Calculation
compute_vif <- function(prepared, features) {
  linear_fit <- lm(reformulate(features, "TARGET"), data = prepared)
  sort(vif(linear_fit), decreasing = TRUE)
}

# VIF-Based Removal
drop_by_vif <- function(prepared, features, limit) {
  repeat {
    scores <- compute_vif(prepared, features)
    if (scores[[1]] <= limit || length(features) <= 2) break
    features <- setdiff(features, names(scores)[1])
  }
  features
}

# Correlation-Based Removal
drop_by_correlation <- function(prepared, features, limit) {
  correlations <- abs(cor(prepared[features], method = "spearman"))
  correlations[lower.tri(correlations, diag = TRUE)] <- 0
  setdiff(features, features[apply(correlations, 2, max) > limit])
}

# Aliased Predictor Removal
drop_aliased <- function(prepared, features) {
  coefficients <- coef(lm(reformulate(features, "TARGET"), data = prepared))
  setdiff(features, names(coefficients)[is.na(coefficients)])
}

# Feature Selection
select_features <- function(prepared_train, usable_features) {
  linear_set <- drop_by_vif(prepared_train, usable_features, VIF_LIMIT)
  tree_set   <- drop_by_correlation(prepared_train, usable_features, CORRELATION_LIMIT)
  list(baseline = character(0), linear = linear_set,
       forest = tree_set, boosting = tree_set)
}


# 5. Models 

# Model Fitting
fit_model <- function(method, train, features) {
  if (method == "baseline") return(0)
  model_formula <- reformulate(features, "TARGET")
  switch(method,
         linear   = lm(model_formula, data = train),
         forest   = ranger(model_formula, data = train, num.trees = FOREST_TREES,
                           min.node.size = 50, importance = "impurity",
                           seed = RANDOM_SEED, verbose = FALSE),
         boosting = gbm(model_formula, data = train, distribution = "gaussian",
                        n.trees = BOOSTING_TREES, interaction.depth = 4,
                        shrinkage = 0.05, bag.fraction = 0.5, n.minobsinnode = 50)
  )
}

# Model Prediction
predict_model <- function(method, model, new_data) {
  switch(method,
         baseline = rep(model, nrow(new_data)),
         linear   = unname(predict(model, new_data)),
         forest   = predict(model, new_data)$predictions,
         boosting = predict(model, new_data, n.trees = BOOSTING_TREES)
  )
}

# Fit All Models
fit_all_methods <- function(fit_data, feature_sets) {
  models <- lapply(names(feature_sets), function(method) {
    fit_model(method, fit_data, feature_sets[[method]])
  })
  setNames(models, names(feature_sets))
}

# Generate Predictions
predict_all_methods <- function(models, new_data) {
  predictions <- lapply(names(models), function(method) {
    predict_model(method, models[[method]], new_data)
  })
  setNames(predictions, names(models))
}


# 6. Evaluation

# Prediction Metrics
evaluate_predictions <- function(actual, predicted) {
  top_decile <- predicted >= quantile(predicted, 0.90)
  tibble(
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE  = mean(abs(actual - predicted)),
    SPEARMAN = suppressWarnings(cor(predicted, actual, method = "spearman")),
    TOP_DECILE_RETURN = mean(actual[top_decile])
  )
}

# Model Scoring
score_methods <- function(predictions, actual) {
  scores <- lapply(names(predictions), function(method) {
    mutate(evaluate_predictions(actual, predictions[[method]]), METHOD = method)
  })
  bind_rows(scores)
}

# Cross-Validation
cross_validate <- function(train, folds, feature_sets) {
  fold_scores <- lapply(seq_along(folds), function(i) {
    data <- prepare_datasets(train[folds[[i]]$fit, ], train[folds[[i]]$validate, ],
                             CANDIDATE_FEATURES)
    models <- fit_all_methods(data$fit, feature_sets)
    predictions <- predict_all_methods(models, data$evaluate)
    mutate(score_methods(predictions, data$evaluate$TARGET), FOLD = i)
  })
  bind_rows(fold_scores)
}

# Best Model Selection
choose_best_method <- function(cv_scores) {
  cv_scores %>%
    filter(METHOD != "baseline") %>%
    group_by(METHOD) %>%
    summarise(MEAN_RMSE = mean(RMSE), .groups = "drop") %>%
    slice_min(MEAN_RMSE, n = 1) %>%
    pull(METHOD)
}

# Bootstrap Confidence Interval
bootstrap_interval <- function(values, runs) {
  means <- replicate(runs, mean(sample(values, replace = TRUE)))
  quantile(means, c(0.025, 0.975))
}

# Bootstrap Model Comparison
bootstrap_test_results <- function(predictions, actual, runs) {
  baseline_sq_error <- (actual - predictions$baseline)^2
  rows <- lapply(names(predictions), function(method) {
    sq_error <- (actual - predictions[[method]])^2
    rmse_ci  <- sqrt(bootstrap_interval(sq_error, runs))
    gain_ci  <- bootstrap_interval(baseline_sq_error - sq_error, runs)
    tibble(METHOD = method, RMSE_LOW = rmse_ci[[1]], RMSE_HIGH = rmse_ci[[2]],
           MSE_GAIN_LOW = gain_ci[[1]], MSE_GAIN_HIGH = gain_ci[[2]])
  })
  bind_rows(rows)
}


# 7. Visualization

# Bar Chart: Variance Inflation Factors (VIF)
plot_vif <- function(vif_scores) {
  tibble(PREDICTOR = factor(names(vif_scores), levels = rev(names(vif_scores))),
         VIF = as.numeric(vif_scores)) %>%
    ggplot(aes(x = PREDICTOR, y = VIF)) +
    geom_col() +
    geom_hline(yintercept = VIF_LIMIT, linetype = "dashed") +
    coord_flip() +
    labs(title = "Variance Inflation Factors (training data)", x = NULL, y = "VIF") +
    theme_minimal()
}

# Line Chart: Cross-Validated RMSE by Fold
plot_cv_rmse <- function(cv_scores) {
  ggplot(cv_scores, aes(x = FOLD, y = RMSE, colour = METHOD,
                        linetype = METHOD, shape = METHOD)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    scale_colour_grey(start = 0, end = 0.65) +
    scale_x_continuous(breaks = unique(cv_scores$FOLD)) +
    labs(title = "Cross-Validated RMSE by Fold", x = "Fold", y = "RMSE (percentage points)") +
    theme_minimal()
}

# Grouped Bar Chart: Test-Set Error Comparison
plot_metric_comparison <- function(test_scores) {
  test_scores %>%
    pivot_longer(c(RMSE, MAE), names_to = "METRIC", values_to = "VALUE") %>%
    ggplot(aes(x = METHOD, y = VALUE, fill = METRIC)) +
    geom_col(position = "dodge") +
    scale_fill_grey(start = 0.25, end = 0.75) +
    labs(title = "Test-Set Error by Method", x = "Method", y = "Error (percentage points)") +
    theme_minimal()
}

# Point-Range Plot: Test RMSE with 95% Bootstrap Intervals
plot_bootstrap_intervals <- function(test_scores, bootstrap_results) {
  inner_join(test_scores, bootstrap_results, by = "METHOD") %>%
    ggplot(aes(x = METHOD, y = RMSE, ymin = RMSE_LOW, ymax = RMSE_HIGH)) +
    geom_pointrange() +
    labs(title = "Test RMSE with 95% Bootstrap Interval", x = "Method", y = "RMSE") +
    theme_minimal()
}

# Bar Chart: Top-Decile Mean Actual Return
plot_top_decile <- function(test_scores) {
  ggplot(test_scores, aes(x = METHOD, y = TOP_DECILE_RETURN)) +
    geom_col() +
    geom_hline(yintercept = 0) +
    labs(title = "Mean Actual Return of Top-Ranked 10% of Contracts (test set)",
         x = "Method", y = "Mean actual return (%)") +
    theme_minimal()
}

# Mean predicted and mean actual return within deciles of the prediction.
calibration_table <- function(predicted, actual) {
  tibble(predicted, actual, decile = ntile(predicted, 10)) %>%
    group_by(decile) %>%
    summarise(PREDICTED = mean(predicted), ACTUAL = mean(actual), .groups = "drop")
}

# Scatter Plot: Predicted vs. Actual Return by Decile
plot_calibration <- function(calibration, method) {
  ggplot(calibration, aes(x = PREDICTED, y = ACTUAL)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point(size = 3) +
    labs(title = paste("Predicted vs Actual Return by Decile:", method),
         x = "Mean predicted return (%)", y = "Mean actual return (%)") +
    theme_minimal()
}

# Horizontal Bar Chart: Random Forest Variable Importance
plot_forest_importance <- function(forest_model) {
  importance <- sort(forest_model$variable.importance)
  tibble(VARIABLE = factor(names(importance), levels = names(importance)),
         IMPORTANCE = as.numeric(importance)) %>%
    ggplot(aes(x = VARIABLE, y = IMPORTANCE)) +
    geom_col() +
    coord_flip() +
    labs(title = "Random Forest Variable Importance", x = NULL, y = "Importance") +
    theme_minimal()
}

# 8. Reporting steps 

# Section Header
print_section_header <- function(title) {
  
  cat(title, "\n")
}

# Chart Display
show_chart <- function(label, chart) {
  cat("\nGraph:", label, "\n")
  print(chart)
}

# Return Data Preparation
prepare_returns_data <- function(wide) {
  wide %>%
    stack_calls_puts() %>%
    add_next_quote() %>%
    keep_valid_returns() %>%
    engineer_features()
}

report_split <- function(split) {
  print_section_header("1. CHRONOLOGICAL TRAIN / TEST SPLIT")
  print(data.frame(
    Dataset          = c("Training", "Test"),
    Observations     = c(nrow(split$train), nrow(split$test)),
    First_Quote_Date = c(min(split$train$QUOTE_DATE), min(split$test$QUOTE_DATE)),
    Last_Quote_Date  = c(max(split$train$QUOTE_DATE), max(split$test$QUOTE_DATE))
  ))
  cat("Training cutoff date:", format(split$cutoff), "\n")
}

check_collinearity <- function(split) {
  prepared_train <- prepare_datasets(split$train, split$train[1, ], CANDIDATE_FEATURES)$fit
  usable_features <- drop_aliased(prepared_train, CANDIDATE_FEATURES)
  vif_scores <- compute_vif(prepared_train, usable_features)
  
  print_section_header("2. COLLINEARITY CHECKS")
  cat("\nPredictors dropped as constant or aliased:\n")
  print(setdiff(CANDIDATE_FEATURES, usable_features))
  cat("\nVariance inflation factors:\n")
  print(data.frame(Predictor = names(vif_scores),
                   VIF = round(as.numeric(vif_scores), 2), row.names = NULL))
  show_chart("Variance Inflation Factors", plot_vif(vif_scores))
  list(prepared_train = prepared_train, usable_features = usable_features,
       vif_scores = vif_scores)
}

report_feature_sets <- function(feature_sets) {
  cat("\nPredictors used by each method:\n")
  print(feature_sets)
}

run_cross_validation <- function(split, feature_sets) {
  folds <- make_time_folds(split$train, CV_FOLDS)
  cv_scores <- cross_validate(split$train, folds, feature_sets)
  
  print_section_header("3. ROLLING-ORIGIN CROSS-VALIDATION")
  cat("\nCross-validation scores by fold:\n")
  print(cv_scores, n = Inf)
  cat("\nMean cross-validation scores by method:\n")
  print(cv_scores %>% group_by(METHOD) %>%
          summarise(across(RMSE:TOP_DECILE_RETURN, mean), .groups = "drop"))
  show_chart("Cross-Validated RMSE by Fold", plot_cv_rmse(cv_scores))
  cv_scores
}

fit_and_predict_test <- function(split, feature_sets) {
  final <- prepare_datasets(split$train, split$test, CANDIDATE_FEATURES)
  models <- fit_all_methods(final$fit, feature_sets)
  predictions <- predict_all_methods(models, final$evaluate)
  list(models = models, predictions = predictions, actual = final$evaluate$TARGET)
}

report_test_scores <- function(test_scores) {
  print_section_header("4. FINAL TEST-SET MODEL EVALUATION")
  print(test_scores)
  show_chart("Test-Set Error by Method", plot_metric_comparison(test_scores))
}

report_bootstrap <- function(test_scores, bootstrap_results) {
  print_section_header("5. BOOTSTRAP CONFIDENCE INTERVALS")
  print(bootstrap_results)
  show_chart("Test RMSE with 95% Bootstrap Interval",
             plot_bootstrap_intervals(test_scores, bootstrap_results))
}

report_top_decile <- function(test_scores) {
  print_section_header("6. TOP-DECILE CONTRACT SELECTION")
  print(test_scores %>% select(METHOD, TOP_DECILE_RETURN))
  show_chart("Mean Actual Return of Top-Ranked 10% of Contracts",
             plot_top_decile(test_scores))
}

report_calibration <- function(calibration, best_method) {
  print_section_header("7. CALIBRATION OF THE BEST METHOD")
  print(calibration)
  show_chart("Predicted vs Actual Return by Decile",
             plot_calibration(calibration, best_method))
}

report_forest_importance <- function(forest_model) {
  print_section_header("8. RANDOM FOREST VARIABLE IMPORTANCE")
  importance <- sort(forest_model$variable.importance, decreasing = TRUE)
  print(data.frame(Variable = names(importance),
                   Importance = as.numeric(importance), row.names = NULL))
  show_chart("Random Forest Variable Importance", plot_forest_importance(forest_model))
}


# 9. Main analysis

run_pipeline <- function(wide) {
  set.seed(RANDOM_SEED)
  split <- split_chronologically(prepare_returns_data(wide), TRAIN_FRACTION)
  report_split(split)
  
  collinearity <- check_collinearity(split)
  feature_sets <- select_features(collinearity$prepared_train, collinearity$usable_features)
  report_feature_sets(feature_sets)
  
  cv_scores <- run_cross_validation(split, feature_sets)
  best_method <- choose_best_method(cv_scores)
  cat("\nBest non-baseline method by mean cross-validated RMSE:", best_method, "\n")
  
  test <- fit_and_predict_test(split, feature_sets)
  test_scores <- score_methods(test$predictions, test$actual)
  bootstrap_results <- bootstrap_test_results(test$predictions, test$actual, BOOTSTRAP_RUNS)
  calibration <- calibration_table(test$predictions[[best_method]], test$actual)
  
  report_test_scores(test_scores)
  report_bootstrap(test_scores, bootstrap_results)
  report_top_decile(test_scores)
  report_calibration(calibration, best_method)
  report_forest_importance(test$models$forest)
  
  invisible(list(split = split, cv_scores = cv_scores, best_method = best_method,
                 models = test$models, predictions = test$predictions,
                 test_scores = test_scores, bootstrap_results = bootstrap_results,
                 calibration = calibration))
}


# 10. Run the analysis 

if (!exists("SKIP_MAIN")) {
  results <- run_pipeline(load_wide_data(DATA_PATH))
}