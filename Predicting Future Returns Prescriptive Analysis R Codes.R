# Predicting Future Returns and Optimizing SPY Options Selection
# Submitted by : Swetha Rani Keshava Ravi
# Date : November 1, 2026
# Subject : Prescriptive Analysis


# 1. Loading Libraries and Setting

library(dplyr)
library(ggplot2)
library(lpSolve)
library(tidyr)

PREDICTIVE_SCRIPT <- "C:/Users/sweth/Desktop/Sem 3/Operational Analytics DSCI-726/Swetha Rani Predictive Analysis R codes.R"

CAPITAL                     <- 100000   
COMMISSION_PER_CONTRACT     <- 0.65     
VOLUME_LIMIT_SHARE          <- 0.10     
SINGLE_CONTRACT_LIMIT_SHARE <- 0.05     
OPTION_TYPE_LIMIT_SHARE     <- 0.70     
MINIMUM_ASK_PRICE           <- 0.25     
MINIMUM_VOLUME              <- 10       

# 2. Predictions from the predictive script

SKIP_MAIN <- TRUE
source(PREDICTIVE_SCRIPT)
if (!exists("wide_data")) wide_data <- load_wide_data(DATA_PATH)

option_returns            <- prepare_returns_data(wide_data)
chronological_split       <- split_chronologically(option_returns, TRAIN_FRACTION)
prepared_training_data    <- prepare_datasets(chronological_split$train,
                                              chronological_split$train[1, ],
                                              CANDIDATE_FEATURES)$fit
usable_predictors         <- drop_aliased(prepared_training_data, CANDIDATE_FEATURES)
boosting_features         <- select_features(prepared_training_data, usable_predictors)$boosting
final_datasets            <- prepare_datasets(chronological_split$train,
                                              chronological_split$test,
                                              CANDIDATE_FEATURES)
set.seed(RANDOM_SEED)
boosting_model            <- fit_model("boosting", final_datasets$fit, boosting_features)

test_data <- chronological_split$test        # raw test-period data
test_data$PREDICTED_RETURN <- predict_model("boosting", boosting_model,
                                            final_datasets$evaluate)

# 3. Profit per contract 

test_data <- test_data %>%
  mutate(BID_ASK_SPREAD   = ASK - BID,
         CONTRACT_COST    = 100 * ASK + COMMISSION_PER_CONTRACT,
         EXPECTED_PROFIT  = 100 * (PRICE * (1 + PREDICTED_RETURN / 100) -
                                     0.5 * BID_ASK_SPREAD - ASK) - 2 * COMMISSION_PER_CONTRACT,
         REALIZED_PROFIT  = 100 * (coalesce(NEXT_BID, NEXT_PRICE - 0.5 * BID_ASK_SPREAD) - ASK) -
           2 * COMMISSION_PER_CONTRACT,
         MAXIMUM_QUANTITY = pmin(floor(VOLUME_LIMIT_SHARE * VOLUME),
                                 floor(SINGLE_CONTRACT_LIMIT_SHARE * CAPITAL / CONTRACT_COST)),
         IS_ELIGIBLE      = !is.na(BID) & BID > 0 & ASK >= MINIMUM_ASK_PRICE & !is.na(VOLUME) &
           VOLUME >= MINIMUM_VOLUME & MAXIMUM_QUANTITY >= 1)

# 4. Optimizing each trade date 

optimize_one_trade_date <- function(one_day_data) {
  candidate_options <- one_day_data %>%
    filter(IS_ELIGIBLE, EXPECTED_PROFIT > 0) %>%
    arrange(desc(EXPECTED_PROFIT / CONTRACT_COST)) %>%
    slice_head(n = 300)
  if (nrow(candidate_options) < 3) return(NULL)
  number_of_candidates <- nrow(candidate_options)
  constraint_matrix <- rbind(candidate_options$CONTRACT_COST,
                             candidate_options$CONTRACT_COST * (candidate_options$OPTION_TYPE == "Call"),
                             candidate_options$CONTRACT_COST * (candidate_options$OPTION_TYPE == "Put"),
                             diag(number_of_candidates))
  linear_program_solution <- lp("max", candidate_options$EXPECTED_PROFIT, constraint_matrix,
                                rep("<=", 3 + number_of_candidates),
                                c(CAPITAL, OPTION_TYPE_LIMIT_SHARE * CAPITAL,
                                  OPTION_TYPE_LIMIT_SHARE * CAPITAL, candidate_options$MAXIMUM_QUANTITY))
  candidate_options$QUANTITY <- floor(linear_program_solution$solution + 1e-7)   # whole contracts
  candidate_options %>% filter(QUANTITY > 0) %>% mutate(STRATEGY = "Optimized (LP)")
}

# Comparison portfolio: equal dollars in the 50 highest predicted returns

top_50_portfolio_one_trade_date <- function(one_day_data) {
  top_options <- one_day_data %>% filter(IS_ELIGIBLE) %>%
    slice_max(PREDICTED_RETURN, n = 50, with_ties = FALSE)
  top_options$QUANTITY <- pmin(floor(CAPITAL / nrow(top_options) / top_options$CONTRACT_COST),
                               top_options$MAXIMUM_QUANTITY)
  top_options %>% filter(QUANTITY > 0) %>% mutate(STRATEGY = "Top 50 by predicted return")
}

# Comparison portfolio: equal dollars in the same contracts

equal_dollar_portfolio_one_trade_date <- function(optimized_portfolio) {
  if (is.null(optimized_portfolio)) return(NULL)
  equal_options <- optimized_portfolio
  equal_options$QUANTITY <- pmin(floor(CAPITAL / nrow(equal_options) / equal_options$CONTRACT_COST),
                                 equal_options$MAXIMUM_QUANTITY)
  equal_options %>% filter(QUANTITY > 0) %>% mutate(STRATEGY = "Equal dollars, same contracts")
}

trade_dates <- sort(unique(test_data$QUOTE_DATE))
all_trades <- bind_rows(lapply(trade_dates, function(trade_date) {
  one_day_data        <- test_data %>% filter(QUOTE_DATE == trade_date)
  optimized_portfolio <- optimize_one_trade_date(one_day_data)
  bind_rows(optimized_portfolio,
            equal_dollar_portfolio_one_trade_date(optimized_portfolio),
            top_50_portfolio_one_trade_date(one_day_data))
})) %>% mutate(NET_PROFIT          = QUANTITY * REALIZED_PROFIT,
               CAPITAL_SPENT       = QUANTITY * CONTRACT_COST,
               EXPECTED_NET_PROFIT = QUANTITY * EXPECTED_PROFIT,
               CONTRACT_ID         = paste(OPTION_TYPE, STRIKE, EXPIRE_DATE))



# 4a. Diagnostic: identify trades using estimated exit prices

exit_diagnostic <- all_trades %>%
  group_by(STRATEGY) %>%
  summarise(
    Total_Positions = n(),
    Estimated_Exit_Positions = sum(is.na(NEXT_BID)),
    Observed_Bid_Positions = sum(!is.na(NEXT_BID)),
    Estimated_Exit_Position_Pct =
      100 * Estimated_Exit_Positions / Total_Positions,
    
    Total_Contracts = sum(QUANTITY, na.rm = TRUE),
    Contracts_Using_Estimated_Exit =
      sum(QUANTITY[is.na(NEXT_BID)], na.rm = TRUE),
    Estimated_Exit_Contract_Pct =
      100 * Contracts_Using_Estimated_Exit / Total_Contracts,
    
    .groups = "drop"
  )

cat("\nDIAGNOSTIC: TRADES USING ESTIMATED EXIT PRICES\n")
print(exit_diagnostic)

# ============================================================
# 4B. OBSERVED-BID-ONLY ANALYSIS
# ============================================================

# Retain only positions with an observed next available bid.
# Original selected contracts and quantities are preserved.
# No capital is redistributed and no replacement trades are added.

observed_bid_trades <- all_trades %>%
  filter(!is.na(NEXT_BID)) %>%
  mutate(
    OBSERVED_REALIZED_PROFIT =
      100 * (NEXT_BID - ASK) -
      2 * COMMISSION_PER_CONTRACT,
    
    OBSERVED_NET_PROFIT =
      QUANTITY * OBSERVED_REALIZED_PROFIT,
    
    OBSERVED_CAPITAL_SPENT = CAPITAL_SPENT
  )


# Calculate daily observed-bid-only results.

observed_daily_actual <- observed_bid_trades %>%
  group_by(STRATEGY, QUOTE_DATE) %>%
  summarise(
    NET_PROFIT = sum(OBSERVED_NET_PROFIT, na.rm = TRUE),
    CAPITAL_SPENT = sum(OBSERVED_CAPITAL_SPENT, na.rm = TRUE),
    .groups = "drop"
  )


# Include dates with no observed-bid-only trades.

observed_daily_results <- tidyr::expand_grid(
  STRATEGY = unique(all_trades$STRATEGY),
  QUOTE_DATE = trade_dates
) %>%
  left_join(
    observed_daily_actual,
    by = c("STRATEGY", "QUOTE_DATE")
  ) %>%
  mutate(
    NET_PROFIT = coalesce(NET_PROFIT, 0),
    CAPITAL_SPENT = coalesce(CAPITAL_SPENT, 0)
  ) %>%
  group_by(STRATEGY) %>%
  arrange(QUOTE_DATE) %>%
  mutate(
    CUMULATIVE_PROFIT = cumsum(NET_PROFIT)
  ) %>%
  ungroup()


# Summarize observed-bid-only performance.

observed_strategy_summary <- observed_daily_results %>%
  group_by(STRATEGY) %>%
  summarise(
    TOTAL_PROFIT = sum(NET_PROFIT),
    AVERAGE_CAPITAL_SPENT = mean(CAPITAL_SPENT),
    WORST_DAY_PROFIT = min(NET_PROFIT),
    
    RETURN_ON_CAPITAL_SPENT_PCT =
      ifelse(
        sum(CAPITAL_SPENT) > 0,
        100 * sum(NET_PROFIT) / sum(CAPITAL_SPENT),
        NA_real_
      ),
    
    .groups = "drop"
  )


# ============================================================
# 4C. ORIGINAL STRATEGY SUMMARY
# ============================================================

cash_holdings <- tibble(
  STRATEGY = "Hold cash",
  QUOTE_DATE = trade_dates,
  NET_PROFIT = 0,
  CAPITAL_SPENT = 0
)


daily_results <- all_trades %>%
  group_by(STRATEGY, QUOTE_DATE) %>%
  summarise(
    NET_PROFIT = sum(NET_PROFIT, na.rm = TRUE),
    CAPITAL_SPENT = sum(CAPITAL_SPENT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  bind_rows(cash_holdings) %>%
  group_by(STRATEGY) %>%
  arrange(QUOTE_DATE) %>%
  mutate(
    CUMULATIVE_PROFIT = cumsum(NET_PROFIT)
  ) %>%
  ungroup()


# Create the original strategy summary FIRST.

strategy_summary_table <- daily_results %>%
  group_by(STRATEGY) %>%
  summarise(
    TOTAL_PROFIT = sum(NET_PROFIT),
    AVERAGE_CAPITAL_SPENT = mean(CAPITAL_SPENT),
    WORST_DAY_PROFIT = min(NET_PROFIT),
    
    RETURN_ON_CAPITAL_SPENT_PCT =
      ifelse(
        sum(CAPITAL_SPENT) > 0,
        100 * sum(NET_PROFIT) / sum(CAPITAL_SPENT),
        NA_real_
      ),
    
    .groups = "drop"
  )


# Display original strategy results.

print(strategy_summary_table)


# ============================================================
# 4D. ORIGINAL VS. OBSERVED-BID-ONLY COMPARISON
# ============================================================

# Original strategy results.

original_comparison <- strategy_summary_table %>%
  filter(STRATEGY != "Hold cash") %>%
  select(
    STRATEGY,
    ORIGINAL_TOTAL_PROFIT = TOTAL_PROFIT,
    ORIGINAL_AVERAGE_CAPITAL = AVERAGE_CAPITAL_SPENT,
    ORIGINAL_RETURN_PCT = RETURN_ON_CAPITAL_SPENT_PCT
  )


# Observed-bid-only strategy results.

observed_comparison <- observed_strategy_summary %>%
  select(
    STRATEGY,
    OBSERVED_ONLY_TOTAL_PROFIT = TOTAL_PROFIT,
    OBSERVED_ONLY_AVERAGE_CAPITAL = AVERAGE_CAPITAL_SPENT,
    OBSERVED_ONLY_RETURN_PCT = RETURN_ON_CAPITAL_SPENT_PCT
  )


# Combine both summaries AFTER creating them.

final_exit_comparison <- original_comparison %>%
  left_join(
    observed_comparison,
    by = "STRATEGY"
  )


# Display final comparison.

print(final_exit_comparison)




#  5. Graphs
theme_set(theme_minimal(base_size = 11))

# Graph 1: cumulative profit
print(ggplot(daily_results, aes(QUOTE_DATE, CUMULATIVE_PROFIT, colour = STRATEGY, linetype = STRATEGY)) +
        geom_hline(yintercept = 0, colour = "grey60") + geom_line(linewidth = 1) + geom_point() +
        scale_colour_grey(start = 0, end = 0.6) + scale_y_continuous(labels = scales::dollar) +
        labs(title = "Cumulative Net Profit by Strategy", x = "Trade date", y = "Cumulative profit"))

# Graph 2: total profit
print(ggplot(strategy_summary_table, aes(reorder(STRATEGY, TOTAL_PROFIT), TOTAL_PROFIT)) +
        geom_col(fill = "grey35", width = 0.6) + geom_hline(yintercept = 0) + coord_flip() +
        scale_y_continuous(labels = scales::dollar) +
        labs(title = "Total Net Profit over the Test Period", x = NULL, y = "Net profit"))

# Graph 3: where the optimizer put the money
first_date_allocation <- all_trades %>%
  filter(STRATEGY == "Optimized (LP)") %>%
  filter(QUOTE_DATE == min(QUOTE_DATE)) %>%
  mutate(OPTION_GROUP = paste(OPTION_TYPE, format(EXPIRE_DATE, "%b %d"))) %>%
  group_by(OPTION_GROUP) %>% summarise(CAPITAL_SPENT = sum(CAPITAL_SPENT))
print(ggplot(first_date_allocation, aes(reorder(OPTION_GROUP, CAPITAL_SPENT), CAPITAL_SPENT)) +
        geom_col(fill = "grey35") + coord_flip() + scale_y_continuous(labels = scales::dollar) +
        labs(title = "Capital Allocated on the First Test Date", x = NULL, y = "Capital"))

# Graph 4: sensitivity
profit_with_return_haircut <- function(share_of_predicted_return_used) {
  data_with_haircut <- test_data %>%
    mutate(EXPECTED_PROFIT = 100 * (PRICE * (1 + share_of_predicted_return_used * PREDICTED_RETURN / 100) -
                                      0.5 * BID_ASK_SPREAD - ASK) - 2 * COMMISSION_PER_CONTRACT)
  sum(sapply(trade_dates, function(trade_date) {
    optimized_portfolio <- optimize_one_trade_date(data_with_haircut %>% filter(QUOTE_DATE == trade_date))
    if (is.null(optimized_portfolio)) 0 else sum(optimized_portfolio$QUANTITY * optimized_portfolio$REALIZED_PROFIT)
  }))
}
haircut_sensitivity_table <- data.frame(SHARE_OF_PREDICTED_RETURN_USED = c(1, 0.75, 0.5, 0.25, 0))
haircut_sensitivity_table$NET_PROFIT <- sapply(haircut_sensitivity_table$SHARE_OF_PREDICTED_RETURN_USED,
                                               profit_with_return_haircut)
print(haircut_sensitivity_table)
print(ggplot(haircut_sensitivity_table, aes(factor(SHARE_OF_PREDICTED_RETURN_USED * 100), NET_PROFIT)) +
        geom_col(fill = "grey35", width = 0.6) +
        geom_hline(yintercept = 0) + scale_y_continuous(labels = scales::dollar) +
        labs(title = "Sensitivity: Optimizer Trusts Only Part of the Predicted Return",
             x = "Share of predicted return used (%)", y = "Net profit"))

# Graph 5: sensitivity - commission
optimized_portfolio_trades <- all_trades %>% filter(STRATEGY == "Optimized (LP)")
profit_at_commission <- function(commission_per_contract) {
  sum(optimized_portfolio_trades$NET_PROFIT -
        2 * (commission_per_contract - COMMISSION_PER_CONTRACT) * optimized_portfolio_trades$QUANTITY)
}
commission_sensitivity_table <- data.frame(COMMISSION_PER_CONTRACT = c(0, 0.65, 1.3, 2.6))
commission_sensitivity_table$NET_PROFIT <- sapply(commission_sensitivity_table$COMMISSION_PER_CONTRACT,
                                                  profit_at_commission)
print(commission_sensitivity_table)
print(ggplot(commission_sensitivity_table, aes(factor(COMMISSION_PER_CONTRACT), NET_PROFIT)) +
        geom_col(fill = "grey35", width = 0.6) +
        geom_hline(yintercept = 0) + scale_y_continuous(labels = scales::dollar) +
        labs(title = "Sensitivity: Commission per Contract (optimized portfolio)",
             x = "Commission per contract per side (USD)", y = "Net profit"))

# Graph 6: total profit with a 95% bootstrap interval 
test_data$CONTRACT_ID <- paste(test_data$OPTION_TYPE, test_data$STRIKE, test_data$EXPIRE_DATE)
eligible_contract_ids <- unique(test_data$CONTRACT_ID[test_data$IS_ELIGIBLE])
strategy_names        <- unique(all_trades$STRATEGY)
profit_by_contract <- sapply(strategy_names, function(strategy_name) {
  profit_per_traded_contract <- tapply(all_trades$NET_PROFIT[all_trades$STRATEGY == strategy_name],
                                       all_trades$CONTRACT_ID[all_trades$STRATEGY == strategy_name], sum)
  profit_for_every_contract <- setNames(rep(0, length(eligible_contract_ids)), eligible_contract_ids)
  profit_for_every_contract[names(profit_per_traded_contract)] <- profit_per_traded_contract
  profit_for_every_contract
})
set.seed(726)
bootstrap_total_profits <- t(replicate(1000, {
  contract_weights <- tabulate(sample.int(nrow(profit_by_contract), nrow(profit_by_contract), replace = TRUE),
                               nrow(profit_by_contract))
  colSums(contract_weights * profit_by_contract)
}))
bootstrap_interval_table <- data.frame(
  STRATEGY     = strategy_names,
  TOTAL_PROFIT = colSums(profit_by_contract),
  LOWER_95     = apply(bootstrap_total_profits, 2, quantile, 0.025),
  UPPER_95     = apply(bootstrap_total_profits, 2, quantile, 0.975))
print(bootstrap_interval_table, row.names = FALSE)
print(ggplot(bootstrap_interval_table,
             aes(reorder(STRATEGY, TOTAL_PROFIT), TOTAL_PROFIT, ymin = LOWER_95, ymax = UPPER_95)) +
        geom_pointrange() + geom_hline(yintercept = 0, colour = "grey50") + coord_flip() +
        scale_y_continuous(labels = scales::dollar) +
        labs(title = "Total Net Profit with 95% Contract-Level Bootstrap Interval",
             x = NULL, y = "Net profit") +
        theme(plot.title = element_text(hjust = 0.5, size = 11),
              plot.title.position = "plot"))

# Graph 7: profit the model expected vs profit actually realized
expected_vs_realized_table <- all_trades %>%
  group_by(STRATEGY) %>%
  summarise(Expected = sum(EXPECTED_NET_PROFIT), Realized = sum(NET_PROFIT), .groups = "drop") %>%
  tidyr::pivot_longer(c(Expected, Realized), names_to = "PROFIT_TYPE", values_to = "NET_PROFIT")
print(as.data.frame(expected_vs_realized_table))
print(ggplot(expected_vs_realized_table, aes(STRATEGY, NET_PROFIT, fill = PROFIT_TYPE)) +
        geom_col(position = "dodge", width = 0.7) +
        geom_hline(yintercept = 0) + scale_fill_grey(start = 0.3, end = 0.75) +
        scale_y_continuous(labels = scales::dollar) +
        theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
        labs(title = "Expected versus Realized Net Profit", x = NULL, y = "Net profit", fill = NULL))

# Graph 8: top-decile return on the CAPPED and UNCAPPED return scales
capped_return               <- cap_target(test_data$FUTURE_RETURN, chronological_split$train$FUTURE_RETURN)
is_top_decile               <- test_data$PREDICTED_RETURN >= quantile(test_data$PREDICTED_RETURN, 0.90)
mean_with_contract_interval <- function(returns, include_flag) {
  contract_index      <- match(test_data$CONTRACT_ID, unique(test_data$CONTRACT_ID))
  number_of_contracts <- max(contract_index)
  bootstrap_means <- replicate(500, {
    contract_weights <- tabulate(sample.int(number_of_contracts, number_of_contracts, replace = TRUE),
                                 number_of_contracts)[contract_index]
    sum(contract_weights * include_flag * returns) / sum(contract_weights * include_flag)
  })
  c(mean(returns[include_flag]), quantile(bootstrap_means, c(0.025, 0.975)))
}
decile_results <- list(
  c("All contracts", "Capped",   mean_with_contract_interval(capped_return, rep(TRUE, nrow(test_data)))),
  c("All contracts", "Uncapped", mean_with_contract_interval(test_data$FUTURE_RETURN, rep(TRUE, nrow(test_data)))),
  c("Top decile",    "Capped",   mean_with_contract_interval(capped_return, is_top_decile)),
  c("Top decile",    "Uncapped", mean_with_contract_interval(test_data$FUTURE_RETURN, is_top_decile)))
capped_vs_uncapped_table <- data.frame(
  GROUP        = sapply(decile_results, `[`, 1),
  RETURN_SCALE = sapply(decile_results, `[`, 2),
  MEAN_RETURN  = as.numeric(sapply(decile_results, `[`, 3)),
  LOWER_95     = as.numeric(sapply(decile_results, `[`, 4)),
  UPPER_95     = as.numeric(sapply(decile_results, `[`, 5)))
print(capped_vs_uncapped_table, row.names = FALSE)
print(ggplot(capped_vs_uncapped_table,
             aes(GROUP, MEAN_RETURN, fill = RETURN_SCALE, ymin = LOWER_95, ymax = UPPER_95)) +
        geom_col(position = position_dodge(0.8), width = 0.7) +
        geom_errorbar(position = position_dodge(0.8), width = 0.2) +
        geom_hline(yintercept = 0) + scale_fill_grey(start = 0.3, end = 0.75) +
        labs(title = "Mean Actual Return: Capped versus Uncapped",
             x = NULL, y = "Mean return (%)", fill = NULL))