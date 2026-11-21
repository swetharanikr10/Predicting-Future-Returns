# Predicting Future Returns and Optimizing SPY Options Selection
# Submitted by : Swetha Rani Keshava Ravi
# Date : October 18, 2026
# Subject : Descriptive Analysis (Revised)

# 1. Loading data

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

data <- read_excel(
  "C:/Users/sweth/Desktop/Sem 3/Operational Analytics DSCI-726/project new.xlsx"
)

names(data)

data$`[QUOTE_DATE]`  <- as.Date(data$`[QUOTE_DATE]`)
data$`[EXPIRE_DATE]` <- as.Date(data$`[EXPIRE_DATE]`)

numeric_vars <- c(
  "[UNDERLYING_LAST]", "[DTE]",
  "[C_DELTA]", "[C_GAMMA]", "[C_VEGA]", "[C_THETA]", "[C_IV]", "[C_VOLUME]",
  "[C_LAST]", "[C_BID]", "[C_ASK]", "[STRIKE]",
  "[P_BID]", "[P_ASK]", "[P_LAST]",
  "[P_DELTA]", "[P_GAMMA]", "[P_VEGA]", "[P_THETA]",
  "[P_IV]", "[P_VOLUME]", "[STRIKE_DISTANCE_PCT]"
)
data[numeric_vars] <- lapply(data[numeric_vars], as.numeric)

dim(data)
head(data)

# 2. Measures of Location and Dispersion (original wide data)

descriptive_stats <- data %>%
  select(where(is.numeric)) %>%
  summarise(
    across(
      everything(),
      list(
        Min    = ~min(.x, na.rm = TRUE),
        Q1     = ~quantile(.x, 0.25, na.rm = TRUE),
        Median = ~median(.x, na.rm = TRUE),
        Q3     = ~quantile(.x, 0.75, na.rm = TRUE),
        Max    = ~max(.x, na.rm = TRUE),
        Mean   = ~mean(.x, na.rm = TRUE),
        SD     = ~sd(.x, na.rm = TRUE)
      )
    )
  )

# 3. Readable descriptive statistics table

descriptive_table <- descriptive_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "^\\[(.*)\\]_(Min|Q1|Median|Q3|Max|Mean|SD)$",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

descriptive_table

# 4. Missing data

missing_data <- data.frame(
  Variable   = names(data),
  Missing    = sapply(data, function(x) sum(is.na(x))),
  Percentage = sapply(data, function(x) mean(is.na(x)) * 100)
) %>%
  arrange(desc(Missing))

missing_data

missing_plot <- missing_data %>% filter(Missing > 0)

ggplot(missing_plot, aes(x = reorder(Variable, Percentage), y = Percentage)) +
  geom_col() +
  coord_flip() +
  labs(title = "Percentage of Missing Values",
       x = "Variable", y = "Missing Percentage (%)") +
  theme_minimal()

# 5. Creating call and put observations (long format)

calls <- data %>%
  transmute(
    QUOTE_DATE = `[QUOTE_DATE]`, EXPIRE_DATE = `[EXPIRE_DATE]`,
    STRIKE = `[STRIKE]`, OPTION_TYPE = "Call",
    PRICE = `[C_LAST]`, DTE = `[DTE]`,
    DELTA = `[C_DELTA]`, GAMMA = `[C_GAMMA]`, VEGA = `[C_VEGA]`,
    THETA = `[C_THETA]`, IV = `[C_IV]`, VOLUME = `[C_VOLUME]`,
    BID = `[C_BID]`, ASK = `[C_ASK]`,
    UNDERLYING_LAST = `[UNDERLYING_LAST]`,
    STRIKE_DISTANCE_PCT = `[STRIKE_DISTANCE_PCT]`
  )

puts <- data %>%
  transmute(
    QUOTE_DATE = `[QUOTE_DATE]`, EXPIRE_DATE = `[EXPIRE_DATE]`,
    STRIKE = `[STRIKE]`, OPTION_TYPE = "Put",
    PRICE = `[P_LAST]`, DTE = `[DTE]`,
    DELTA = `[P_DELTA]`, GAMMA = `[P_GAMMA]`, VEGA = `[P_VEGA]`,
    THETA = `[P_THETA]`, IV = `[P_IV]`, VOLUME = `[P_VOLUME]`,
    BID = `[P_BID]`, ASK = `[P_ASK]`,
    UNDERLYING_LAST = `[UNDERLYING_LAST]`,
    STRIKE_DISTANCE_PCT = `[STRIKE_DISTANCE_PCT]`
  )

options_long <- bind_rows(calls, puts)

dim(calls); dim(puts); dim(options_long)

# 5b. Summary statistics for main predictors (long-format data)

predictor_vars <- c("UNDERLYING_LAST", "DTE", "STRIKE", "STRIKE_DISTANCE_PCT",
                    "DELTA", "GAMMA", "VEGA", "THETA", "IV", "VOLUME",
                    "BID", "ASK", "PRICE")

predictor_table <- options_long %>%
  select(all_of(predictor_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N       = sum(!is.na(Value)),
    Missing = sum(is.na(Value)),
    Min     = min(Value, na.rm = TRUE),
    Q1      = quantile(Value, 0.25, na.rm = TRUE),
    Median  = median(Value, na.rm = TRUE),
    Q3      = quantile(Value, 0.75, na.rm = TRUE),
    Max     = max(Value, na.rm = TRUE),
    Mean    = mean(Value, na.rm = TRUE),
    SD      = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

predictor_table

# 6. Future return (single definition, percent units)

options_long <- options_long %>%
  arrange(OPTION_TYPE, STRIKE, EXPIRE_DATE, QUOTE_DATE) %>%
  group_by(OPTION_TYPE, STRIKE, EXPIRE_DATE) %>%
  mutate(
    NEXT_PRICE = lead(PRICE),
    NEXT_BID   = lead(BID),
    NEXT_ASK   = lead(ASK)
  ) %>%
  ungroup()

# 6a. Investigate zero next prices (current price positive)
zero_check <- options_long %>%
  filter(!is.na(PRICE), PRICE > 0, NEXT_PRICE == 0) %>%
  mutate(
    ZERO_TYPE = case_when(
      NEXT_BID == 0 & NEXT_ASK == 0 ~ "Genuine zero (bid = ask = 0)",
      NEXT_ASK > 0                  ~ "Unavailable last price (ask > 0)",
      TRUE                          ~ "Bid/ask missing - unclear"
    )
  )

table(zero_check$ZERO_TYPE)

# 6b. Current price must be > 0; next price must be > 0 or a genuine zero (-100%)
options_long <- options_long %>%
  mutate(
    GENUINE_ZERO = NEXT_PRICE == 0 & NEXT_BID == 0 & NEXT_ASK == 0,
    VALID_RETURN = !is.na(PRICE) & PRICE > 0 & !is.na(NEXT_PRICE) &
      (NEXT_PRICE > 0 | GENUINE_ZERO %in% TRUE),
    FUTURE_RETURN = if_else(
      VALID_RETURN,
      (NEXT_PRICE - PRICE) / PRICE * 100,
      NA_real_
    )
  )

# Single dataset used for ALL return statistics, plots, outliers, correlations
returns_valid <- options_long %>%
  filter(!is.na(FUTURE_RETURN), is.finite(FUTURE_RETURN))

nrow(returns_valid)
sum(returns_valid$FUTURE_RETURN == -100)   # genuine full-loss observations

# Display-only subset (-100% to +100%); no data deleted
plot_data <- returns_valid %>%
  filter(FUTURE_RETURN >= -100, FUTURE_RETURN <= 100)

# 6c. Histogram
ggplot(plot_data, aes(x = FUTURE_RETURN)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Future Option Returns",
       x = "Future Return (%)", y = "Frequency") +
  theme_minimal()

# 7. Cumulative distribution (ECDF)
ggplot(plot_data, aes(x = FUTURE_RETURN)) +
  stat_ecdf() +
  labs(title = "Cumulative Distribution of Future Option Returns",
       x = "Future Return (%)", y = "Cumulative Proportion") +
  theme_minimal()

# 8. Scatter plot - DTE vs Future Return
set.seed(726)
scatter_sample <- plot_data %>%
  filter(!is.na(DTE)) %>%
  slice_sample(n = min(10000, nrow(.)))

ggplot(scatter_sample, aes(x = DTE, y = FUTURE_RETURN)) +
  geom_point(alpha = 0.3) +
  labs(title = "Days to Expiration vs Future Option Return",
       x = "Days to Expiration", y = "Future Return (%)") +
  theme_minimal()

# 9. Scatter plot - Implied Volatility vs Future Return
set.seed(726)
iv_sample <- plot_data %>%
  filter(!is.na(IV)) %>%
  slice_sample(n = min(10000, nrow(.)))

ggplot(iv_sample, aes(x = IV, y = FUTURE_RETURN)) +
  geom_point(alpha = 0.3) +
  labs(title = "Implied Volatility vs Future Option Return",
       x = "Implied Volatility", y = "Future Return (%)") +
  theme_minimal()

# 10. Summary of future return (percent)
return_summary <- data.frame(
  Measure = c("Valid observations", "Minimum", "Q1", "Median", "Q3", "Mean",
              "Standard Deviation", "99th Percentile", "Maximum"),
  Value = c(
    nrow(returns_valid),
    min(returns_valid$FUTURE_RETURN),
    quantile(returns_valid$FUTURE_RETURN, 0.25),
    median(returns_valid$FUTURE_RETURN),
    quantile(returns_valid$FUTURE_RETURN, 0.75),
    mean(returns_valid$FUTURE_RETURN),
    sd(returns_valid$FUTURE_RETURN),
    quantile(returns_valid$FUTURE_RETURN, 0.99),
    max(returns_valid$FUTURE_RETURN)
  )
)
return_summary

# 11. Box plot for Future Return
ggplot(plot_data, aes(y = FUTURE_RETURN)) +
  geom_boxplot() +
  labs(title = "Box Plot of Future Option Returns", y = "Future Return (%)") +
  theme_minimal()

# 12. IQR outliers (same dataset, percent units)
Q1 <- quantile(returns_valid$FUTURE_RETURN, 0.25)
Q3 <- quantile(returns_valid$FUTURE_RETURN, 0.75)
IQR_value   <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

c(Q1 = Q1, Q3 = Q3, IQR = IQR_value, Lower = lower_bound, Upper = upper_bound)

n_iqr <- sum(returns_valid$FUTURE_RETURN < lower_bound |
               returns_valid$FUTURE_RETURN > upper_bound)
n_iqr
n_iqr / nrow(returns_valid) * 100

# 13. Z-score outliers (same dataset, percent units)

return_mean <- mean(returns_valid$FUTURE_RETURN)
return_sd   <- sd(returns_valid$FUTURE_RETURN)

returns_valid <- returns_valid %>%
  mutate(Z_SCORE = (FUTURE_RETURN - return_mean) / return_sd)

n_z <- sum(abs(returns_valid$Z_SCORE) > 3)
n_z
n_z / nrow(returns_valid) * 100

# 14. Spearman correlation with future return (same dataset)

cor_data <- returns_valid %>%
  select(FUTURE_RETURN, UNDERLYING_LAST, DTE, DELTA, GAMMA, VEGA, THETA,
         IV, VOLUME, BID, ASK, STRIKE, STRIKE_DISTANCE_PCT)

spearman_results <- sapply(
  cor_data[-1],
  function(x) cor(x, cor_data$FUTURE_RETURN,
                  method = "spearman", use = "complete.obs")
)
spearman_results <- sort(spearman_results, decreasing = TRUE)
spearman_results

cor_plot_data <- data.frame(
  Variable    = names(spearman_results),
  Correlation = as.numeric(spearman_results)
)

ggplot(cor_plot_data, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_col() +
  coord_flip() +
  labs(title = "Spearman Correlation with Future Option Return",
       x = "Predictor", y = "Spearman Correlation") +
  theme_minimal()

# 15. Predictor multicollinearity (same dataset)
predictors <- returns_valid %>%
  select(UNDERLYING_LAST, DTE, DELTA, GAMMA, VEGA, THETA,
         IV, VOLUME, BID, ASK, STRIKE, STRIKE_DISTANCE_PCT)

cor_matrix <- cor(predictors, use = "pairwise.complete.obs", method = "spearman")
round(cor_matrix, 2)

high_cor <- which(abs(cor_matrix) > 0.90 & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor

# 16. Later-date testing (chronological split)

all_dates <- sort(unique(returns_valid$QUOTE_DATE))
cutoff <- all_dates[floor(length(all_dates) * 0.70)]

train_data <- returns_valid %>% filter(QUOTE_DATE <= cutoff)
test_data  <- returns_valid %>% filter(QUOTE_DATE > cutoff)

min(train_data$QUOTE_DATE); max(train_data$QUOTE_DATE)
min(test_data$QUOTE_DATE);  max(test_data$QUOTE_DATE)