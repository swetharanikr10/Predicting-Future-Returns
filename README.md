# Predicting-Future-Returns
Predicting Future Returns and Optimizing SPY Options Selection 

Descriptive, predictive and prescriptive analysis in R of SPY options (Kaggle, Aug 2 – Sep 30, 2021). The question is whether historical quotes can predict which contracts gain value, and whether those predictions produce a profitable daily portfolio under capital, liquidity and concentration limits.

Data: 224,364 daily quotes were reshaped into 448,728 contract records, leaving 353,178 valid observations after cleaning. The data are split by date, with purged boundary rows.

Methods: zero-return baseline, linear regression, random forest and gradient boosting, validated with rolling-origin CV, then a daily linear program (lpSolve) over 13 test dates.

Key Findings
Gradient boosting was the best predictor. Test RMSE was 19.99 against 21.49 for the baseline (about 7% lower). Its top-decile contracts earned +5.41%, against −2.41% for all contracts. CV results were inconsistent, so the edge is promising but unproven.
The signal did not become profit. The optimized portfolio lost $35,752 (−2.82% per dollar), while the model had expected a $367,450 profit. Cash beat every invested strategy.
Likely cause: a stale-price target. The top predictor was the gap between the last traded price and the bid-ask midpoint. A buyer who pays the ask and sells at the bid cannot capture that move.
Robustness checks. The same trades still lose $30,468 with zero commission, and every exit used an observed next bid.
