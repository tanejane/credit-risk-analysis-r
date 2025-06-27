# credit-risk-classification-model-r
A data-driven analysis of loan duration and installment commitment in predicting customer credit risk using classification models such as Random Forest and GBM.

This project explores how **loan duration** and **installment commitment** influence the **credit risk classification** of bank customers. 
Using dataset, we:
- Performed exploratory data analysis (EDA)
- Built classification models (Random Forest, GBM)
- Evaluated metrics (AUC, Accuracy, Log-loss, etc.)
- Identified thresholds to reduce bad risk cases

## Objectives
- Analyze credit risk distribution across different financial profiles
- Identify predictive patterns using statistical and ML models
- Recommend actionable loan strategies to minimize bad risks

## Tools
- RStudio
- ggplot2, dplyr, randomForest, caret, gbm

## Summary
- Bad risk customers have longer durations (~24.3 months) and higher installment (~3.15%)
- Random Forest AUC: 82.76%
- GBM AUC: 86.5%
- Optimal threshold: 0.38
- Recommendations: Prefer shorter loans (<20 months) and installment commitment <3%


