# Chi-squared_RF_Toxicity
This code is to make RF model detecting Toxicity after Chi-squared test filtering

# Purpose
Occasionally there are too many columns to choose. So using Chi-squared test to target label "Toxicity", get specific columns P-value < 0.05 from the input data. Given columns with continuous variables, replace values greater than median to "High" and rest to "low". Adopting this idea, the model AUC socre is from 0.83 to 0.87.
