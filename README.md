# Kaggle_Competition

### Instructions for the interim submission:
The package MedMast is contained in MedMast folder.
For reproducing our current best score, consider the script Test.R in the SCRIPTS folder.

### Algorithms used:
- Knn
- Svm
- Random Forest with rolling windows
- Lasso Regression with generalized linear model (multinomial type)
- Boosting
- Ordinal regression

### Data analysis:
- Imputed missing values/Removed the outliers
- Standardized both continuous and categorical inputs
- Extracted most significant features using Fisher Scoring and/or Lasso


### Packages needed to run MedMast:
- glmnet
- assertthat
- HotDeckImputation
- randomForest

