# Kaggle_Competition

### Instructions for the final submission:
The package MedMast is contained in MedMast folder.
In the same folder there is a subdirectory called ```vignettes```, which contains a vignette for reproducing step by step our best prediction (which is also contained in the script ```Final.R``` in the ```SCRIPTS``` folder).\
For the report please consider instead the folder ```Report```, in which there is a .pdf file with the analyses on the methods tried.

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
- Generalized rolling windows to any of the previous models

### Packages needed to run MedMast:
- glmnet
- assertthat
- HotDeckImputation
- randomForest
- caret
- e1071
- ranger 
- MASS
- adabag

