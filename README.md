# Kaggle_Competition

### Instructions for the final submission:
The package MedMast is contained in MedMast folder.
In the same folder there is a subdirectory called ```vignettes```, which contains a vignette for reproducing step by step our best prediction (which is also contained in the script ```Final.R``` in the ```SCRIPTS``` folder).\
For the report please consider instead the folder ```Report```, in which there is a .pdf file with the analyses on the methods tried.

### IMPORTANT! Instructions for MedMast:
Some of the team members have experienced troubles when installing MedMast.
We think that this might be due to the R version locally installed (with the latest version of R everything seems to work).
Thus, we provide some lines of code to be run in case any problem of dependencies between packages would happen:
```{r, eval =FALSE}
install.packages("lme4")  #first install the following package
packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz"   #get the url of the other package
install.packages(packageurl, repos=NULL, type="source")  #install it.
```

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
- gbm

