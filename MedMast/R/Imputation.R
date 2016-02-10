# Hot deck Imputation: output is a new data frame with imputed features with url attached
imp.features <- impute.NN_HD(DATA=features[,-c(1:3)],distance="eukl")

imp.features <- data.frame(url = features$url, imp.features)