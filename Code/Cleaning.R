setwd("/Users/santhoshnarayanan/Documents/Semester_2/15D012_dv_Computational_Methods/Kaggle_Competition/Kaggle_Competition/Data")

train <- read.csv("news_popularity_training.csv", sep = ",")
test <- read.csv("news_popularity_test.csv", sep = ",")
test$popularity <- NA

dataset <- rbind(train,test)

features <- dataset[,c(4:61)]
labels <- as.factor(dataset$popularity)

if(!require("mice")) install.packages("mice"); library(mice)
if(!require("HotDeckImputation")) install.packages("HotDeckImputation"); library(HotDeckImputation)
# Remove the constant column
features$n_non_stop_words <- NULL
# Remove the ukrain outlier
features <- features[-which(features$n_unique_tokens > 1),]
# Recode the missing values
features$n_unique_tokens[features$n_tokens_content == 0] <- NA
features$n_non_stop_unique_tokens[features$n_tokens_content == 0] <- NA
features$num_hrefs[features$n_tokens_content == 0] <- NA
features$num_self_hrefs[features$n_tokens_content == 0] <- NA
features$average_token_length[features$n_tokens_content == 0] <- NA
features$n_tokens_content[features$n_tokens_content == 0] <- NA
# Recode the missing values
features$global_sentiment_polarity[features$global_subjectivity == 0] <- NA
features$global_rate_positive_words[features$global_subjectivity == 0] <- NA
features$global_rate_negative_words[features$global_subjectivity == 0] <- NA
features$rate_positive_words[features$global_subjectivity == 0] <- NA
features$rate_negative_words[features$global_subjectivity == 0] <- NA
features$avg_positive_polarity[features$global_subjectivity == 0] <- NA
features$avg_negative_polarity[features$global_subjectivity == 0] <- NA
features$min_positive_polarity[features$global_subjectivity == 0] <- NA
features$min_negative_polarity[features$global_subjectivity == 0] <- NA
features$max_positive_polarity[features$global_subjectivity == 0] <- NA
features$max_negative_polarity[features$global_subjectivity == 0] <- NA
features$global_subjectivity[features$global_subjectivity == 0] <- NA
# Hot deck Imputation
imp.features <- impute.NN_HD(DATA=features,distance="eukl")