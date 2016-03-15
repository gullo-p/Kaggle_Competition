library(MedMast)

# DATA INPUT

train <- read.csv("../DATA/news_popularity_training.csv", sep = ",")
test <- read.csv("../DATA/news_popularity_test.csv", sep = ",")

test$popularity <- NA

dataset <- rbind(train,test)

# DATA CLEANING
##########################################################################################
# remove outliers 
dataset <- dataset[-which(train$n_unique_tokens == 701),]

# Remove non-sense or redundant features: 
# Remove the almost constant column
dataset$n_non_stop_words <- NULL

# # Remove the rate negative_words
dataset$rate_negative_words <- NULL

dataset$flag <- 0
dataset$flag[is.na(dataset$popularity)] <- 1

# order the dataset by date
time.data <- dataset[order(dataset$timedelta,decreasing = TRUE),]
time.data$ts <- as.numeric(731 - time.data$timedelta)

# initialize a new feature which flags the missing values in the data
time.data$missing.flag <- 0
time.data$missing.flag[time.data$n_tokens_content == 0] <- 1
time.data$missing.flag[time.data$global_subjectivity == 0] <- 1

#Adding features
##########################################################################################

# add the date variables (year, month and is.holiday)
time.data <- obtain.date(time.data)

# add the quarter variable
time.data$quarter <- cut(time.data$ts,breaks=8,labels=1:8)

# Convert the response to ordinal
time.data$popularity <- factor(time.data$popularity, ordered = TRUE)

# Add channel other
time.data$data_channel_is_other <- ifelse(time.data$data_channel_is_lifestyle == 0 &
                                            time.data$data_channel_is_entertainment == 0 &
                                            time.data$data_channel_is_socmed == 0 &
                                            time.data$data_channel_is_tech == 0 &
                                            time.data$data_channel_is_world == 0, 1,0)

# Add variable day
time.data$day <- as.factor(substring(time.data$url, 29,30))

# Add how many post the same date
time.data$countsDay <- ave(time.data$timedelta, time.data$timedelta, FUN = length)

#Change quarter
time.data$quarter <- ifelse(time.data$month < 4, 1,
                            ifelse(time.data$month < 7, 2,
                                   ifelse(time.data$month < 10, 3,4)))

# New variables according to words in the url
techBrands <- c("facebook|roomba|acer|google|3g|wikipedia|app|apple|ibm|ios|iphone|blackberry|xbox|playstation|ipad|windows|dell|pc|samsung|microsoft|lenovo|yahoo|asus|amazon|lg|gopro|hp|sony|bill-gates|htc|nintendo|nokia|beatsvbing|bitcoin|imac|kindle|drone")

gossip <- c("kyle|golden-globes|emmy|tom-cruise|megan-fox|celebrities|ryan-gosling|avril-lavigne|prince|harry|ashton-kutcher|kanye|oscars|megan|steve|john|zuckerberg|sex|oprah|angelina|katy-perry|lady-gaga|madonna|beyonce|brittney|michael-jackson|oscar|ashley|kardashian|taylor-swift|breaking-bad|justin-bieber|engagement|radcliffe|hollywood|teenage|proposal|hillary")

politics <- c("obama|militar|referendum|al-gore|cia|aids|fbi|washington|jihad|politic|migration|nuclear|wealth|iran|ebola|war|nasa|clinton|cnn|senator|riot|police|syria|bush|congress|usa|us-|russian|ukraine|protest|putin|white-house|world-war|abortion|law|interpol|fbi|independence|korea|europe|drugs|taliban|osama|marijuana|israel|conflict")

socialMedia <- c("facebook|evernote|myspace|twitter|instagram|snapchat|uber|vine|youtube|tinder|pinterest|linkedin|airbnb|spotify|netflix|tumblr|shazam|foursquare") 

sports <- c("cup|ping-pong|crossfit|olympics|scoccer|nhl|clippers|football|rugby|fifa|suarez|skating|michael-jordan|ferguson|chelsea|nfl|messi|guardiola|baseball|basket|nba|nfl|golf|tiger-woods|lebron-james")

cars <- c("toyota|audi|super-bowl|car|aston-martin|mercedes|ford|nissan")

tvShows <- c("star-trek|sitcom|hbo|armstrong|tv-premier|doctor-who|how-i-met-your-mother|spoiler|tv-show|castle|season|thrones|true-blood|star-wars|potter|sons-of-anarchy|superman|sherlock|walter-white")


time.data$techBrands <- grepl(techBrands,time.data$url)
time.data$politics <- grepl(politics,time.data$url)
time.data$gossip <- grepl(gossip,time.data$url)
time.data$socialMedia <- grepl(socialMedia,time.data$url)
time.data$sports <- grepl(sports,time.data$url)
time.data$cars <- grepl(cars,time.data$url)
time.data$tvShows <- grepl(tvShows,time.data$url)

time.data$isPopular <- ifelse(time.data$techBrands == 0 &
                                time.data$politics == 0 &
                                time.data$gossip == 0 &
                                time.data$socialMedia == 0 &
                                time.data$sports == 0 &
                                time.data$cars == 0 &
                                time.data$tvShows == 0, 0,1)


# Obtain the vector of words 
##########################################################################################
time.data$urll <- NA
for (i in 1:nrow(time.data)) {
  n <- nchar(as.character(time.data[i,2]))
  word <- substr(as.character(time.data[i,2]), 32, n-1)
  time.data[i,'urll'] <- word
}


#Get vector of all words in the test data
vecTest <- c()
#Get vector of all words of the test
tes <- time.data[time.data$flag == 1,]
vecTest <- tes[,'urll']
vecTest <- unlist(strsplit(vecTest, "-"))[-1]


train.sample <- time.data[time.data$flag == 0,]
rows <- list()
numWords <- c()
for (i in 1:100) {
  rows[[i]] <- sample(1:nrow(train.sample), 22500)
  vecTrain <- train.sample[-rows[[i]],"urll"]  
  vecTrain <- unlist(strsplit(vecTrain, "-"))[-1]
  #Get how many words they share with the test data
  numWords[i] <- length(vecTest[vecTest %in% vecTrain])
}


#Obtain the train.test that share most words with the real test data
n <- which.max(numWords)
train.sample[-rows[[n]],"flag"] <- 1



# Predict the popularity
##########################################################################################

# Predict the popularity using the PredictLabels function
pred <- predictLabels(train.sample)

prev <- merge(train.sample, pred,  by.x = "id", by.y = "id")
check <- cbind(id = prev$id, real = prev$popularity.x, predicted = prev$popularity.y)
check <- as.data.frame(check)

# Check how many predictions are made compared to the total test
totalPredictions <- length(na.omit(check$predicted))
dataPredicted <- totalPredictions/nrow(train.sample[-rows[[n]],])*100

# Check how many of the predictions are correct
check$wellPredicted <- check$real == check$predicted
numWellPredicted <- sum(check$wellPredicted,na.rm=T)
percentage <- numWellPredicted/totalPredictions*100

# Check the table of true labels and predicted labels
table(check$real, check$predicted)


#### 29% of the test popularity has been predicted with an accuracy of 88%
### This means that it should be an improvement of the accuracy of at most 20%
### Even though no data is classified at 5, we a lot of 4 are obtained (we didn't obtain
### any when doing just the random forest).



# Run the whole algorithm
##########################################################################################

#Let's just check what would it be the real improvement running the whole script:

train.sample$urll <- NULL
rf.predictions2 <- rolling.windows.rf(dataset = train.sample, ntree = 200)

colnames(rf.predictions2) <- c("id","popularity")


# Check the improvement
##########################################################################################
tes <- train.sample[train.sample$flag == 1,]

sum(tes$popularity == rf.predictions2$popularity)/length(tes$popularity)

# 0.42 of the data is correctly predicted

#Now, using our function of predictLabels:
prev <- cbind(rf.predictions2, pred$popularity)

check <- cbind(id = rf.predictions2$id, real = tes$popularity,  pred1 = rf.predictions2$popularity, pred2 = prev$`pred$popularity`)
check <- as.data.frame(check)


# Check how many predictions are made compared to the total test
totalPredictions <- length(na.omit(check$pred2))
dataPredicted <- totalPredictions/nrow(train.sample[-rows[[n]],])*100

# Check how many of the predictions are correct
check$wellPredicted <- check$pred1 == check$pred2
numWellPredicted <- sum(check$wellPredicted,na.rm=T)
percentage <- numWellPredicted/totalPredictions*100

# Check the table of true labels and predicted labels
table(check$pred1, check$pred2)
table(check$real, check$pred2)

check$final <- ifelse(is.na(check$pred2), check$pred1, check$pred2)
sum(tes$popularity == check$final)/length(tes$popularity)
