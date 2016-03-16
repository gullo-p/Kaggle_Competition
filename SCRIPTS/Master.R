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

# add variable day
time.data$day <- as.factor(substring(time.data$url, 29,30))

# add the quarter variable
time.data$quarter <- ifelse(time.data$month < 4, 1,
                            ifelse(time.data$month < 7, 2,
                                   ifelse(time.data$month < 10, 3,4)))

# add how many articles in the same date
time.data$countsDay <- ave(time.data$timedelta, time.data$timedelta, FUN = length)


# Convert the response to ordinal
time.data$popularity <- factor(time.data$popularity)


# Add channel other
time.data$data_channel_is_other <- ifelse(time.data$data_channel_is_lifestyle == 0 &
                                            time.data$data_channel_is_entertainment == 0 &
                                            time.data$data_channel_is_socmed == 0 &
                                            time.data$data_channel_is_tech == 0 &
                                            time.data$data_channel_is_world == 0, 1,0)


#0.5350


# New variables according to words in the url
techBrands <- c("facebook|roomba|acer|google|3g|wikipedia|app|apple|ibm|ios|iphone|blackberry|xbox|playstation|
                ipad|windows|dell|pc|samsung|microsoft|lenovo|yahoo|asus|amazon|lg|gopro|hp|sony|bill-gates|htc
                |nintendo|nokia|beatsvbing|bitcoin|imac|kindle|drone")

gossip <- c("kyle|golden-globes|emmy|tom-cruise|megan-fox|celebrities|ryan-gosling|avril-lavigne|prince
            |harry|ashton-kutcher|kanye|oscars|megan|steve|john|zuckerberg|sex|oprah|angelina|katy-perry
            |lady-gaga|madonna|beyonce|brittney|michael-jackson|oscar|ashley|kardashian|taylor-swift|
            breaking-bad|justin-bieber|engagement|radcliffe|hollywood|teenage|proposal|hillary")

politics <- c("obama|militar|referendum|al-gore|cia|aids|fbi|washington|jihad|politic|migration
              |nuclear|wealth|iran|ebola|war|nasa|clinton|cnn|senator|riot|police|syria|bush|congress
              |usa|us-|russian|ukraine|protest|putin|white-house|world-war|abortion|law|interpol|fbi|independence|
              korea|europe|drugs|taliban|osama|marijuana|israel|conflict")

socialMedia <- c("facebook|evernote|myspace|twitter|instagram|snapchat|uber|vine|youtube|tinder
                 |pinterest|linkedin|airbnb|spotify|netflix|tumblr|shazam|foursquare") 

sports <- c("cup|ping-pong|crossfit|olympics|scoccer|nhl|clippers|football|rugby|fifa|suarez
            |skating|michael-jordan|ferguson|chelsea|nfl|messi|guardiola|baseball|basket|nba|nfl|golf|
            tiger-woods|lebron-james")

cars <- c("toyota|audi|car")

tvShows <- c("star-trek|sitcom|hbo|armstrong|tv-premier|doctor-who|how-i-met-your-mother|spoiler|
             tv-show|castle|season|thrones|true-blood|star-wars|potter|sons-of-anarchy|superman|sherlock|walter-white")


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

# PREDICTIONS
##########################################################################################
# Train and predict using random forest on Rolling windows
rf.predictions <- rolling.windows(dataset = time.data, step.size = 1000,
                                  FUN = my.forest, ntree = 300, mtry = 4 )

rf.predictions <- rf.predictions[,c(1,32)]
colnames(rf.predictions) <- c("id","popularity")

# call the text mining function for getting some more predictions
pred <- predictLabels(time.data)

# merge the two predictions 
final.prediction <- merge(pred, rf.predictions, by.x = "id", by.y = "id")
final.prediction$popularity <- ifelse(is.na(final.prediction$popularity.x), 
                                      final.prediction$popularity.y, final.prediction$popularity.x)

final.prediction$popularity.x <- NULL
final.prediction$popularity.y <- NULL

write.table(final.prediction,"submit.csv",sep=",",row.names = F)