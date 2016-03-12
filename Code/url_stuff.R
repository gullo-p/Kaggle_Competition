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
#max(time.data$countsDay) - min(time.data$countsDay)/3
time.data$countsDay2 <- ifelse(time.data$countsDay < 32, 1,
                                ifelse(time.data$countsDay < 64, 2,3))
time.data$countsDay2 <- as.factor(time.data$countsDay2)

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

#altres <- c("")

time.data$techBrands <- grepl(techBrands,time.data$url)
time.data$politics <- grepl(politics,time.data$url)
time.data$gossip <- grepl(gossip,time.data$url)
time.data$socialMedia <- grepl(socialMedia,time.data$url)
time.data$sports <- grepl(sports,time.data$url)
time.data$cars <- grepl(cars,time.data$url)
time.data$tvShows <- grepl(tvShows,time.data$url)
#time.data$altres <- grepl(altres,time.data$url)

time.data$isPopular <- ifelse(time.data$techBrands == 0 &
                                time.data$politics == 0 &
                                time.data$gossip == 0 &
                                time.data$socialMedia == 0 &
                                time.data$sports == 0 &
                                time.data$cars == 0 &
                               # time.data$altres == 0 &
                                time.data$tvShows == 0, 0,1)

#p <- time.data[time.data$isPopular == 0, 2]
#p <- as.data.frame(p)
#round(prop.table(table(time.data$isPopular, time.data$popularity)) * 100, digits = 1)
#table(time.data$isPopular, time.data$popularity)


#################

#Obtain vectors of words
vec5 <- c()
vec4 <- c()
vec3 <- c()
vec2 <- c()
vec1 <- c()
time.data$urll <- NA
for (i in 1:nrow(time.data)) {
  n <- nchar(as.character(time.data[i,2]))
  word <- substr(as.character(time.data[i,2]), 32, n-1)
  time.data[i,'urll'] <- word
  if ( !is.na(time.data[i,'popularity'])) {
    if(as.numeric(time.data[i,'popularity']) == 5) {
      vec5 <- paste(vec5, word, sep = "-")
    } else if(as.numeric(time.data[i,'popularity']) == 4) {
      vec4 <- paste(vec4, word, sep = "-")
    } else if(as.numeric(time.data[i,'popularity']) == 3) {
      vec3 <- paste(vec3, word, sep = "-")
    } else if(as.numeric(time.data[i,'popularity']) == 2) {
      vec2 <- paste(vec2, word, sep = "-")
    } else if(as.numeric(time.data[i,'popularity']) == 1) {
      vec1 <- paste(vec1, word, sep = "-")
    }
  }
}

#Count number of words in url
time.data$wordsUrl <- sapply(time.data$urll, 
                             FUN=function(x) {as.numeric(length(unlist(strsplit(x, "-"))))})

# Convert from string to vector strings
vec5 <- unlist(strsplit(vec5, "-"))
vec4 <- unlist(strsplit(vec4, "-"))
vec3 <- unlist(strsplit(vec3, "-"))
vec2 <- unlist(strsplit(vec2, "-"))
vec1 <- unlist(strsplit(vec1, "-"))

#Remove first element ""
vec5 <- vec5[-c(1)]
vec4 <- vec4[-c(1)]
vec3 <- vec3[-c(1)]
vec2 <- vec2[-c(1)]
vec1 <- vec1[-c(1)]

#Remove the numbers
vec5 <- gsub(" [A-Za-z] ", "", gsub("[0-9]", "", vec5))
vec5 <- vec5[vec5 != ""]
vec4 <- gsub(" [A-Za-z] ", "", gsub("[0-9]", "", vec4))
vec4 <- vec4[vec4 != ""]
vec3 <- gsub(" [A-Za-z] ", "", gsub("[0-9]", "", vec3))
vec3 <- vec3[vec3 != ""]
vec2 <- gsub(" [A-Za-z] ", "", gsub("[0-9]", "", vec2))
vec2 <- vec2[vec2 != ""]
vec1 <- gsub(" [A-Za-z] ", "", gsub("[0-9]", "", vec1))
vec1 <- vec1[vec1 != ""]

# Vector of prepositions
prepositions <- c("is", "it", "for", "all", "to", "the", "from", "on", "off", "up", "and",
                  "into", "at", "of","about","a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                  "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

#Remove words that are prepositions
vec5 <- vec5[!vec5 %in% prepositions]
vec4 <- vec4[!vec4 %in% prepositions]
vec3 <- vec3[!vec3 %in% prepositions]
vec2 <- vec2[!vec2 %in% prepositions]
vec1 <- vec1[!vec1 %in% prepositions]

#Get words repetead among all popularity
repetits <- Reduce(intersect, list(vec4,vec3,vec2,vec1))

#Get words repetead popularity 1 and 2
repetits12 <- Reduce(intersect, list(vec2,vec1))

#Remove words contained in the repetits vector
vec5 <- vec5[!vec5 %in% repetits]
vec4 <- vec4[!vec4 %in% repetits]
vec3 <- vec3[!vec3 %in% repetits]
vec2 <- vec2[!vec2 %in% repetits]
vec1 <- vec1[!vec1 %in% repetits]

#Remove words contained in the repetits vector
vec5 <- vec5[!vec5 %in% repetits12]
vec4 <- vec4[!vec4 %in% repetits12]
vec3 <- vec3[!vec3 %in% repetits12]
vec2 <- vec2[!vec2 %in% repetits12]
vec1 <- vec1[!vec1 %in% repetits12]

# Get the 500 most popular words
vec5Pop <- as.character(na.omit(names(sort(table(vec5),decreasing=TRUE)[1:200])))
vec4Pop <- as.character(na.omit(names(sort(table(vec4),decreasing=TRUE)[1:200])))
vec3Pop <- as.character(na.omit(names(sort(table(vec3),decreasing=TRUE)[1:200])))
vec2Pop <- as.character(na.omit(names(sort(table(vec2),decreasing=TRUE)[1:200])))
vec1Pop <- as.character(na.omit(names(sort(table(vec1),decreasing=TRUE)[1:200])))

#Get the unique words for vector
vec5Unique <- as.character(unique(vec5))
vec4Unique <- as.character(unique(vec4))
vec3Unique <- as.character(unique(vec3))
vec2Unique <- as.character(unique(vec2))
vec1Unique <- as.character(unique(vec1))

#Change format to grepl
vecs5 <- paste(vec5Pop, collapse = '|')
vecs4 <- paste(vec4Pop, collapse = '|')
vecs3 <- paste(vec3Pop, collapse = '|')
vecs2 <- paste(vec2Pop, collapse = '|')
vecs1 <- paste(vec1Pop, collapse = '|')
repetits <- paste(repetits, collapse = '|')

#For each popularity, add column if it contains its popular words
time.data$cinc <- grepl(vecs5, time.data$urll)
time.data$quatre <- grepl(vecs4,time.data$urll)
time.data$tres <- grepl(vecs3,time.data$urll)
time.data$dos <- grepl(vecs2,time.data$urll)
time.data$un <- grepl(vecs1,time.data$urll)
time.data$repetits <- grepl(repetits,time.data$urll)

#Get the probability of each
time.data$pdos <- 0
time.data$pu <- 0
time.data$ptres <- 0
time.data$pquatre <- 0
time.data$pcinc <- 0
for (i in 1:nrow(time.data)) {
  words <- unlist(strsplit(time.data[i,'urll'], "-"))
  time.data[i,'pcinc'] <-  length((words %in% vec5Unique)[(words %in% vec5Unique)==TRUE])
  time.data[i,'pquatre'] <- length((words %in% vec4Unique)[(words %in% vec4Unique)==TRUE])
  time.data[i,'ptres'] <- length((words %in% vec3Unique)[(words %in% vec3Unique)==TRUE])
  time.data[i,'pdos'] <-  length((words %in% vec2Unique)[(words %in% vec2Unique)==TRUE])
  time.data[i,'pu'] <- length((words %in% vec1Unique)[(words %in% vec1Unique)==TRUE])
}

#Most suitable probability
time.data$testProb <-  apply(time.data[,which(colnames(time.data)=="pdos"):which(colnames(time.data)=="pcinc")],1,which.max)

# Remove urll
time.data$urll <- NULL


# PREDICTIONS
##########################################################################################

rf.predictions <- rolling.windows.rf(dataset = time.data, ntree = 200)

colnames(rf.predictions) <- c("id","popularity")

#write.table(submit,"submit0309.csv",sep=",",row.names = F)

submit <- rf.predictions
sum(popularity$popularity == submit$popularity)/length(submit$popularity)


