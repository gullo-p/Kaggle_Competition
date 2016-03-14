# ----------------------------------------------------------------------------------------------------
# PREDICT FINAL LABELS THROUGH TEXT MINING
# ----------------------------------------------------------------------------------------------------
#' @title Predict final labels through text mining
#' 
#' @description This function predicts the final popularity based on the strings contained in the different
#' urls. It obtains the words that appear in both, train and test data, then it applies some common words
#' removal (most common words in english, verbs, and adjectives) and finally, removal of those words that 
#' appear in more than one class. After this, it returns a data frame containing all the test data
#' and its predicted popularity (NA otherwise).  
#' 
#' @param time.data The dataframe containing the train and test data
#' @return A dataframe containing the predictions (id and popularity)
#' @export
#' @import 
#' @examples 
#' # create sample dataset
#' time.data <- dataset[order(dataset$timedelta,decreasing = TRUE),]
#' # obtain the predicted popularity for the test data
#' predictLabels(time.data)
#' 
predictLabels <- function(time.data) {
  
  # Obtain vectors of words: train
  ################################################################################
  
  #Obtain vectors of words for train
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
  
  
  # Convert from string to vector strings
  vec5 <- unlist(strsplit(vec5, "-"))[-1]
  vec4 <- unlist(strsplit(vec4, "-"))[-1]
  vec3 <- unlist(strsplit(vec3, "-"))[-1]
  vec2 <- unlist(strsplit(vec2, "-"))[-1]
  vec1 <- unlist(strsplit(vec1, "-"))[-1]
  
  
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
  

  # Obtain vectors of words: test
  ################################################################################
  
  vec <- c()
  #Get vector of all words of the test
  t <- time.data[time.data$flag == 1,]
  for (i in 1:nrow(t)) {
    word <- t[i,'urll']
    vec <- paste(vec, word, sep = "-")
  }
  # Convert from string to vector strings
  vec <- unlist(strsplit(vec, "-"))
  
  #Remove first element ""
  vec <- vec[-c(1)]
  
  # Remove the 4000 (10%) most popular words
  rep2 <- names(head(sort(table(vec),decreasing=TRUE), n  = 4000))
  vec <- vec[!vec %in% rep2]
  
  # Vector of letters
  prepositions <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j","k", "l", "m", "n", 
                    "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
  
  #Remove words that are letters
  vec <- vec[!vec %in% prepositions]
  
  # Remove numbers
  temp <- gregexpr("[0-9]+", vec)
  n <- max(as.numeric(unique(unlist(regmatches(vec, temp)))))
  num <- as.character(c(0:n))
  vec <- vec[!vec %in% num]
  
  vec <- unique(vec)
  
  # Obtain vectors of words that are both in train and test
  ################################################################################
  
  # Keep only those words which are also in the test
  tes <- Reduce(intersect, list(vec5,vec))
  vec5 <- vec5[vec5 %in% tes]
  tes <- Reduce(intersect, list(vec4,vec))
  vec4 <- vec4[vec4 %in% tes]
  tes <- Reduce(intersect, list(vec3,vec))
  vec3 <- vec3[vec3 %in% tes]
  tes <- Reduce(intersect, list(vec2,vec))
  vec2 <- vec2[vec2 %in% tes]
  tes <- Reduce(intersect, list(vec1,vec))
  vec1 <- vec1[vec1 %in% tes]
  
  
  # Remove verbs
  ################################################################################
  
  verbs <- read.csv("../DATA/verbs.csv", sep="")
  verbs <- as.vector(verbs$x)
  
  vec5 <- vec5[!vec5 %in% verbs]
  vec4 <- vec4[!vec4 %in% verbs]
  vec3 <- vec3[!vec3 %in% verbs]
  vec2 <- vec2[!vec2 %in% verbs]
  vec1 <- vec1[!vec1 %in% verbs]
  
  
  # Remove adjectives
  ################################################################################
  
  adjectives <- read.csv("../DATA/adjectives.csv", sep="")
  adjectives <- as.vector(adjectives$x)
  
  vec5 <- vec5[!vec5 %in% adjectives]
  vec4 <- vec4[!vec4 %in% adjectives]
  vec3 <- vec3[!vec3 %in% adjectives]
  vec2 <- vec2[!vec2 %in% adjectives]
  vec1 <- vec1[!vec1 %in% adjectives]
  
  
  # Remove common nouns
  ################################################################################
  nouns <- read.csv("../DATA/nouns.csv", sep="")
  nouns <- as.vector(nouns$x)
  
  vec5 <- vec5[!vec5 %in% nouns]
  vec4 <- vec4[!vec4 %in% nouns]
  vec3 <- vec3[!vec3 %in% nouns]
  vec2 <- vec2[!vec2 %in% nouns]
  vec1 <- vec1[!vec1 %in% nouns]
  
  
  
  # Get the highest frequency for each vector and remove it from other classes
  ################################################################################
  
  vr2 <- names((which(table(vec2) == max(table(vec2)))))
  vec5 <- vec5[!vec5 %in% vr2]
  vec4 <- vec4[!vec4 %in% vr2]
  vec3 <- vec3[!vec3 %in% vr2]
  vec1 <- vec1[!vec1 %in% vr2]
  
  vr1 <- names((which(table(vec1) == max(table(vec1)))))
  vec5 <- vec5[!vec5 %in% vr1]
  vec4 <- vec4[!vec4 %in% vr1]
  vec3 <- vec3[!vec3 %in% vr1]
  vec2 <- vec2[!vec2 %in% vr1]
  
  vr3 <- names((which(table(vec3) == max(table(vec3)))))
  vec5 <- vec5[!vec5 %in% vr3]
  vec4 <- vec4[!vec4 %in% vr3]
  vec2 <- vec2[!vec2 %in% vr3]
  vec1 <- vec1[!vec1 %in% vr3]
  
  vr4 <- names((which(table(vec4) == max(table(vec4)))))
  vec5 <- vec5[!vec5 %in% vr4]
  vec3 <- vec3[!vec3 %in% vr4]
  vec2 <- vec2[!vec2 %in% vr4]
  vec1 <- vec1[!vec1 %in% vr4]
  
  #Not applied to class 5 given that it only has 1 word
  
  
             
  # Final vectors
  ################################################################################
  
  # Get unique words
  vr1 <- unique(vec1)
  vr2 <- unique(vec2)
  vr3 <- unique(vec3)
  vr4 <- unique(vec4)
  vr5 <- unique(vec5)
  
  # Remove words that are in more than one classification
  v <- intersect(vr1, vr2)
  vr2 <- vr2[!vr2 %in% v]
  vr1 <- vr1[!vr1 %in% v]
  
  v <- intersect(vr1, vr3)
  vr3 <- vr3[!vr3 %in% v]
  vr1 <- vr1[!vr1 %in% v]
  
  v <- intersect(vr2, vr3)
  vr3 <- vr3[!vr3 %in% v]
  vr2 <- vr2[!vr2 %in% v]
  
  
  # Create the predicted labels
  ################################################################################
  
  m1 <- time.data[time.data$flag == 1,]
  
  # Compute the prob of belonging to a class
  m1$four <- FALSE
  m1$three <- FALSE
  m1$two <- FALSE
  m1$one <- FALSE
  m1$label <- NA
  for(i in 1:nrow(m1)) {
    words <- unlist(strsplit(m1[i,"urll"], "-"))
    m1[i,"four"] <- any(words %in% vr4)
    m1[i,"three"] <- any(words %in% vr3)
    m1[i,"two"] <- any(words %in% vr2)
    m1[i,"one"] <- any(words %in% vr1)
  }
  
  #Get the final label
  for(i in 1:nrow(m1)) {
    if(m1[i,"four"]) m1[i,"label"] <- 4
    if(m1[i,"three"]) m1[i,"label"] <- 3
    if(m1[i,"two"]) m1[i,"label"] <- 2
    if(m1[i,"one"]) m1[i,"label"] <- 1
  }
  
  pred <- as.data.frame(cbind(id = m1$id, popularity = m1$label))
  pred <- pred[with(pred, order(id)), ]
  return(pred)
}

