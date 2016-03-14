


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

#Remove words that are prepositions
vec <- vec[!vec %in% prepositions]

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

# Clean the vectors
################################################################################

# Get vectors of most frequent elements  (most freq)
vr5 <- names(head(sort(table(vec5),decreasing=TRUE), n  = 20)) 
vr4 <- names(head(sort(table(vec4),decreasing=TRUE), n  = 25)) # 2 high classes
vr3 <- names(head(sort(table(vec3),decreasing=TRUE), n  = 11)) # 2 high classes
vr2 <- names(head(sort(table(vec2),decreasing=TRUE), n  = 2)) # 2 high classes

# Remove frequent most frequent words per class
vec5 <- vec5[!vec5 %in% vr5]
vec4 <- vec4[!vec4 %in% vr4]
vec3 <- vec3[!vec3 %in% vr3]
vec2 <- vec2[!vec2 %in% vr2]


# Remove verbs
################################################################################

verbs <- read.csv("../DATA/verbs.csv", sep="")
verbs <- as.vector(verbs$x)

vec4 <- vec4[!vec4 %in% verbs]
vec3 <- vec3[!vec3 %in% verbs]
vec2 <- vec2[!vec2 %in% verbs]
vec1 <- vec1[!vec1 %in% verbs]


# Remove adjectives
################################################################################

adjectives <- read.csv("../DATA/adjectives.csv", sep="")
adjectives <- as.vector(adjectives$x)


vec4 <- vec4[!vec4 %in% adjectives]
vec3 <- vec3[!vec3 %in% adjectives]
vec2 <- vec2[!vec2 %in% adjectives]
vec1 <- vec1[!vec1 %in% adjectives]


# Remove common nouns
################################################################################
nouns <- read.csv("../DATA/nouns.csv", sep="")
nouns <- as.vector(nouns$x)

vec4 <- vec4[!vec4 %in% nouns]
vec3 <- vec3[!vec3 %in% nouns]
vec2 <- vec2[!vec2 %in% nouns]
vec1 <- vec1[!vec1 %in% nouns]


# Get the words with higher probabilities
################################################################################

#Get words repetead among all popularity
vr4 <- vec4[c(13, 33, 36, 47, 70, 76, 95, 138)]
vec4 <- vec4[vec4 %in% vr4]
vec3 <- vec3[!vec3 %in% vr4]
vec2 <- vec2[!vec2 %in% vr4]
vec1 <- vec1[!vec1 %in% vr4]


# For class 3: remove high frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr2 <- names(head(sort(table(vec2),decreasing=TRUE), n = 6)) 
vr1 <- names(head(sort(table(vec1),decreasing=TRUE), n = 3)) 

# Remove frequent most frequent words per class
vec3 <- vec3[!vec3 %in% vr2]
vec3 <- vec3[!vec3 %in% vr1]


# For class 2: remove high frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr3 <- names(head(sort(table(vec3),decreasing=TRUE), n = 3)) 
vr1 <- names(head(sort(table(vec1),decreasing=TRUE), n = 2)) 

# Remove frequent most frequent words per class
vec2 <- vec2[!vec2 %in% vr3]
vec2 <- vec2[!vec2 %in% vr1]


# For class 1: remove high frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr3 <- names(head(sort(table(vec3),decreasing=TRUE), n = 1)) 
vr2 <- names(head(sort(table(vec2),decreasing=TRUE), n = 4))

# Remove frequent most frequent words per class
vec1 <- vec1[!vec1 %in% vr3]
vec1 <- vec1[!vec1 %in% vr2]


# For class 3: remove low frequencies of other classes
################################################################################

# Get vectors of words with lower frequencies
vr2 <- names(head(sort(table(vec2),decreasing=FALSE), n = 29)) 
vr1 <- names(head(sort(table(vec1),decreasing=FALSE), n = 14)) 

# Remove frequent most frequent words per class
vec3 <- vec3[!vec3 %in% vr2]
vec3 <- vec3[!vec3 %in% vr1]


# For class 1: remove low frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr3 <- names(head(sort(table(vec3),decreasing=FALSE), n = 11)) 
vr2 <- names(head(sort(table(vec2),decreasing=FALSE), n = 1)) 

# Remove frequent most frequent words per class
vec1 <- vec1[!vec1 %in% vr3]
vec1 <- vec1[!vec1 %in% vr2]


# Assign the most probable labels to its classes
################################################################################
# Get vectors of words with higher frequencies
vr3 <- names(head(sort(table(vec3),decreasing=TRUE), n = 1)) 
vr2 <- names(head(sort(table(vec2),decreasing=TRUE), n = 1)) 
vr1 <- names(head(sort(table(vec1),decreasing=TRUE), n = 1)) 

# Remove frequent most frequent words per class
vec1 <- vec1[!vec1 %in% vr3]
vec1 <- vec1[!vec1 %in% vr2]
vec2 <- vec2[!vec2 %in% vr1]
vec2 <- vec2[!vec2 %in% vr3]
vec3 <- vec3[!vec3 %in% vr1]
vec3 <- vec3[!vec3 %in% vr2]

# For class 1: remove high frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr2 <- names(head(sort(table(vec2),decreasing=TRUE), n = 4)) 
vr3 <- names(head(sort(table(vec3),decreasing=TRUE), n = 1))

# Remove frequent most frequent words per class
vec1 <- vec1[!vec1 %in% vr2]
vec1 <- vec1[!vec1 %in% vr3]


# Class 3: Assign higher probabilities
################################################################################

# Get vectors of words with higher frequencies
vr2 <- names(head(sort(table(vec2),decreasing=TRUE)))[6] 
vr1 <- names(head(sort(table(vec1),decreasing=TRUE)))[4]  

# Remove frequent most frequent words per class
vec1 <- vec1[!vec1 %in% vr2]
vec1 <- vec1[!vec1 %in% vr1]
vec2 <- vec2[!vec2 %in% vr2]
vec2 <- vec2[!vec2 %in% vr1]


# Class 2: Assign higher probabilities
################################################################################

# Get vectors of words with higher frequencies
vr3 <- names(head(sort(table(vec3),decreasing=TRUE)))[4]
vr1 <- names(head(sort(table(vec1),decreasing=TRUE)))[3]

# Remove frequent most frequent words per class
vec1 <- vec1[!vec1 %in% vr3]
vec1 <- vec1[!vec1 %in% vr1]
vec3 <- vec3[!vec3 %in% vr1]
vec3 <- vec3[!vec3 %in% vr3]


# Class 1: Assign higher probabilities
################################################################################

# Get vectors of words with higher frequencies
vr3 <- names(head(sort(table(vec3),decreasing=TRUE)))[2]
vr2 <- names(head(sort(table(vec2),decreasing=TRUE)))[5]

# Remove frequent most frequent words per class
vec2 <- vec2[!vec2 %in% vr2]
vec2 <- vec2[!vec2 %in% vr3]
vec3 <- vec3[!vec3 %in% vr2]
vec3 <- vec3[!vec3 %in% vr3]



# For class 3: remove high frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr2 <- names(head(sort(table(vec2),decreasing=TRUE), n = 14)) 
vr1 <- names(head(sort(table(vec1),decreasing=TRUE), n = 13)) 

# Remove frequent most frequent words per class
vec3 <- vec3[!vec3 %in% vr2]
vec3 <- vec3[!vec3 %in% vr1]


# For class 2: remove high frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr3 <- names(head(sort(table(vec3),decreasing=TRUE), n = 3)) 
vr1 <- names(head(sort(table(vec1),decreasing=TRUE), n = 2)) 

# Remove frequent most frequent words per class
vec2 <- vec2[!vec2 %in% vr3]
vec2 <- vec2[!vec2 %in% vr1]


# For class 1: remove high frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr3 <- names(head(sort(table(vec3),decreasing=TRUE), n = 1)) 
vr2 <- names(head(sort(table(vec2),decreasing=TRUE), n = 5)) 

# Remove frequent most frequent words per class
vec1 <- vec1[!vec1 %in% vr3]
vec1 <- vec1[!vec1 %in% vr2]


# For class 3: remove low frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr2 <- names(head(sort(table(vec2),decreasing=FALSE), n = 29)) 
vr1 <- names(head(sort(table(vec1),decreasing=FALSE), n = 44)) 

# Remove frequent most frequent words per class
vec3 <- vec3[!vec3 %in% vr2]
vec3 <- vec3[!vec3 %in% vr1]


# For class 2: remove low frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr3 <- names(head(sort(table(vec3),decreasing=FALSE), n = 2))
vr1 <- names(head(sort(table(vec1),decreasing=FALSE), n = 3)) 

# Remove frequent most frequent words per class
vec3 <- vec3[!vec3 %in% vr3]
vec3 <- vec3[!vec3 %in% vr1]
vec1 <- vec1[!vec1 %in% vr3]
vec1 <- vec1[!vec1 %in% vr1]


# For class 1: remove low frequencies of other classes
################################################################################

# Get vectors of words with higher frequencies
vr3 <- names(head(sort(table(vec3),decreasing=FALSE), n = 11)) 
vr2 <- names(head(sort(table(vec2),decreasing=FALSE), n = 1)) 

# Remove frequent most frequent words per class
vec1 <- vec1[!vec1 %in% vr3]
vec1 <- vec1[!vec1 %in% vr2]


# For class 3: clean vector
################################################################################

# Get vectors of words with higher frequencies
vr2 <- names(head(sort(table(vec2),decreasing=FALSE), n = 29)) 
vr1 <- names(head(sort(table(vec1),decreasing=FALSE), n = 44)) 

# Remove frequent most frequent words per class
vec3 <- vec3[!vec3 %in% vr2]
vec3 <- vec3[!vec3 %in% vr1]

           
# Final vectors
################################################################################
vc1 <- unique(vec1)
vc2 <- unique(vec2)
vc3 <- unique(vec3)
vc4 <- unique(vec4)

x <- unlist(list(vc1,vc2,vc3,vc4))
rep <- unique(x[duplicated(x)])

vc4 <- vc4[!vc4 %in% rep]
vc3 <- vc3[!vc3 %in% rep]
vc2 <- vc2[!vc2 %in% rep]
vc1 <- vc1[!vc1 %in% rep]

  ########
match(vc4, v4)
match(vc3, v3)
match(vc2, v2)
match(vc1, v1)

#137
#348
#55
#8

# 491
