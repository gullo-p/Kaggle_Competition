create.target1 <- function(data){
  for(i in 1:nrow(data)){
    if(data$popularity ==1)
      data$target1 <- 1
    else
      data$target1 <- 0
  }
    
}

create.target2 <- function(data){
  for(i in 1:nrow(data)){
    if(data$popularity ==2)
      data$target2 <- 1
    else
      data$target2 <- 0
  }
  
}

create.target3 <- function(data){
  for(i in 1:nrow(data)){
    if(data$popularity ==3)
      data$target3 <- 1
    else
      data$target3 <- 0
  }
  
}

create.target4 <- function(data){
  for(i in 1:nrow(data)){
    if(data$popularity ==4)
      data$target4 <- 1
    else
      data$target4 <- 0
  }
  
}

create.target5 <- function(data){
  for(i in 1:nrow(data)){
    if(data$popularity ==5)
      data$target5 <- 1
    else
      data$target5 <- 0
  }
  
}
train$target1
create.target1(train)
create.target2(train)
create.target3(train)
create.target4(train)
create.target5(train)