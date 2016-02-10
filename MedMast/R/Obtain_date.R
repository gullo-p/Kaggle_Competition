#obtain date & holiday variables
#New years, Martin Luther, Washington's Birthday, Memorial day, Independence, labor, Columbus
#Veterans, Thanksgiving, Christmas

obtain.date <- function(dataset){
  
  library(assertthat)
  # test the inputs
  not_empty(dataset);
  
  dates = c("2013-01-01","2013-01-21","2013-02-18" , "2013-05-27", "2013-07-04", 
            "2013-09-02", "2013-10-14", "2013-11-11", "2013-11-28", 
            "2013-12-25", "2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26", 
            "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27" ,
            "2014-12-25")
  myholidays  <- as.Date(dates,format ="%Y-%m-%d")
  
  year <- as.numeric(substring(dataset$url, 21,24))
  month <- as.numeric(substring(dataset$url, 26,27))
  day <- as.numeric(substring(dataset$url, 29,30))
  date <- paste(year,month,day, sep = "-")
  date <- as.Date(date)
  is_holiday <- rep(0,length(year))
  is_holiday[which(date %in% myholidays)] <- 1
  
  updated.frame <- data.frame(dataset, year = year,month = month,is_holiday = as.numeric(is_holiday))
  
  return(updated.frame)
  
}