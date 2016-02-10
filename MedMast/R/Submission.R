# ----------------------------------------------------------------------------------------------------
# SUBMISSION GENERATOR
# ----------------------------------------------------------------------------------------------------
#' Function for creating a csv in the right format for submission
#' @param fit A vector of length 9644 containing the predicted labels for the test dataset.
#' @param sub_numb An integer indicating the number of the generating submission.
#' @return A csv file in the working directory ready for submission named submitX.csv, with X=sub_numb.

submit <- function(fit, sub_numb){
   data <- data.frame(cbind(c(30001:39644), fit))
   colnames(data) <- c("id", "popularity")
   s1 <- paste("submit", sub_numb, sep = "")
   s2 <- ".csv"
   write.csv(data, file = paste(s1, s2, sep = ""), quote = FALSE, row.names = FALSE)
 }
