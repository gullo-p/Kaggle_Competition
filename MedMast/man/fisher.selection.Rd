% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fisher_scoring.R
\name{fisher.selection}
\alias{fisher.selection}
\title{Fisher scoring for feature selection}
\usage{
fisher.selection(features, labels, n, threshold)
}
\arguments{
\item{features}{The dataframe containing the features.}

\item{labels}{The categorical labels (in our case we have 5 categories for popularity) 
according to which you want to measure the variability of each feature.}

\item{n}{The number of features with the highest score you want to select for your final model.}

\item{threshold}{The value to use as the threshold for converting the categorical labels to binary.}
}
\value{
A dataframe containing the selected features.
}
\description{
This function computes the Fisher scoring for each feature 
based on the binary version of the popularity label ("non-popular" being 0 and "popular" being 1) 
and ranks the Fisher scoring of the features in descending order.
}
\examples{
# create sample dataset
features <- matrix(rnorm(200), ncol=2)
labels <- c(rep(1, 40), rep(2, 40), rep(3,40), rep(4, 40), rep(5,40))
n <- 1
threshold <- 2 #categories above 2 will be set to be 1, otherwise 0.
# get the variable with the highest fisher scoring
fisher.selection(features, labels, n, threshold)
}

