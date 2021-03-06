% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rolling.windows.generic.R
\name{rolling.windows}
\alias{rolling.windows}
\title{Rolling windows}
\usage{
rolling.windows(dataset, window.size = 10000, step.size = 1000,
  FUN = my.forest, ...)
}
\arguments{
\item{dataset}{A dataset comprising of training and test data}

\item{window.size}{An integer which establishes the size of each chunk of the rolling windows.}

\item{step.size}{An integer which defines the distance between the starting observations for two consecutive windows.}

\item{FUN}{The function to which you want to apply the rolling window technique (can be one among the 
other my.functions present in the package).}

\item{...}{Additional parameter passed to FUN}
}
\value{
A dataframe containing the id's and the predicted.
}
\description{
This function fits a desired model with a rolling windows technique, i.e., 
the dataset is split in chunks of fixed size, were each chunk starts from the observation
which follows the starting observation of the previous chunk 
(this is why is called rolling windows).
In each split of the data we predict a portion of the test data labels.
The final prediction is given then by a majority vote over all the predictions made.
}

