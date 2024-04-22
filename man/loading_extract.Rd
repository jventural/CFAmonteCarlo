\name{loading_extract}
\alias{loading_extract}
\title{Extract and Summarize Factor Loadings from Lavaan Model Fits}

\description{
The \code{loading_extract} function processes a list of Lavaan model fits to extract and summarize standardized factor loadings. It is designed to handle multiple model outputs by extracting factor loadings, compiling them into a single dataframe, and then calculating descriptive statistics for each item across all models.
}

\usage{
loading_extract(results)
}

\arguments{
  \item{results}{A list containing a sub-list named \code{Fits}, which should include one or more Lavaan model fit objects.}
}

\details{
This function first checks for the necessary \pkg{lavaan} and \pkg{dplyr} packages and then proceeds to extract factor loadings ("=~") from each model fit provided in the \code{results} list. Each set of loadings is processed and summarized to provide mean, standard deviation, minimum, and maximum values for each item across all fits, aiding in the comprehensive analysis of factor loading consistency and variability.
}

\value{
A list with two elements:
\itemize{
  \item \code{Raw_Data}: A dataframe containing all extracted factor loadings.
  \item \code{Descriptive_Statistics}: A dataframe providing descriptive statistics for each item, including mean, standard deviation, minimum, and maximum of the standardized loadings.
}
These components are useful for detailed examination and comparison of model results, especially in large-scale simulation studies or comparative model evaluations.
}

\examples{
# Assuming 'results' is a preloaded list containing Lavaan fits
loading_results <- loading_extract(results)
print(loading_results$Raw_Data)        # Access the raw data dataframe
print(loading_results$Descriptive_Statistics)  # Access the dataframe with descriptive statistics
}

\author{
Dr. José Ventura-León
}
