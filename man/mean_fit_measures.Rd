
\name{mean_fit_measures}
\alias{mean_fit_measures}
\title{Calculate Mean Goodness-of-Fit Measures from SEM Model Fits}

\description{
The \code{mean_fit_measures} function calculates the arithmetic mean for a set of goodness-of-fit measures from multiple structural equation modeling (SEM) fits. This summary function provides a concise overview of model performance indicators, essential for the meta-analysis of SEM model fits.
}

\usage{
mean_fit_measures(fit_measures_results)
}

\arguments{
  \item{fit_measures_results}{A dataframe containing goodness-of-fit measures for multiple SEM model fits. It is expected to include specific columns named 'chisq.scaled', 'df.scaled', 'srmr', 'wrmr', 'cfi.scaled', 'tli.scaled', and 'rmsea.scaled'.}
}

\details{
On execution, the function checks the provided dataframe for the presence of the required columns. If any are missing, an error is raised indicating which columns need to be present. It then proceeds to calculate the mean of each column, disregarding any missing values (NA). The resulting means provide a quick statistical summary of the central tendency for each goodness-of-fit measure, which is helpful when comparing different models or when synthesizing results across studies.
}

\value{
A dataframe with a single row containing the mean values of the specified goodness-of-fit measures, facilitating their interpretation and comparison across different SEM analyses.
}

\examples{
# Assuming 'fit_measures_results' is a dataframe with the required goodness-of-fit measures
mean_results <- mean_fit_measures(fit_measures_results)
print(mean_results)
}

\author{
Dr. José Ventura-León
}
