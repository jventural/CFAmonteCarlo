\name{extractFitMeasures}
\alias{extractFitMeasures}
\title{Extract Goodness-of-Fit Measures from SEM Model Fits}

\description{
The \code{extractFitMeasures} function extracts a specified set of goodness-of-fit measures from multiple structural equation modeling (SEM) fits. This function supports a thorough evaluation of model fit across various metrics, which is crucial for comparative analysis of SEM results in simulation studies or across multiple datasets.
}

\usage{
extractFitMeasures(results)
}

\arguments{
  \item{results}{A list containing a sub-list named \code{Fits}, which should include one or more SEM model fit objects processed by Lavaan.}
}

\details{
Upon verifying that the necessary \pkg{lavaan} package is installed, the function iterates through each model fit stored in the \code{results} list. It applies \code{fitMeasures} from the Lavaan package to extract key fit indices such as Chi-squared, SRMR, WRMR, CFI, TLI, and RMSEA. Each set of fit indices is compiled into a tidy format using a \code{tibble}, making it straightforward to compare and analyze across multiple replications or models.
}

\value{
A \code{tibble} containing the goodness-of-fit measures for each model fit. Each row corresponds to a different model fit, and columns represent different fit indices like Chi-squared, SRMR, WRMR, CFI, TLI, and RMSEA. An additional column 'Replica' indicates the model replication identifier.
}

\examples{
# Assuming 'results' is already prepared with multiple Lavaan model fits
fit_measures_results <- extractFitMeasures(results)
print(fit_measures_results)
}

\author{
Dr. José Ventura-León
}
