\name{parameter_extractlavaan}
\alias{parameter_extractlavaan}
\title{Extract and Format Standardized Loadings and Thresholds for Lavaan Models}

\description{
This function extracts standardized solution parameters for specified models using the Lavaan package, focusing on factor loadings ("=~") and thresholds ("|"). It processes these parameters to create a formatted model string suitable for reporting or further analysis. This utility is particularly useful in structural equation modeling workflows where interpretation of model estimates is essential.
}

\usage{
parameter_extractlavaan(fit)
}

\arguments{
  \item{fit}{An object of class \code{lavaan}, typically the result of a \code{lavaan} function call that fits a structural equation model to data.}
}

\details{
The function begins by extracting the standardized solution from the \code{fit} object. It filters for factor loadings and thresholds, formats these estimates to a precision of three decimal places, and then combines them into a single text representation. Factor loadings are presented in a regression-style format, while thresholds are handled separately if they exist. This formatted string is helpful for quick visual inspections or inclusion in reports.
}

\value{
A character string representing the formatted factor loadings and thresholds. The string is ready for presentation or further processing and is trimmed of any leading or trailing whitespace.
}

\examples{
# Assuming that 'fit' is a Lavaan model object already estimated
model_string <- parameter_extractlavaan(fit)
print(model_string)
}

\author{
Dr. José Ventura-León
}

