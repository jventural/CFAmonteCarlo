\name{parameter_extractlavaan2}
\alias{parameter_extractlavaan2}
\title{Extract and Format Detailed Standardized Parameters for Lavaan Models}

\description{
This function extends the capabilities of parameter extraction for Lavaan models by including factor loadings, thresholds, and covariances/variances ('~~'). It also extracts and formats additional statistical measures such as standard errors, z-values, p-values, and confidence intervals for each parameter, making it highly useful for detailed statistical reporting and analysis in structural equation modeling.
}

\usage{
parameter_extractlavaan2(fit)
}

\arguments{
  \item{fit}{An object of class \code{lavaan}, typically the result of fitting a structural equation model using the \code{lavaan} package.}
}

\details{
The function retrieves a standardized solution from the \code{fit} object, filtering for relevant model operators. It processes factor loadings, thresholds, and covariances/variances, formatting these estimates with precision and including statistical measures for comprehensive evaluation. Each parameter type is handled distinctly to maintain clarity in the output, which is formatted as a textual string suitable for inclusion in reports or presentations.
}

\value{
A character string that includes formatted factor loadings, thresholds, covariances/variances along with their corresponding standard errors, z-values, p-values, and confidence intervals. This string is designed for easy readability and immediate use in statistical analysis documentation.
}

\examples{
# Assuming 'fit' is a previously estimated Lavaan model object
detailed_model_string <- parameter_extractlavaan2(fit)
print(detailed_model_string)
}

\author{
Dr. José Ventura-León
}
