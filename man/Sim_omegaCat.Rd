\name{Sim_omegaCat}
\alias{Sim_omegaCat}
\title{Simulate and Compute Categorical Omega Reliability Coefficients}

\description{
The \code{Sim_omegaCat} function calculates the Omega reliability coefficients for categorical data across multiple fits in Structural Equation Modeling (SEM). It is designed to handle both ordinal and categorical scale data. This function simulates the reliability coefficients using the \pkg{semTools} package and returns a dataframe that summarizes these coefficients across specified latent factors.
}

\usage{
Sim_omegaCat(results, num_factors)
}

\arguments{
  \item{results}{A list containing multiple SEM model fits, where each fit is stored in an element of the list.}
  \item{num_factors}{An integer specifying the number of factors for which Omega reliability coefficients are to be calculated.}
}

\details{
Upon invocation, the function first checks for the necessary R packages (\pkg{semTools}, \pkg{tidyr}, and \pkg{dplyr}) and loads them. Each model fit is processed to compute Omega reliability coefficients for the given number of latent factors. Missing factors in any model results are assigned NA values to ensure consistency across outputs. Results are displayed in a tidy dataframe with factors as columns and corresponding reliability coefficients as rows.
}

\value{
A dataframe where each column represents a factor and each row contains the Omega reliability coefficients for that factor, calculated from the SEM model fits. This structured output makes it easy to compare reliability across factors and models.
}

\examples{
# Assuming 'model_results' contains the required SEM fits
omega_results <- Sim_omegaCat(model_results, 5)
print(omega_results)
}

\author{
Dr. José Ventura-León
}
