\name{MonteCarloSimCFA}
\alias{MonteCarloSimCFA}
\title{Monte Carlo Simulation for Confirmatory Factor Analysis (CFA)}

\description{
The \code{MonteCarloSimCFA} function conducts Monte Carlo simulations to perform Confirmatory Factor Analysis (CFA) using specified model parameters and settings. This function is designed to generate multiple data sets based on a defined model, apply CFA to each set, and collect the results, thereby facilitating the assessment of model robustness and the evaluation of estimator performance under controlled conditions.
}

\usage{
MonteCarloSimCFA(parameters_model, n_replicas, model_cfa, sample.nobs = 1000,
                 estimator = "WLSMV", ordered = TRUE, seed = 2023)
}

\arguments{
  \item{parameters_model}{The parameters defining the model from which data will be simulated.}
  \item{n_replicas}{The number of simulated data sets to generate and analyze.}
  \item{model_cfa}{The CFA model specification to be fit to each simulated data set.}
  \item{sample.nobs}{The number of observations for each simulated data set. Default is 1000.}
  \item{estimator}{The estimator to be used in the CFA analysis. Default is "WLSMV".}
  \item{ordered}{Logical, indicating whether the variables are ordered. Default is TRUE.}
  \item{seed}{The seed for the random number generator to ensure reproducibility. Default is 2023.}
}

\details{
The function initializes by setting a global seed for reproducibility. It then generates data sets using a progression of seeds to ensure variation across simulations. Each data set is analyzed using the specified CFA model and estimator, with results collected for further analysis. This procedure allows for testing the stability and suitability of CFA models under various simulated conditions, which is particularly useful in methodological research and teaching.
}

\value{
A list containing two elements:
\itemize{
  \item \code{Fits}: A list of \code{lavaan} objects representing the CFA fits for each replica.
  \item \code{Data}: The list of data frames corresponding to each generated data set.
}
This structure provides comprehensive outputs for detailed examination of model behavior and estimator performance across simulations.
}

\examples{
results <- MonteCarloSimCFA(parameters_model = parameter_extract2,
                            n_replicas = 1000,
                            model_cfa = model,
                            sample.nobs = 300,
                            estimator = "WLSMV",
                            ordered = TRUE,
                            seed = 1234)
}

\author{
Dr. José Ventura-León
}
