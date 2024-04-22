

\name{compRelSEM_to_df}
\alias{compRelSEM_to_df}
\title{Compute and Summarize Composite Reliability for SEM Models}

\description{
The \code{compRelSEM_to_df} function computes the composite reliability (such as Omega) for structural equation modeling (SEM) fits stored in a list. It applies the \code{compRelSEM} function from the \pkg{semTools} package to each model fit, facilitating the assessment of reliability across multiple models. This function is particularly useful in large-scale studies where consistency across multiple SEM analyses needs to be evaluated.
}

\usage{
compRelSEM_to_df(results)
}

\arguments{
  \item{results}{A list containing a sub-list named \code{Fits}, which should include one or more SEM model fit objects.}
}

\details{
This function first ensures the required \pkg{semTools} package is installed and loaded. It then iterates over each SEM model fit provided in the \code{results} list, applying the \code{compRelSEM} function with specific parameters set (tau.eq = FALSE, ord.scale = TRUE) to each. The results are aggregated into a single dataframe, with each row representing the composite reliability for one model fit, named 'OmegaCat'.
}

\value{
A dataframe where each row corresponds to the composite reliability of each SEM model fit processed. The dataframe has a single column 'OmegaCat', which holds the computed reliability values.
}

\examples{
# Assuming 'results' is a preloaded list containing SEM fits
compRel_results <- compRelSEM_to_df(results)
print(compRel_results)
}

\author{
Dr. José Ventura-León
}
