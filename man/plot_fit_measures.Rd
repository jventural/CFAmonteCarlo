\name{plot_fit_measures}
\alias{plot_fit_measures}
\title{Plot Histograms of Goodness-of-Fit Measures with Empirical and Simulated Means}

\description{
The \code{plot_fit_measures} function creates histograms for a range of goodness-of-fit measures extracted from SEM model fits and overlays these with vertical lines representing both empirical and simulated mean values. This visualization aids in the comparative analysis of model fit across different SEM models and replications, highlighting the distribution and central tendencies of fit indices.
}

\usage{
plot_fit_measures(fit_measures_results, bondades)
}

\arguments{
  \item{fit_measures_results}{A dataframe containing goodness-of-fit measures for multiple SEM model fits.}
  \item{bondades}{A dataframe with empirical goodness-of-fit measures, which will be compared against those obtained from the simulated results.}
}

\details{
The function begins by reshaping the input dataframe into a long format suitable for ggplot2 faceting. It then calculates the mean values of simulated results for each fit measure and maps empirical mean values from the \code{bondades} dataframe. These means are then plotted as vertical lines on the corresponding histograms to visually assess how empirical data compares against simulated distributions. The function assumes that \code{bondades} includes empirical data for the same measures present in \code{fit_measures_results}.
}

\value{
A \code{ggplot} object that includes a series of histograms, each corresponding to a different goodness-of-fit measure, with overlaid vertical lines for the empirical and simulated means, facilitating the evaluation of model fit quality.
}

\examples{
# Assuming 'fit_measures_results' and 'bondades' are already prepared with the required data
plot <- plot_fit_measures(fit_measures_results, bondades)
print(plot)
}

\author{
Dr. José Ventura-León
}
