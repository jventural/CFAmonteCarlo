\name{plot_fit_indices}
\alias{plot_fit_indices}
\title{Plot Boxplots for Goodness-of-Fit Measures Across Specified Factors}

\description{
  The \code{plot_fit_indices} function visualizes the distribution of selected goodness-of-fit measures from SEM model fits, considering different factors specified by the user. It creates boxplots for both absolute and comparative fit indices across a number of factors, providing a clear graphical representation that is useful for examining variations and trends across multiple SEM analyses.
}

\usage{
  plot_fit_indices(fit_measures_results, num_factors)
}

\arguments{
  \item{fit_measures_results}{A dataframe containing goodness-of-fit measures for multiple SEM model fits. Columns must include 'srmr', 'rmsea.scaled', 'cfi.scaled', and 'tli.scaled' for the function to operate correctly.}
  \item{num_factors}{An integer specifying the number of factors across which the fit measures are analyzed and plotted.}
}

\details{
  The function first verifies that the specified number of factors is present in the results and then processes the data by converting the relevant columns of the input dataframe into a long format. It generates boxplots for each variable split by the factors, arranging the boxplots of absolute fit indices and comparative fit indices side by side for convenient comparison. Libraries 'tidyr', 'ggplot2', and 'ggpubr' are required for the operation of this function.
}

\value{
  An object of class \code{ggarrange} containing the arranged boxplots for the specified number of factors. The output can be displayed using the print function or saved to a file using ggsave or similar.
}

\examples{
  # Assuming 'fit_measures_results' is a dataframe with the required goodness-of-fit measures
  # and the dataframe includes data for multiple factors
  fit_plots <- plot_fit_indices(fit_measures_results, 3)  # Example for 3 factors
  print(fit_plots)
}

\author{
  Dr. José Ventura-León
}
