\name{plot_fit_indices}
\alias{plot_fit_indices}
\title{Plot Boxplots for Goodness-of-Fit Measures}

\description{
The \code{plot_fit_indices} function visualizes the distribution of selected goodness-of-fit measures from SEM model fits. It creates boxplots for both absolute and comparative fit indices, providing a clear graphical representation that is useful for examining variations and trends across multiple SEM analyses.
}

\usage{
plot_fit_indices(fit_measures_results)
}

\arguments{
  \item{fit_measures_results}{A dataframe containing goodness-of-fit measures for multiple SEM model fits. Columns must include 'srmr', 'rmsea.scaled', 'cfi.scaled', and 'tli.scaled' for the function to operate correctly.}
}

\details{
The function internally defines a plotting routine that converts specified columns of the input dataframe into a long format and then generates boxplots for these variables. It arranges the boxplots of absolute fit indices and comparative fit indices side by side for convenient comparison. The required libraries 'tidyr', 'ggplot2', and 'ggpubr' must be installed for the function to work.
}

\value{
An object of class \code{ggarrange} containing the arranged boxplots. The output can be displayed using the print function or saved to a file using ggsave or similar.
}

\examples{
# Assuming 'fit_measures_results' is a dataframe with the required goodness-of-fit measures
fit_plots <- plot_fit_indices(fit_measures_results)
print(fit_plots)
}

\author{
Dr. José Ventura-León
}

