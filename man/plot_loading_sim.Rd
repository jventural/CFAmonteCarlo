\name{plot_loading_sim}
\alias{plot_loading_sim}
\title{Plot Simulation Results of Factor Loadings}

\description{
The \code{plot_loading_sim} function visualizes the distribution of factor loadings from multiple simulations or model fits. It takes standardized factor loadings, processes them, and displays them in a boxplot format to assess the spread and central tendency across different items or variables. This function is ideal for evaluating the consistency and variability of factor loadings across simulations.
}

\usage{
plot_loading_sim(loading_results)
}

\arguments{
  \item{loading_results}{A list containing an element named \code{Raw_Data} which includes a dataframe with the standardized factor loadings (est.std) and corresponding items (rhs).}
}

\details{
Upon validating the presence of the necessary \pkg{ggplot2} and \pkg{dplyr} packages, the function first checks that \code{loading_results} contains the expected structure. It then proceeds to calculate the minimum value of \code{est.std} for setting dynamic plot limits, ensuring the plot is adequately scaled to show all data points effectively. The factor loadings are rounded for clarity in presentation and plotted using \code{ggplot2} to create a visually appealing and informative boxplot.
}

\value{
An object of class \code{ggplot}, which is a boxplot visualizing the spread and central tendencies of the factor loadings for each item. The y-axis is dynamically scaled based on the data provided.
}

\examples{
# Assuming 'loading_results' is already prepared and contains the required data structure
plot_loading_sim(loading_results)
}

\author{
Dr. José Ventura-León
}

