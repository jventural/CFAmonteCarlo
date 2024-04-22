\name{plot_omega_cat}
\alias{plot_omega_cat}
\title{Plot Boxplots for Categorical Omega Reliability Coefficients}

\description{
The \code{plot_omega_cat} function visualizes the distribution of Omega reliability coefficients, typically used in SEM (Structural Equation Modeling) to assess the reliability of latent variables. This function produces a boxplot that offers insights into the variation and central tendencies of these coefficients across different SEM analyses or simulation runs.
}

\usage{
plot_omega_cat(compRel_results)
}

\arguments{
  \item{compRel_results}{A dataframe containing the Omega reliability coefficients for multiple SEM model fits. The dataframe must include a column named 'OmegaCat'.}
}

\details{
Upon loading, the function first checks if the necessary libraries, \pkg{tidyr} and \pkg{ggplot2}, are installed. It then reshapes the dataframe from wide to long format using \pkg{tidyr}'s \code{pivot_longer} function, organizing the data for plotting. Finally, \pkg{ggplot2} is utilized to create the boxplot, which is styled minimally to emphasize the data itself.
}

\value{
An object of class \code{ggplot}, representing a boxplot of the categorical Omega reliability coefficients. This plot is useful for quickly assessing the distribution and identifying any outliers or anomalies in reliability measurements.
}

\examples{
# Assuming 'compRel_results' is a dataframe with the required 'OmegaCat' column
omega_plot <- plot_omega_cat(compRel_results)
print(omega_plot)
}

\author{
Dr. José Ventura-León
}
