plot_loading_sim <- function(loading_results) {
  library(ggplot2)
  library(dplyr)
  # Ensure the 'loading_results' contains the expected 'Raw_Data'
  if (!"Raw_Data" %in% names(loading_results)) {
    stop("The input 'loading_results' must contain an element named 'Raw_Data'.")
  }

  # Calculate the minimum value of 'est.std' from the data
  min_est_std <- min(loading_results$Raw_Data$est.std, na.rm = TRUE)

  # Proceed with the plotting
  loading_results$Raw_Data %>%
    mutate(est.std = round(est.std,2)) %>%
    rename(Items = rhs) %>%
    ggplot(aes(x = Items, y = est.std)) +
    geom_boxplot(fill = "gray68") +
    theme_minimal() +
    labs(title = "Simulation of factor loadings", x = "Variable", y = "Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(
      limits = c(round(min_est_std,2), 1.00),
      breaks = seq(round(min_est_std,2), 1.00, by = 0.04)
    )
}
