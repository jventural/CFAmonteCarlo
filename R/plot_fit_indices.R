plot_fit_indices <- function(fit_measures_results) {
  # Load required libraries
  if (!require("tidyr")) {
    stop("Please install the 'tidyr' package.")
  }
  if (!require("ggplot2")) {
    stop("Please install the 'ggplot2' package.")
  }
  if (!require("ggpubr")) {
    stop("Please install the 'ggpubr' package.")
  }

  # Define the plotting function for absolute and comparative fit indices
  plot_indices <- function(data, cols, title) {
    data %>%
      pivot_longer(cols = all_of(cols),
                   names_to = "variable", values_to = "value") %>%
      ggplot(aes(x = variable, y = value)) +
      geom_boxplot(fill = "gray68") +
      theme_minimal() +
      labs(title = title, x = "Variable", y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

  # Create the plots
  g1 <- plot_indices(fit_measures_results, c("srmr", "rmsea.scaled"), "Simulation of Absolute Fit Indexes")
  g2 <- plot_indices(fit_measures_results, c("cfi.scaled", "tli.scaled"), "Simulation of Comparative Fit Indexes")

  # Arrange the plots side by side
  Figure_gg <- ggpubr::ggarrange(g1, g2,
                                 labels = c("A", "B"),
                                 ncol = 2, nrow = 1)

  return(Figure_gg)
}
