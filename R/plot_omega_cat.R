plot_omega_cat <- function(compRel_results) {
  # Load required library
  if (!require("ggplot2")) {
    stop("Please install the 'ggplot2' package.")
  }
  if (!require("tidyr")) {
    stop("Please install the 'tidyr' package.")
  }
  compRel_results %>%
    as_tibble() %>%
    pivot_longer(cols = c(OmegaCat),
                 names_to = "Reliability", values_to = "value") %>%
    # Crea el boxplot
    ggplot(aes(x = Reliability, y = value)) +
    geom_boxplot(fill = "gray68", colour = "black") +
    theme_minimal() +
    labs(title = "Simulation of categorical omegas",
         x = "", y = "Reliability coefficient")
}
