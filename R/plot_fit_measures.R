plot_fit_measures <- function(fit_measures_results, bondades) {
  # Load required libraries
  library(tidyr)
  library(ggplot2)
  library(dplyr)

  # Reshape the dataframe to long format
  df_long <- fit_measures_results %>%
    pivot_longer(
      cols = c(srmr, wrmr, cfi.scaled, tli.scaled, rmsea.scaled),
      names_to = "Measure", values_to = "Value"
    )

  # Calculate means for each measure to add as vertical lines
  mean_values <- df_long %>%
    group_by(Measure) %>%
    summarise(Mean = mean(Value, na.rm = TRUE))

  # Assuming 'original_values' comes from the 'bondades' parameter
  original_values <- data.frame(bondades) %>%
    tibble::rownames_to_column("Measure") %>%
    rename(Mean = bondades) %>%
    filter(Measure != "chisq.scaled" & Measure != "df.scaled")

  # Plot histograms with facets and add a vertical line for the mean
  plot <- ggplot(df_long, aes(x = Value)) +
    geom_histogram(bins = 30, fill = "gray68", color = "black") +
    facet_wrap(~ Measure, scales = "free") +
    geom_vline(data = mean_values, aes(xintercept = Mean, linetype = "Simulated"), size = 0.5) +
    geom_vline(data = original_values, aes(xintercept = Mean, linetype = "Empirical"), size = 0.5) +
    labs(title = "Histograms of Fit Measures", x = "Value", y = "Frequency") +
    theme_bw() +
    scale_linetype_manual(
      name = "Type",
      values = c("Simulated" = "dashed", "Empirical" = "solid")
    ) +
    scale_color_manual(
      name = "",
      values = setNames(rainbow(length(unique(df_long$Measure))), unique(df_long$Measure)),
      guide = FALSE
    ) +
    theme(legend.position = "right")

  return(plot)
}
