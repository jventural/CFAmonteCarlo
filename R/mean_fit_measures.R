mean_fit_measures <- function(fit_measures_results) {
  # Load required library
  library(dplyr)

  # Check if the input dataframe has all the required columns
  required_columns <- c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled")
  missing_columns <- setdiff(required_columns, names(fit_measures_results))

  if (length(missing_columns) > 0) {
    stop("The input dataframe is missing the following required columns: ", paste(missing_columns, collapse = ", "))
  }

  # Calculate the mean for each of the specified fit measures
  summary_results <- fit_measures_results %>%
    summarise(
      mean_chisq.scaled = mean(chisq.scaled, na.rm = TRUE),
      mean_df.scaled = mean(df.scaled, na.rm = TRUE),
      mean_srmr = mean(srmr, na.rm = TRUE),
      mean_wrmr = mean(wrmr, na.rm = TRUE),
      mean_cfi.scaled = mean(cfi.scaled, na.rm = TRUE),
      mean_tli.scaled = mean(tli.scaled, na.rm = TRUE),
      mean_rmsea.scaled = mean(rmsea.scaled, na.rm = TRUE)
    )

  return(summary_results)
}
