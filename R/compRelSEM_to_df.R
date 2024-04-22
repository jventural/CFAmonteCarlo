compRelSEM_to_df <- function(results, num_factors) {
  library(pbapply)  # Ensure pbapply is installed and loaded
  if (!requireNamespace("semTools", quietly = TRUE)) {
    stop("Please install the 'semTools' package.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Please install the 'tidyr' package.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install the 'dplyr' package.")
  }

  # Generate a list of factor names based on the specified number of factors
  all_factors <- paste0("F", 1:num_factors)

  # Process each fit and return the result in a consistent format
  process_fit <- function(fit) {
    result <- semTools::compRelSEM(fit, tau.eq = FALSE, ord.scale = TRUE)
    # Ensure all factors are included even if not present in the result
    result <- setNames(result, names(result))
    missing_factors <- setdiff(all_factors, names(result))
    result[missing_factors] <- NA  # Assign NA to missing factors

    # Make sure the order of results matches the order of all_factors
    ordered_result <- setNames(vector("numeric", length(all_factors)), all_factors)
    ordered_result[names(result)] <- result

    return(as_tibble(t(ordered_result), .name_repair = "minimal"))
  }

  # Apply compRelSEM to each element of fit_list with a progress bar
  fit_list <- results$Fits
  results_list <- pblapply(fit_list, process_fit)

  # Combine the results into a single dataframe
  results_df <- dplyr::bind_rows(results_list)

  return(results_df)
}
