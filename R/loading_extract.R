loading_extract <- function(results) {
  # Asegurarse de que las librerías necesarias estén cargadas
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Por favor instala el paquete 'lavaan'")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Por favor instala el paquete 'dplyr'")
  }

  # Asignar results$Fits a fit_list para simplificar el acceso
  fit_list <- results$Fits

  # Lista para almacenar los resultados procesados
  results_list <- list()

  # Bucle que recorre cada elemento de fit_list y aplica las operaciones
  for (i in seq_along(fit_list)) {
    results <- lavaan::standardizedsolution(fit_list[[i]]) %>%
      dplyr::select(lhs, op, est.std, rhs) %>%
      dplyr::filter(op == "=~")

    # Guardar los resultados en la lista
    results_list[[i]] <- results
  }

  # Combinar todos los dataframes almacenados en la lista en un único dataframe
  final_results <- dplyr::bind_rows(results_list)

  # Crear un dataframe con las estadísticas descriptivas
  descriptive_stats <- final_results %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(rhs) %>%
    dplyr::rename(Item = rhs) %>%
    dplyr::summarise(
      Mean = mean(est.std, na.rm = TRUE),
      Std_Dev = sd(est.std, na.rm = TRUE),
      Min = min(est.std, na.rm = TRUE),
      Max = max(est.std, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Retornar ambos dataframes como una lista
  list(Raw_Data = final_results, Descriptive_Statistics = descriptive_stats)
}
