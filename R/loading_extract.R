loading_extract <- function(results) {
  # Asegurarse de que las librerías necesarias estén cargadas
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Por favor instala el paquete 'lavaan'")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Por favor instala el paquete 'dplyr'")
  }
  if (!requireNamespace("pbapply", quietly = TRUE)) {
    stop("Por favor instala el paquete 'pbapply'")
  }

  # Asignar results$Fits a fit_list para simplificar el acceso
  fit_list <- results$Fits

  # Usar pblapply para procesar cada ajuste en fit_list con una barra de progreso
  results_list <- pblapply(fit_list, function(fit) {
    lavaan::standardizedsolution(fit) %>%
      dplyr::select(lhs, op, est.std, rhs) %>%
      dplyr::filter(op == "=~")
  })

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
