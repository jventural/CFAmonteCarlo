compRelSEM_to_df <- function(results) {
  library(pbapply) # Asegúrate de tener pbapply instalado y cargado
  if (!requireNamespace("semTools", quietly = TRUE)) {
    stop("Por favor instala el paquete 'semTools'")
  }

  # Asignar results$Fits a fit_list para simplificar el acceso
  fit_list <- results$Fits

  # Función para aplicar compRelSEM a un elemento de fit_list y retornar el resultado
  process_fit <- function(fit) {
    result <- semTools::compRelSEM(fit, tau.eq = FALSE, ord.scale = TRUE)
    return(result)
  }

  # Usar pblapply para aplicar compRelSEM a cada elemento de fit_list con una barra de progreso
  results_list <- pblapply(fit_list, process_fit)

  # Convertir la lista de resultados en un data.frame
  results_df <- do.call(rbind, results_list)

  # Cambiar el nombre de la columna a 'OmegaCat'
  colnames(results_df) <- "OmegaCat"

  # Devolver el data.frame resultante
  return(results_df)
}
