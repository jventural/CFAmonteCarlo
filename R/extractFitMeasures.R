extractFitMeasures <- function(results) {
  # Asegúrate de que las librerías necesarias están cargadas y disponibles
  library(pbapply)
  library(tibble)
  library(dplyr)
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Por favor instala el paquete 'lavaan'")
  }

  # Asignar results$Fits a fit_list para simplificar el acceso
  fit_list <- results$Fits

  # Define los nombres de las medidas que deseas extraer
  measure_names <- c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled")

  # Función para aplicar fitMeasures a un elemento de fit_list y retornar el resultado
  extract_measures <- function(fit) {
    measures <- fitMeasures(fit, measure_names)
    return(measures)
  }

  # Usar pblapply para aplicar fitMeasures a cada elemento de fit_list con una barra de progreso
  results_list <- pblapply(fit_list, extract_measures)

  # Combina los resultados en un único tibble
  results_tibble <- bind_rows(results_list, .id = "Replica")

  return(results_tibble)
}
