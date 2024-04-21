parameter_extractlavaan <- function(fit) {
  library(lavaan)
  library(dplyr)
  # Extraer la solución estandarizada y filtrar por los operadores de interés
  model_df <- lavaan::standardizedSolution(fit) %>%
    dplyr::filter(op %in% c("=~", "|")) %>%
    dplyr::select(lhs, op, rhs, est.std)

  # Asegurarse de que est.std es numérico
  model_df$est.std <- as.numeric(model_df$est.std)

  # Procesar las relaciones factoriales =~
  factor_loadings <- model_df %>%
    filter(op == "=~") %>%
    group_by(lhs) %>%
    summarize(model_part = paste(sprintf("%.3f*%s", est.std, rhs), collapse = " + ")) %>%
    mutate(full_model_part = paste(lhs, "=~", model_part)) %>%
    pull(full_model_part)

  model_string <- paste(factor_loadings, collapse = "\n")

  # Procesar los umbrales |
  if(any(model_df$op == "|")) {
    thresholds <- model_df %>%
      filter(op == "|") %>%
      mutate(model_part = paste(lhs, "|", sprintf("%.3f*%s", est.std, rhs))) %>%
      pull(model_part)

    # Añadir los umbrales al string del modelo si existen
    model_string <- paste(model_string, paste(thresholds, collapse = "\n"), sep = "\n")
  }

  return(trimws(model_string))
} # Cargas y umbrales
