parameter_extractlavaan2 <- function(fit) {
  library(lavaan)
  library(dplyr)

  # Extraer la solución estandarizada y filtrar por los operadores de interés (incluyendo '~~')
  model_df <- lavaan::standardizedSolution(fit) %>%
    dplyr::filter(op %in% c("=~", "|", "~~")) %>%
    dplyr::select(lhs, op, rhs, est.std, se, z, pvalue, ci.lower, ci.upper)

  # Asegurarse de que est.std, se, z, pvalue, ci.lower y ci.upper son numéricos
  model_df$est.std <- as.numeric(model_df$est.std)
  model_df$se <- as.numeric(model_df$se)
  model_df$z <- as.numeric(model_df$z)
  model_df$pvalue <- as.numeric(model_df$pvalue)
  model_df$ci.lower <- as.numeric(model_df$ci.lower)
  model_df$ci.upper <- as.numeric(model_df$ci.upper)

  # Inicializar string del modelo
  model_string <- ""

  # Procesar las relaciones factoriales =~ y varianzas/covarianzas ~~
  for(op_type in unique(model_df$op)) {
    sub_df <- model_df %>% filter(op == op_type)

    if(op_type %in% c("=~", "~~")) {
      sub_df <- sub_df %>%
        group_by(lhs) %>%
        summarize(model_part = paste(sprintf("%.3f*%s", est.std, rhs), collapse = " + ")) %>%
        mutate(full_model_part = paste(lhs, op_type, model_part)) %>%
        pull(full_model_part)

      model_string <- paste(model_string, paste(sub_df, collapse = "\n"), sep = "\n")
    } else if(op_type == "|") {
      thresholds <- sub_df %>%
        mutate(model_part = paste(lhs, "|", sprintf("%.3f*%s", est.std, rhs))) %>%
        pull(model_part)

      model_string <- paste(model_string, paste(thresholds, collapse = "\n"), sep = "\n")
    }
  }

  return(trimws(model_string))
} #Cargas, umbrales y errores
