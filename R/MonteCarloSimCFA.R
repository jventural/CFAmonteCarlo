MonteCarloSimCFA <- function(parameters_model, n_replicas, model_cfa, sample.nobs = 1000,
                             estimator = "WLSMV", ordered = TRUE, seed = 2023) {
  library(pbapply)

  # Establecer una semilla global para reproducibilidad
  set.seed(seed)

  # Función para generar datos simulados para una réplica
  generate_data <- function(i) {
    set.seed(seed + i)  # La semilla de cada réplica depende de la semilla global y del índice de la réplica
    simulateData(parameters_model, sample.nobs = sample.nobs, standardized = TRUE)
  }

  # Generar muestras aleatorias con diferentes seeds utilizando pblapply para mostrar una barra de progreso
  # con un mensaje personalizado
  message("Data generation")
  data_list <- pblapply(1:n_replicas, generate_data)

  # Función para aplicar CFA a un conjunto de datos
  run_cfa <- function(data) {
    fit <- cfa(model_cfa,
               data = data,
               estimator = estimator,
               mimic = "Mplus",
               ordered = ordered)
    return(fit)
  }

  # Aplicar la función run_cfa a cada conjunto de datos en data_list utilizando pblapply para mostrar una barra de progreso
  # con un mensaje personalizado durante los análisis CFA
  message("Calculating CFA models")
  fit_list <- pblapply(data_list, run_cfa)

  # Retorna una lista con los ajustes de modelo y los datos generados
  return(list(Fits = fit_list, Data = data_list))
}
