#' Function to check VIF of metrics.
#'
#' Function to check Variance Inflation Factor of the non-correlated metrics, before it is trained using a model. 
#' @param Obtain metrics with high VIF value (> 5), Defaults to sw.metrics, vif threshold, dataset.
#' @keywords sw.metrics, vif threshold, dataset
#' @export
#' @examples  
#' applyVIF()


applyVIF <- function(dataset,
                     sw.metrics,
                     defect,
                     VIF.threshold) {
  print('Variance Inflation Factor : START')
  glm.model <-
    glm(as.formula(paste(
      defect, '~', paste0(sw.metrics, collapse = '+')
    )),
    data = dataset,
    family = binomial())
  VIF <- rms::vif(glm.model)
  inter.correlated.metrics <- names(VIF[VIF >= VIF.threshold])
  mitigated.metrics <-
    sw.metrics[!(sw.metrics %in% inter.correlated.metrics)]
  
  print(paste0('nMetrics : ', length(sw.metrics)))
  print(paste0(
    'Mitigated metrics (',
    length(mitigated.metrics),
    '): ',
    paste0(mitigated.metrics, collapse = ', ')
  ))
  print(paste0(
    'Inter-correlated metrics (',
    length(inter.correlated.metrics),
    '): ',
    paste0(inter.correlated.metrics, collapse = ', ')
  ))
  
  # return original software metrics if all of the metrics are removed by VIF
  if (length(mitigated.metrics) == 0) {
    return(sw.metrics[sw.metrics %in% sw.metrics])
  }
  
  # reapply VIF to ensure that there is no presence of inter-correlated metrics
  if (length(inter.correlated.metrics) != 0) {
    mitigated.metrics <-
      applyVIF(dataset, mitigated.metrics, defect, VIF.threshold)
  }
  
  print('Variance Inflation Factor : END')
  return(sw.metrics[sw.metrics %in% mitigated.metrics])
  
}