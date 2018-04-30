#' Function for VarClus.
#'
#' A function that automates the process of collecting non-correlated metrics after the application of varclus.
#' @param Collect non-correlated metrics, Defaults to dataset, sw.metrics, threshold of varclus (>0.7)
#' @keywords sw.metrics, varclus.threshold, output
#' @export
#' @examples
#' applyVarClus()

applyVarClus <- function(dataset, sw.metrics, VarClus.threshold) {
  print('Variable Clustering : START')
  output <- {
  }
  ran <- {
  }
  correlated <- {
  }
  mitigated.metrics <- {
  }
  vc <-
    varclus(
      ~ . - Type_1 - Type_3,
      similarity = 'spearman',
      data = dataset,
      trans = "abs",
      na.action = na.omit
    )
  
  # Apply cutoff threshold
  var.clustered <-
    cutree(vc$hclust, h = (1 - VarClus.threshold))
  
  # Get surviving metrics
  non.correlated.metrics <-
    names(var.clustered)[var.clustered %in% names(table(var.clustered)[table(var.clustered) == 1])]
  print(paste0(
    length(non.correlated.metrics),
    ' non-correlated metrics : ',
    paste0(non.correlated.metrics, collapse = ', ')
  ))
  
  # Get correlated clusters index
  correlated.clusters.index <-
    names(table(var.clustered)[table(var.clustered) > 1])
  
  print(paste0((length(sw.metrics) - length(non.correlated.metrics)),
               ' correlated metrics from ',
               length(correlated.clusters.index),
               ' clusters'
  ))
  
  # For each cluster of correlated metrics, print out for manual selection
  cluster.count <- 1
  for (cluster.index in correlated.clusters.index) {
    print(paste0(
      'Cluster ',
      cluster.count,
      ' : ',
      paste0(names(var.clustered)[var.clustered == cluster.index], collapse = ', ')
    ))
    output <- rbind(output,
                    data.frame(
                      CID = cluster.count,
                      CorrelatedMetrics = paste0(names(var.clustered)[var.clustered == cluster.index], collapse = ',')
                    ))
    correlated <-
      c(correlated, names(var.clustered)[var.clustered == cluster.index])
    cluster.count <- cluster.count + 1
    
  }
  RandSelCorr <-
    strsplit(names(table(
      output$CorrelatedMetrics
    )[table(output$CorrelatedMetrics) == 1]), ",")
  for (i in 1:length(RandSelCorr))
  {
    ran[i] <- sample(RandSelCorr[[i]], 1, replace = TRUE)
  }
  print(
    'Random Selection of highly correlated metrics from each Clusters'
  )
  ran <- as.character(ran)
  print(ran)
  mitigated.metrics <- c(non.correlated.metrics, ran)
  mitigated.metrics <- as.character(mitigated.metrics)
  print('Collection of software metrics after 1st iteration of Varclus')
  print(mitigated.metrics)
  print('Variable Clustering : END')
  return (
    list(
      correlated.cluster = output,
      correlated.metrics = correlated,
      randomselection = ran,
      MM = mitigated.metrics
      
    )
  )
}