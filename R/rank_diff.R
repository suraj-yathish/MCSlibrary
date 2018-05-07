#' Function to collect Rank Differences between two metrics in Type_1 and Type_3 models.
#' 
#' A function that automates the process of deleting the highly correlated metrics after the application of varclus.
#' @param Collect Rank Difference of various metrics in T1 and T3 models.  
#' @keywords skT1, skT3, rank 
#' @export
#' @examples 
#' rank_diff() 

rank_diff <- function(skT1, skT3, i)
{
  m <- {
    
  }
  n <- {
    
  }
  m <- names(skT1$groups)[skT1$groups == i]
  m1 <- names(skT3$groups)
  b <- length(m)
  if(b == 1){
    n <- (skT1$groups[names(skT1$groups) == m]) - (skT3$groups[names(skT3$groups) == m])
  }
  else {
    for (i in seq_len(b)) {
      if(m[i] %in% m1){
        n[[i]] <- (skT1$groups[names(skT1$groups) == m[i]]) - (skT3$groups[names(skT3$groups) == m[i]])
      } else { next }
    } 
    
  }
  return (list(m,n))
}
