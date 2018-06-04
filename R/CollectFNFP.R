#' Collect False Negative and False Positive Values.
#'
#' Function to Collect the False Negative and False Positive Values from each dataset, for the purpose of knowing how many percentage of mislabelled and defective modules are present.
#' @param Obtain the FN and FP values through a dataframe, Defaults to MSD (dataframe containing percentage values of all FN,FP)
#' @keywords MSD, FN, FP
#' @export
#' @examples  
#' CollectFNFP()

CollectFNFP <- function(MSD)
{
  FN <- {}
  FP <- {}
  MSD <- data.frame()
  for(i in 1:length(data))
  { 
    class <- data[[i]]$Type_1; 
    scored.class <- data[[i]]$Type_3; 
    FN[i] <- sum(class == 0 & scored.class == 1)/nrow(data[[i]]); 
    FP[i] <- sum(class == 1 & scored.class == 0)/nrow(data[[i]]); 
    
    MSDx <- data.frame(Labels = "Non-defective Modules mislabelled as Defective", Percentage = Non-defective Modules mislabelled as Defective[i]);
    MSD <- rbind(MSD, MSDx); 
    MSDx <- data.frame(Labels = "Actual defective modules mislabelled as non-defective", Percentage = Actual defective modules mislabelled as non-defective[i]); 
    MSD <- rbind(MSD, MSDx); }
  return(MSD)
}
