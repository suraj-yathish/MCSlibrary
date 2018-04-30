#' Function to perform Anova test
#' 
#' A function that automates the process of performing Anova type 2 test for the LR models.
#' @param Collect Chisq values of all the metrics involved in LR, Defaults to dataset, VIF1, VIF3, defect.  
#' @keywords VIF, defect, data 
#' @export
#' @examples 
#' AnovaTest()

AnovaTest <- function(data, VIF1, VIF3, defect1, defect2)
  
  
{
  A <- {
  }
  B <- {
  }
  Ax <- {
  }
  Bx <- {
  }
  
  #Bootstrap the Anova Chisq results
  for (j in seq(1:100))
  {
    #Generate a bootstrap sample with replacement
    indices <- sample(nrow(data), replace = TRUE)
    
    #Generate training dataset using a bootstrap sample
    train_data <- data[indices,]
    
    #Generate testing dataset (instances not included in the bootstrap sample)
    test_data <- data[-unique(indices),]
    
    #convert response variables to factors.
    train_data$Type_1 <- as.factor(train_data$Type_1)
    train_data$Type_3 <- as.factor(train_data$Type_3)
    
    test_data$Type_1 <- as.factor(test_data$Type_1)
    test_data$Type_3 <- as.factor(test_data$Type_3)
    
    
    #Fit a prediction model using Logistics Regression Model
    
    modelT1 <-
      glm(as.formula(paste(
        defect1, '~', paste0(VIF1, collapse = '+')
      )), data = train_data, family = binomial)
    modelT3 <-
      glm(as.formula(paste(
        defect2, '~', paste0(VIF3, collapse = '+')
      )), data = train_data, family = binomial)
    
    #Perform Anova Type2 test on the selected LR Models for Type_1.
    Ax <- Anova(modelT1, type = 2)
    #Perform Anova Type2 test on the selected LR Models for Type_3.
    Bx <- Anova(modelT3, type = 2)
    
    
    #Select only the Chisqr column.
    A <- rbind(A, Ax[, 1])
    #change the colnames of the data frame A.
    colnames(A) <- rownames(Ax)
    
    
    
    #Select only the Chisqr column.
    B <- rbind(B, Bx[, 1]) #For Type_3.
    #change the colnames of the data frame A.
    colnames(B) <- rownames(Bx) #For Type_3.
    
    
  }
  return (list(A, B))
} 