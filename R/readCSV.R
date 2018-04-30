#' A ReadCSV Function
#' 
#' This Function allows to Read Multiple CSV files into a list
#' @param Want to read multiple csv files ? Defaults to list data.
#' @keywords ldata
#' @export
#' @examples 
#' readCSV()

readCSV <- function(ldata) 
{   
  getwd(); #Locate the folder containing csv files.
  files <- list.files(pattern = ".csv$", full.names = TRUE); #read all the files with .csv extensions to a list.
  ldata <- list(); #create a empty list.
  
  #For Loop for performing various tasks.
  for(i in 1:length(files))
  {
    ldata[[i]] <- read.csv(files[i]);  #copy the data to the empty list.
    ldata[[i]][,c(1,63,68,69,70,72)] <- NULL; #remove the unwanted columns.
    ldata[[i]]$Type_1 <- ifelse(ldata[[i]]$Type_1 == "Bug",1,0);  #Change the labels to 0 and 1.
    ldata[[i]]$Type_3 <- ifelse(ldata[[i]]$Type_3 == "Bug",1,0);  
    ldata[[i]]$Type_1 <- as.factor(ldata[[i]]$Type_1);  #Convert the Type_1 and Type_3 columns to factor.
    ldata[[i]]$Type_3 <- as.factor(ldata[[i]]$Type_3); 
  }
  names(ldata) <- files; #change the names of the datasets in the list. 
  return(ldata); #load the list of data.
  
}