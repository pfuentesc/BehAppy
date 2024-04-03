# Script to convert TAP scores to FSIQ (WAIS-III)


calculate_tap <- function(inputnum) {
  
  # Input from app GUI
  inputnum <- tap
  
  
  # Convert number
  fsiq <- c(60,62,64,66,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,98,100,102,104,106,108,110,112,114,116)
  
  
  for (i in 1:length(inputnum)) {
    
    conv_fsiq <- fsiq[inputnum]
    
  }
  
  return(conv_fsiq)
  
}


