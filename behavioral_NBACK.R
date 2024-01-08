# Script to calculate dprime for the nback task

# This version is a function version to be used with the Shiny App BehAppy


behavioral_NBACK <- function(outputdir, logdir, subjects, outfile) {
  
  # Define inputs
  outputdir <- outputdir
  logdir <- logdir
  ids <- subjects
  
  # Read files with correct responses and conditions
  correct <- read.csv("corr_responses.txt", header = FALSE)$V1
  conditions <- read.csv("conditions.txt", header = FALSE)$V1
  
  # Read IDs
  ids <- as.character(ids)
  
  
  # Dataframe
  dprime <- data.frame(ID=character(), dprime1=double(), dprime2=double())
  
  
  for (j in 1:length(ids)){
    
    
    fileName=paste(logdir, "\\", ids[[j]], "_NBACK.txt", sep="")
    
    if (file.exists(fileName)) {
      
      sub <- read.table(fileName, header = TRUE)
      
      # Remove scanner pulses
      sub <- sub[sub$Event!="t",]
      
      # Mark stimuli with responses
      sub$response <- NA
      
      for (i in 1:(nrow(sub)-1)) {
        if (sub$Event[i+1]!="e") {
          sub$response[i] <- 1
        } else {
          sub$response[i] <- 0
        }
      }
      
      # Remove responses
      sub <- sub[sub$Event=="e",]
      
      # Get correct responses and conditions
      sub$correct <- correct
      sub$conditions <- conditions
      
      # Remove baseline
      sub <- sub[sub$conditions!="baseline",]
      
      
      # Classify responses
      sub$type <- ifelse(sub$response==1 & sub$correct==1, "Hit",
                         ifelse(sub$response==0 & sub$correct==1, "Miss",
                                ifelse(sub$response==1 & sub$correct==0, "FA", "CR")))
      
      # Separate conditions
      back1 <- sub[sub$conditions=="1back",]
      back2 <- sub[sub$conditions=="2back",]
      
      # Calculate d-prime for 1back
      hit <- sum(back1$type=="Hit")
      miss <- sum(back1$type=="Miss")
      fa <- sum(back1$type=="FA")
      cr <- sum(back1$type=="CR")
      
      hit_rate <- hit/(hit+miss)
      fa_rate <- fa/(fa+cr)
      
      # Calculate hit_rate and avoid d' infinity
      half_hit <- 0.01
      
      hit_rate <- hit / (hit + miss)
      if (hit_rate == 1) {
        hit_rate <- 1 - half_hit
      } 
      if (hit_rate == 0) {
        hit_rate <- half_hit
      } 
      
      
      # Calculate false alarm rate and avoid d' infinity
      half_fa <- 0.01
      
      fa_rate = fa / (fa + cr)
      if (fa_rate == 1){
        fa_rate = 1 - half_fa
      }
      if (fa_rate == 0){
        fa_rate = half_fa
      }
      
      dprime1 <- qnorm(hit_rate) - qnorm(fa_rate)
      
      
      # Calculate d-prime for 2back
      hit <- sum(back2$type=="Hit")
      miss <- sum(back2$type=="Miss")
      fa <- sum(back2$type=="FA")
      cr <- sum(back2$type=="CR")
      
      hit_rate <- hit/(hit+miss)
      fa_rate <- fa/(fa+cr)
      
      # Calculate hit_rate and avoid d' infinity
      half_hit <- 0.01
      
      hit_rate <- hit / (hit + miss)
      if (hit_rate == 1) {
        hit_rate <- 1 - half_hit
      } 
      if (hit_rate == 0) {
        hit_rate <- half_hit
      } 
      
      
      # Calculate false alarm rate and avoid d' infinity
      half_fa <- 0.01
      
      fa_rate = fa / (fa + cr)
      if (fa_rate == 1){
        fa_rate = 1 - half_fa
      }
      if (fa_rate == 0){
        fa_rate = half_fa
      }
      
      dprime2 <- qnorm(hit_rate) - qnorm(fa_rate)
      
      
      # Write results in data frame
      temp.results <- data.frame(ids[[j]], dprime1, dprime2)
      names(temp.results) <- c("ID", "dprime1", "dprime2")
      dprime <- rbind(dprime, temp.results)
      
      
    } else {
      
      # Write results in data frame (NAs for files not found)
      temp.results <- data.frame(ids[[j]], NA, NA)
      names(temp.results) <- c("ID", "dprime1", "dprime2")
      dprime <- rbind(dprime, temp.results)
      
    }
    
  }
  
  setwd(outputdir)
  write.table(dprime, file = paste(outfile, ".txt", sep = ""), sep = "\t", row.names=FALSE)
  
  
}





