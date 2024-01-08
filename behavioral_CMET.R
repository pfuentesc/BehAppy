#Script to obtain behavioral measures from the CMET task.
#Output is a csv file saved in the directory you indicate below.
#You can read the output directly in Excel or SPSS. 

# This version is a function version to be used with the Shiny App BehAppy


behavioral_CMET <- function(outputdir, logdir, subjects, outfile) {
  
  #Output directory
  outputdir <- outputdir
  
  #Logfile directory 
  logdir <- logdir
  
  #Subject list  
  subs <- subjects
  
  #Output name
  outfile <- outfile
  
  
  # Get data
  setwd(logdir)
  
  # Create empty dataframe that will be populated with subjects' values
  ID <- subs
  behav <- data.frame(ID)
  behav$vol_changes1 <- NA
  behav$vol_changes2 <- NA
  behav$vol_changes3 <- NA
  behav$vol_changes4 <- NA
  behav$score <- NA
  behav$sec_car1 <- NA
  behav$sec_car2 <- NA
  behav$sec_car3 <- NA
  behav$sec_car4 <- NA
  behav$sec_catch1 <- NA
  behav$sec_catch2 <- NA
  behav$sec_catch3 <- NA
  behav$sec_catch4 <- NA
  behav$sec_ball1 <- NA
  behav$sec_ball2 <- NA
  behav$sec_ball3 <- NA
  behav$sec_ball4 <- NA
  behav$sec_brick1 <- NA
  behav$sec_brick2 <- NA
  behav$sec_brick3 <- NA
  behav$sec_brick4 <- NA
  
  
  
  #Loops through all the subjects: will stop if error
  for (i in 1:length(subs)){
    
    
    fileName=paste(subs[[i]], "_CMET-summary.txt", sep="")
    
    
    #Number of voluntary changes 
    fileString <- grep("BLOCK TOTALS", readLines(fileName))
    
    changes <- read.table(file = fileName, skip = fileString, header = TRUE)
    
    volChanges <- subset(changes, changes$Type=='Vol')
    volChanges2 <- subset(volChanges, select = -c(Block, Type, Seconds, Points))
    volChanges_final <- t(volChanges2)
    
    
    
    #Number of points per block
    fileString <- grep("BLOCK TOTALS", readLines(fileName))
    
    scoreb <- read.table(file = fileName, skip = fileString, header = TRUE)
    
    volScoreb <- subset(scoreb, scoreb$Type=='Vol')
    volScoreb2 <- subset(volScoreb, select = -c(Block, Type, Seconds, Changes))
    volScoreb_final <- t(volScoreb2)
    
    autoScoreb <- subset(scoreb, scoreb$Type=='Auto')
    autoScoreb2 <- subset(autoScoreb, select= -c(Block, Type, Seconds, Changes))
    autoScoreb_final <- t(autoScoreb2)
    
    
    
    #Number of points per task
    firstString <- grep("SESSION LOG SUMMARY", readLines(fileName))
    lastString <- grep("Total automatic changes:", readLines(fileName))-2
    linesToRead <- lastString-firstString
    
    score <- read.table(file = fileName, skip = firstString, header = TRUE, nrows = linesToRead-1)
    
    score2 <- subset(score, select = -c(Task, Seconds))
    score_final <- t(score2)
    
    
    
    #Time per task and block
    firstString <- grep("BLOCK SUMMARIES", readLines(fileName))
    lastString <- grep("BLOCK TOTALS", readLines(fileName))-2
    linesToRead <- lastString-firstString
    
    seconds <- read.table(file = fileName, skip = firstString, header = TRUE, nrows = linesToRead-1)
    
    secondsCar1 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Car' & seconds$Block=='2', select = Seconds)
    secondsCar2 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Car' & seconds$Block=='4', select = Seconds)
    secondsCar3 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Car' & seconds$Block=='6', select = Seconds)
    secondsCar4 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Car' & seconds$Block=='8', select = Seconds)
    
    secondsCar <- c(colSums(secondsCar1)) 
    secondsCar <- append(secondsCar, colSums(secondsCar2))
    secondsCar <- append(secondsCar, colSums(secondsCar3))
    secondsCar <- append(secondsCar, colSums(secondsCar4))
    
    
    secondsCatch1 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Catch' & seconds$Block=='2', select = Seconds)
    secondsCatch2 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Catch' & seconds$Block=='4', select = Seconds)
    secondsCatch3 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Catch' & seconds$Block=='6', select = Seconds)
    secondsCatch4 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Catch' & seconds$Block=='8', select = Seconds)
    
    secondsCatch <- c(colSums(secondsCatch1)) 
    secondsCatch <- append(secondsCatch, colSums(secondsCatch2))
    secondsCatch <- append(secondsCatch, colSums(secondsCatch3))
    secondsCatch <- append(secondsCatch, colSums(secondsCatch4))
    
    
    secondsBall1 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Ball' & seconds$Block=='2', select = Seconds)
    secondsBall2 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Ball' & seconds$Block=='4', select = Seconds)
    secondsBall3 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Ball' & seconds$Block=='6', select = Seconds)
    secondsBall4 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Ball' & seconds$Block=='8', select = Seconds)
    
    secondsBall <- c(colSums(secondsBall1)) 
    secondsBall <- append(secondsBall, colSums(secondsBall2))
    secondsBall <- append(secondsBall, colSums(secondsBall3))
    secondsBall <- append(secondsBall, colSums(secondsBall4))
    
    
    secondsBrick1 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Brick' & seconds$Block=='2', select = Seconds)
    secondsBrick2 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Brick' & seconds$Block=='4', select = Seconds)
    secondsBrick3 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Brick' & seconds$Block=='6', select = Seconds)
    secondsBrick4 <- subset(seconds, seconds$Type=='Vol' & seconds$Task=='Brick' & seconds$Block=='8', select = Seconds)
    
    secondsBrick <- c(colSums(secondsBrick1)) 
    secondsBrick <- append(secondsBrick, colSums(secondsBrick2))
    secondsBrick <- append(secondsBrick, colSums(secondsBrick3))
    secondsBrick <- append(secondsBrick, colSums(secondsBrick4))
    
    
    behav$vol_changes1[i] <- volChanges_final[1,1]
    behav$vol_changes2[i] <- volChanges_final[1,2]
    behav$vol_changes3[i] <- volChanges_final[1,3]
    behav$vol_changes4[i] <- volChanges_final[1,4]
    
    behav$score[i] <- score_final[1,5]
    
    behav$sec_car1[i] <- secondsCar1[1,1]
    behav$sec_car2[i] <- secondsCar2[1,1]
    behav$sec_car3[i] <- secondsCar3[1,1]
    behav$sec_car4[i] <- secondsCar4[1,1]
    behav$sec_catch1[i] <- secondsCatch1[1,1]
    behav$sec_catch2[i] <- secondsCatch2[1,1]
    behav$sec_catch3[i] <- secondsCatch3[1,1]
    behav$sec_catch4[i] <- secondsCatch4[1,1]
    behav$sec_ball1[i] <- secondsBall1[1,1]
    behav$sec_ball2[i] <- secondsBall2[1,1]
    behav$sec_ball3[i] <- secondsBall3[1,1]
    behav$sec_ball4[i] <- secondsBall4[1,1]
    behav$sec_brick1[i] <- secondsBrick1[1,1]
    behav$sec_brick2[i] <- secondsBrick2[1,1]
    behav$sec_brick3[i] <- secondsBrick3[1,1]
    behav$sec_brick4[i] <- secondsBrick4[1,1]
    
    
    # Deviation from optimal playing time
    behav[i,c(2:22)][is.na(behav[i,c(2:22)])] <- 0
    devtime <- apply(behav[i,c(7:22)], 2, function(x){12-x})
    devtime <- abs(devtime)
    devtime_sum <- sum(devtime)/2
    behav$devtime <- devtime_sum
    
    # Voluntary changes
    behav$vol_changes_total[i] <- behav$vol_changes1[i]+behav$vol_changes2[i]+
      behav$vol_changes3[i]+behav$vol_changes4[i]
    behav$vol_changes_mean[i] <- behav$vol_changes_total[i]/4
    
    
  }
  
  
  setwd(outputdir)
  write.csv(behav, file = paste(outfile, ".csv", sep = ""), row.names = FALSE)
  
  
  
  
}














