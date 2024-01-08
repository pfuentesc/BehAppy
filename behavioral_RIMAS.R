# Extraction of relevant behavioral data for Rhyming task
# Output is a text file that can be open with Excel, SPSS, Jamovi or R
# This is a function version to use with BehAppy Shiny App


behavioral_RIMAS <- function(outputdir, logdir, subjects, outfile) {
  
  # Read data
  logdir <- logdir
  outdir <- outputdir
  subjects <- subjects
  outfile <- outfile
  
  
  ID <- subjects
  
  
  # Create empty dataframe
  results <- data.frame()
  
  # Correct responses
  rima_correct <- c("no", "no", "si", "no", "si", "si", "no", "no", "si",
                    "no", "no", "si", "no", "si", "si", "no", "si", "si",
                    "no", "no", "si", "no", "si", "si")
  
  control_correct <- c("no", "si", "si", "no", "no", "si", "no", "no", "si",
                       "no", "si", "si", "no", "si", "si", "no", "no", "si",
                       "no", "si", "si", "no", "no", "si")
  
  
  # Extract behavioral features
  
  for (i in seq(1, length(ID))) {
    
    temp.results <- data.frame(ID[i])
    names(temp.results) <- c("ID")
    
    setwd(logdir)
    fileName <- paste(ID[i], "_RIMAS_summary.txt", sep = "")
    sub <- read.table(fileName, header = TRUE)
    
    sub <- sub[sub$event_type!="quest_infos",]
    sub <- sub[sub$event_type!="fix",]
    
    
    # Rhyme
    rima <- sub[sub$event_type=="rhyme",]
    rima_rt <- as.numeric(rima$rt)
    temp.results$rhyme_rt_mean <- mean(rima_rt, na.rm = TRUE)
    temp.results$rhyme_rt_median <- median(rima_rt, na.rm = TRUE)
    temp.results$rhyme_rt_sd <- sd(rima_rt, na.rm = TRUE)
    temp.results$rhyme_rt_min <- min(rima_rt, na.rm = TRUE)
    temp.results$rhyme_rt_max <- max(rima_rt, na.rm = TRUE)
    temp.results$rhyme_miss <- sum(is.na(rima_rt))
    temp.results$rhyme_n_correct <- sum(rima_correct == rima$response)
    temp.results$rhyme_prop_correct <- sum(rima_correct == rima$response)/24
    
    
    # Control
    control <- sub[sub$event_type=="cont",]
    control_rt <- as.numeric(control$rt)
    temp.results$control_rt_mean <- mean(control_rt, na.rm = TRUE)
    temp.results$control_rt_median <- median(control_rt, na.rm = TRUE)
    temp.results$control_rt_sd <- sd(control_rt, na.rm = TRUE)
    temp.results$control_rt_min <- min(control_rt, na.rm = TRUE)
    temp.results$control_rt_max <- max(control_rt, na.rm = TRUE)
    temp.results$control_miss <- sum(is.na(control_rt))
    temp.results$control_n_correct <- sum(control_correct == control$response)
    temp.results$control_prop_correct <- sum(control_correct == control$response)/24
    
    
    # Global
    global <- rbind.data.frame(rima, control)
    global_rt <- as.numeric(global$rt)
    temp.results$global_rt_mean <- mean(global_rt, na.rm = TRUE)
    temp.results$global_rt_median <- median(global_rt, na.rm = TRUE)
    temp.results$global_rt_sd <- sd(global_rt, na.rm = TRUE)
    temp.results$global_rt_min <- min(global_rt, na.rm = TRUE)
    temp.results$global_rt_max <- max(global_rt, na.rm = TRUE)
    temp.results$global_miss <- sum(temp.results$rhyme_miss, temp.results$control_miss)
    temp.results$global_n_correct <- sum(temp.results$rhyme_n_correct, temp.results$control_n_correct)
    temp.results$global_prop_correct <- sum(temp.results$rhyme_n_correct, temp.results$control_n_correct)/48
    
    
    # AVH
    quest <- sub[sub$event_type=="quest",]
    quest_rhyme <- quest[c(1,3,5,7,9,11,13,15),]
    quest_control <- quest[c(2,4,6,8,10,12,14,16),]
    temp.results$avh_rhyme <- sum(quest_rhyme$respone=="si")
    temp.results$noavh_rhyme <- sum(quest_rhyme$response=="no")
    temp.results$avh_control <- sum(quest_control$response=="si")
    temp.results$noavh_control <- sum(quest_control$response=="no")
    
    
    results <- rbind.data.frame(results, temp.results)
    
    
  }
  
  setwd(outdir)
  write.table(results, paste(outfile, ".txt", sep = ""), row.names = FALSE)
  
  
}


