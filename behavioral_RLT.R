#Script to obtain behavioral measures from the RLT task.
#Output is a text file saved in the directory you indicate below.
#You can read the output directly in Excel or SPSS. 

# This version is a function version to be used with the Shiny App BehAppy

#temp
logdir <- "C:/Users/pfuentesc/Desktop/behappy"
outdir <- "C:/Users/pfuentesc/Desktop/behappy"
subs <- read.table("C:/Users/pfuentesc/Desktop/behappy/subjects.txt")$V1
outfile <- "test_rlt"

behavioral_RLT <- function(outputdir, logdir, subjects, outfile) {
  
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
  
  
  #####################################
  # Info we need to extract
  # RT in rewarded and bivalent blocks (in general)
  # RT progression in rewarded and bivalent blocks (trial to trial)
  # Number of trials choosing stimulus A in reward and bivalent blocks
  # Choice progression in reward and bivalent blocks (trial to trial)
  # Win-stay / Loose-shift patterns - NOT IMPLEMENTED YET
  ####################################
  
  
  results.rt <- data.frame(ID=character(), meanRT=double(), sdRT=double(), 
                           meanRT.rew=double(), sdRT.rew=double(), meanRT.biv=double(), sdRT.biv=double(), n.timeout=double(), 
                           n.anticipation=double())
  
  rt.blocks <- data.frame(ID=character(), rew.t1=double(), rew.t2=double(),
                          rew.t3=double(), rew.t4=double(), rew.t5=double(), rew.t6=double(), rew.t7=double(),
                          rew.t8=double(), rew.t9=double(), rew.t10=double(), rew.t11=double(), rew.t12=double(),
                          rew.t13=double(), rew.t14=double(), rew.t15=double(), rew.t16=double(),
                          biv.t1=double(), biv.t2=double(), biv.t3=double(), biv.t4=double(), biv.t5=double(), 
                          biv.t6=double(), biv.t7=double(), biv.t8=double(), biv.t9=double(), biv.t10=double(), 
                          biv.t11=double(), biv.t12=double(), biv.t13=double(), biv.t14=double(), biv.t15=double(), 
                          biv.t16=double())
  
  results.choices <- data.frame(ID=character(), n.rew=double(), p.rew=double(),
                                n.biv=double(), p.biv=double(), rew.t1=double(), rew.t2=double(),
                                rew.t3=double(), rew.t4=double(), rew.t5=double(), rew.t6=double(), 
                                rew.t7=double(), rew.t8=double(), rew.t9=double(), rew.t10=double(), 
                                rew.t11=double(), rew.t12=double(), rew.t13=double(), rew.t14=double(), 
                                rew.t15=double(), rew.t16=double(), biv.t1=double(), biv.t2=double(), 
                                biv.t3=double(), biv.t4=double(), biv.t5=double(), biv.t6=double(), 
                                biv.t7=double(), biv.t8=double(), biv.t9=double(), biv.t10=double(), 
                                biv.t11=double(), biv.t12=double(), biv.t13=double(), biv.t14=double(), 
                                biv.t15=double(), biv.t16=double())
  
  
  
  
  for (i in 1:length(subs)){
    
    
    fileName=paste(subs[[i]], "_RLT.txt", sep="")
    
    if (file.exists(fileName)) {
      
      behav <- read.table(fileName)
      
      
      #############################################################
      ## RT 
      #############################################################
      
      RT <- behav[which(behav$V1=="resp"),]
      
      condition <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      RT$V4 <- condition
      
      RT$V5 <- rep(seq(1:16),10)
      
      timeout <- sum(RT$V3 == "---")
      RT <- RT[which(RT$V3 != "---"),] # Para descartar timeout
      
      if (timeout > 0) {
        RT$V3 <- as.numeric(levels(RT$V3))[RT$V3]
      }
      
      anticip <- sum(RT$V3 < 100)
      RT <- RT[which(RT$V3 > 100),] # Para descartar anticipaciones 
      
      RT.rew <- RT[which(RT$V4==1),]
      RT.biv <- RT[which(RT$V4==0),]
      
      
      # General RTs
      temp.results <- data.frame(subs[i], mean(RT$V3), sd(RT$V3), mean(RT.rew$V3), sd(RT.rew$V3), 
                                 mean(RT.biv$V3), sd(RT.biv$V3), timeout, anticip)
      names(temp.results) <- c("ID", "meanRT", "sdRT", "meanRT.rew", "sdRT.rew", "meanRT.biv", "sdRT.biv",
                               "n.timeout", "n.anticipation")
      
      results.rt <- rbind(results.rt, temp.results)
      
      
      #RTs by trial
      trials.rew <- NULL
      trials.biv <- NULL
      
      for (j in 1:16) {
        
        trials.rew <- c(trials.rew, mean(RT.rew$V3[which(RT.rew$V5==j)], na.rm = TRUE))
        trials.biv <- c(trials.biv, mean(RT.biv$V3[which(RT.rew$V5==j)], na.rm = TRUE))
        
      }
      
      trials <- c(trials.rew, trials.biv)
      
      temp.rt.blocks <- data.frame(subs[i], as.data.frame(t(trials))) 
      
      names(temp.rt.blocks) <- c("ID", "rew.t1", "rew.t2",
                                 "rew.t3", "rew.t4", "rew.t5", "rew.t6", "rew.t7",
                                 "rew.t8", "rew.t9", "rew.t10", "rew.t11", "rew.t12",
                                 "rew.t13", "rew.t14", "rew.t15", "rew.t16",
                                 "biv.t1", "biv.t2", "biv.t3", "biv.t4", "biv.t5", 
                                 "biv.t6", "biv.t7", "biv.t8", "biv.t9", "biv.t10", 
                                 "biv.t11", "biv.t12", "biv.t13", "biv.t14", "biv.t15", 
                                 "biv.t16")
      
      rt.blocks <- rbind(rt.blocks, temp.rt.blocks)
      
      
      #############################################################
      ## Choice proportion and evolution 
      #############################################################
      
      # "Rewarded" stimulus is A in reward condition
      # We will extract choices for A only since choices for B are complementary
      
      # General n and percentage
      n.rew <- sum(RT$V2=="A" & RT$V4==1)
      p.rew <- (n.rew/sum(RT$V4==1))
      n.biv <- sum(RT$V2=="A" & RT$V4==0)
      p.biv <- (n.biv/sum(RT$V4==0))
      
      # Choices by trial
      choices.rew <- NULL
      choices.biv <- NULL
      
      for (j in 1:16) {
        
        choices.rew <- c(choices.rew, sum(RT.rew$V2=="A" & RT.rew$V5==j, na.rm = TRUE))
        choices.biv <- c(choices.biv, sum(RT.biv$V2=="A" & RT.biv$V5==j, na.rm = TRUE))
        
      }
      
      choices.rew <- choices.rew/5
      choices.biv <- choices.biv/5
      
      temp.choices <- data.frame(subs[i], n.rew, p.rew, n.biv, p.biv, as.data.frame(t(choices.rew)),
                                 as.data.frame(t(choices.biv)))
      
      names(temp.choices) <- c("ID", "n.rew", "p.rew", "n.biv", "p.biv", "rew.t1", "rew.t2",
                               "rew.t3", "rew.t4", "rew.t5", "rew.t6", "rew.t7",
                               "rew.t8", "rew.t9", "rew.t10", "rew.t11", "rew.t12",
                               "rew.t13", "rew.t14", "rew.t15", "rew.t16",
                               "biv.t1", "biv.t2", "biv.t3", "biv.t4", "biv.t5", 
                               "biv.t6", "biv.t7", "biv.t8", "biv.t9", "biv.t10", 
                               "biv.t11", "biv.t12", "biv.t13", "biv.t14", "biv.t15", 
                               "biv.t16")
      
      results.choices <- rbind(results.choices, temp.choices)
      
      
      
    } else {
      
      # General RTs
      temp.results <- data.frame(subs[i], t(rep(NA, 8)))
      names(temp.results) <- c("ID", "meanRT", "sdRT", "meanRT.rew", "sdRT.rew", "meanRT.biv", "sdRT.biv",
                               "n.timeout", "n.anticipation")
      results.rt <- rbind(results.rt, temp.results)
      
      
      # RT by trial
      temp.rt.blocks <- data.frame(subs[i], t(rep(NA, 32))) 
      names(temp.rt.blocks) <- c("ID", "rew.t1", "rew.t2",
                                 "rew.t3", "rew.t4", "rew.t5", "rew.t6", "rew.t7",
                                 "rew.t8", "rew.t9", "rew.t10", "rew.t11", "rew.t12",
                                 "rew.t13", "rew.t14", "rew.t15", "rew.t16",
                                 "biv.t1", "biv.t2", "biv.t3", "biv.t4", "biv.t5", 
                                 "biv.t6", "biv.t7", "biv.t8", "biv.t9", "biv.t10", 
                                 "biv.t11", "biv.t12", "biv.t13", "biv.t14", "biv.t15", 
                                 "biv.t16")
      rt.blocks <- rbind(rt.blocks, temp.rt.blocks)
      
      
      # Choice proportion
      temp.choices <- data.frame(subs[i], t(rep(NA, 36)))
      names(temp.choices) <- c("ID", "n.rew", "p.rew", "n.biv", "p.biv", "rew.t1", "rew.t2",
                               "rew.t3", "rew.t4", "rew.t5", "rew.t6", "rew.t7",
                               "rew.t8", "rew.t9", "rew.t10", "rew.t11", "rew.t12",
                               "rew.t13", "rew.t14", "rew.t15", "rew.t16",
                               "biv.t1", "biv.t2", "biv.t3", "biv.t4", "biv.t5", 
                               "biv.t6", "biv.t7", "biv.t8", "biv.t9", "biv.t10", 
                               "biv.t11", "biv.t12", "biv.t13", "biv.t14", "biv.t15", 
                               "biv.t16")
      results.choices <- rbind(results.choices, temp.choices)
      
    }
    
    
  }
  
  
  setwd(outputdir)
  write.table(results.rt, file = paste(outfile, "_global_RTs", ".txt", sep = ""), sep = "\t", row.names=FALSE)
  write.table(rt.blocks, file = paste(outfile, "_RTs_by_trial", ".txt", sep = ""), sep = "\t", row.names=FALSE)
  write.table(results.choices, file = paste(outfile, "_choices", ".txt", sep = ""), sep = "\t", row.names=FALSE)
  
  
}
  
