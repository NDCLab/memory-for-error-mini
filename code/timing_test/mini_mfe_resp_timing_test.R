

#######  www.ndclab.com   ###########
# By: Kianoosh Hosseini at NDCLab@FIU  
# This script reads text files that include triggers sent to the recorder laptop.
# This script is changed for different timing tests.
# These files have ".vmrk" extension.
# install.packages("pracma")
library(stringr)
library(pracma)
library(dplyr)

#setwd("~/Users/kihossei/Desktop") #set working directory to where your text file (the one that you have triggers and their times) is located.
path <- ("/Users/kihossei/Documents/GitHub/memory-for-error-mini/code/timing_test")

mrkTxt <- readLines(paste(path, "/mini_mfe_timeTest_resp.vmrk", sep = "")) # load the .vmrk file into the workspace.
myDat <- setNames(data.frame(matrix(nrow = length(mrkTxt), ncol = 1)), c("colA")) # creates an empty data frame with a single column and row # = length(mrkTxt)

# This for loop creates a dataframe from the loaded text file. 
for (i in 1:length(mrkTxt)) {
  myDat[i, 1] <- mrkTxt[i]
  
}

# Keep rows that have the string "Stimulus"
newDat <- myDat %>%
  filter(
    str_detect(colA, "Stimulus")
  )
# Let's delete all the strings to "Stimulus,"
for (i in 1:nrow(newDat)) {
  newDat$colB[i] <- gsub(".*Stimulus,", "", newDat$colA[i]) 
}

# Let's delete all the strings after the ms of the sent marker!
for (i in 1:nrow(newDat)) {
  newDat$colC[i] <- gsub(",1,0*.", "", newDat$colB[i]) 
}

newDat <- subset(newDat, select = -c(colA, colB)) # removing the colA and colB columns.
proc_fileName <- "resp_timing_output.csv"
write.csv(newDat,paste(path,proc_fileName, sep = "/", collapse = NULL), row.names=FALSE)

newDat <- read.csv(paste(path,proc_fileName, sep = "/", collapse = NULL))

timeVal <- setNames(data.frame(matrix(ncol = 1)), c("timeDiff")) # creating an empty data frame that will be filled with time difference values!
dVal <- setNames(data.frame(matrix(ncol = 1)), c("timeDiff"))

# If you wana perform the flanker timing test for responses run the code below!
# Stim trak markers come first and then resp triggers appear! They may be sent at the same time. In this case, they sum up.
# For ex., 128 (stim trak marker) adds to 11 or 12 (resp triggers) and become either S139 or S140!

for (i in 1:nrow(newDat)) {
  if (str_detect(newDat$colC[i], "S139") || str_detect(newDat$colC[i], "S140") ||  str_detect(newDat$colC[i], "S128")) {
    if (str_detect(newDat$colC[i], "S139") || str_detect(newDat$colC[i], "S140")){
      if (str_detect(newDat$colC[i-1], "S128")){
        task_sent_mrk <- str2num(gsub(".*,", "", newDat$colC[i]))
        stimTrak_mrk <- str2num(gsub(".*,", "", newDat$colC[i-1]))
        diffVal <- task_sent_mrk - stimTrak_mrk # stim trak trigger came first and then the marker from laptop
        dVal[1,] <- diffVal
        timeVal <- rbind(timeVal, dVal)
      } else if (str_detect(newDat$colC[i-1], "S 11") || str_detect(newDat$colC[i-1], "S 12")){
        stimTrak_mrk <- str2num(gsub(".*,", "", newDat$colC[i]))
        task_sent_mrk <- str2num(gsub(".*,", "", newDat$colC[i-1]))
        diffVal <- task_sent_mrk - stimTrak_mrk # The marker from laptop (task sent trigger) came first and then the stim trak trigger!
        dVal[1,] <- diffVal
        timeVal <- rbind(timeVal, dVal)
      } else {
        task_sent_mrk <- str2num(gsub(".*,", "", newDat$colC[i]))
        stimTrak_mrk <- task_sent_mrk
        diffVal <- task_sent_mrk - stimTrak_mrk # both sent at the same time!
        dVal[1,] <- diffVal
        timeVal <- rbind(timeVal, dVal)
      }
    } else if (str_detect(newDat$colC[i], "S128")) {
      if (str_detect(newDat$colC[i-1], "S 11") || str_detect(newDat$colC[i-1], "S 12")){
        stimTrak_mrk <- str2num(gsub(".*,", "", newDat$colC[i]))
        task_sent_mrk <- str2num(gsub(".*,", "", newDat$colC[i-1]))
        diffVal <- task_sent_mrk - stimTrak_mrk # The marker from laptop (task sent trigger) came first and then the stim trak trigger!
        dVal[1,] <- diffVal
        timeVal <- rbind(timeVal, dVal)
      }
    }
  }
}


timeVal <- na.omit(timeVal, na.action = "omit")
mean(timeVal$timeDiff)
sd(timeVal$timeDiff)

######## Below is the timing test results for the Flanker task of the mini memory for error task######
# response triggers 
# mean(test$value)
# [1] 1.126761
# sd(test$value)
# [1] 1.970711








