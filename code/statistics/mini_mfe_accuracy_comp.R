
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)


#Working directory should be the Psychopy experiment directory.
proje_wd <- "/Users/kihossei/Documents/GitHub/memory-for-error-mini/materials/task/mini_mfe"
setwd(proje_wd)

today <- Sys.Date()
today <- format(today, "%Y%m%d")

# Defining the input and output folders.
input_path <- paste(proje_wd, "data", sep ="/", collapse = NULL) # input data directory
output_path <- paste(proje_wd, "stat_output", sep ="/", collapse = NULL) # output directory
proc_fileName <- paste(today, "_mfeProj.csv", sep ="", collapse = NULL) # output filename

#identify data files
datafiles_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(input_path, pattern = ".csv") # listing only csv files
for (lisar1 in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[lisar1], "face_flanker_v1", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[lisar1]
    datafiles_list <- c(datafiles_list, temp_list)
  }
}
mainDat <- setNames(data.frame(matrix(ncol = 11, nrow = 0)), c("id", "accuracy", "congAcc", "incongAcc", "congCorr_meanRT", "incongCorr_meanRT", "congCorr_logMeanRT", "incongCorr_logMeanRT",
                                                               "flankEff_meanACC", "flankEff_meanRT", "flankEff_logMeanRT"))
# will loop over all participant datafiles.
for(i in 1:length(datafiles_list)){
  #for this participant, find the csv file
  psychopy_file <- paste(input_path,datafiles_list[i], sep = "/", collapse = NULL)

  #read in the data for this participant, establish id, and remove extraneous variables
  psychopyDat <- read.csv(file = psychopy_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))
  id <- psychopyDat$id[1]
  psychopyDatTrim <- psychopyDat[c("id",
                                   "new", # The displayed face is new? This column stores the correct value of the task not the response from the subject
                                   "newKey", # Which key should be pressed when the face is new!
                                   "FriendlyKey", # Which key should be pressed when they identify face as friendly!
                                   "congruent",
                                   "stimNum",
                                   "accuracy",
                                   "task1_stim_keyResp.keys",
                                   "textbox_2.text", # stores the number of reported errors by subjects
                                   "surprise_key_resp.keys",
                                   "friendly_key_resp.keys",
                                   "bigFace.started",
                                   "surpriseFaces",
                                   "straightFace",
                                   "task1_stim_keyResp.rt", #  this stores reaction time for each trial
                                   "task_trial_loop.thisTrialN")] # This stores the number of trial in a block; For this study it starts from 0 to 31
  #                                                                 as we have 32 trials in each block.
  ###################################
  #### SECTION 1:
  #remove practice trials and any rows that do not reflect experiment data
  remove_first_row <- psychopyDatTrim[c(-1),]
  remove_prac_trials <- subset(remove_first_row, !complete.cases(remove_first_row$bigFace.started)) # removes practice trials
  # Calculate the overall accuracy in the main task
  accuracy <- mean(remove_prac_trials$accuracy, na.rm = TRUE)
  # Calculate the average accuracy in congruent trials
  congDat <- filter(remove_prac_trials, congruent ==1) # subset the data for congruent trials.
  congAcc <- mean(congDat$accuracy, na.rm = TRUE) # mean accuracy for congruent trials
  # Calculate the average accuracy in incongruent trials
  incongDat <- filter(remove_prac_trials, congruent ==0) # subset the data for incongruent trials.
  incongAcc <- mean(incongDat$accuracy, na.rm = TRUE) # mean accuracy for incongruent trials


  keep_rows_with_acc_vals <- subset(remove_prac_trials, complete.cases(remove_prac_trials$accuracy))
  errorDat <- filter(keep_rows_with_acc_vals, accuracy ==0) # subset error trials
  errorDat$task1_stim_keyResp.rt <- gsub("[", "", errorDat$task1_stim_keyResp.rt, fixed = TRUE) #removing brackets and converting to numeric
  errorDat$task1_stim_keyResp.rt <- gsub("]", "", errorDat$task1_stim_keyResp.rt, fixed = TRUE)
  errorDat$task1_stim_keyResp.rt <- gsub(",.*","",errorDat$task1_stim_keyResp.rt) # removing the RT for the second response within the same trial.
  errorDat$task1_stim_keyResp.rt <- as.numeric(errorDat$task1_stim_keyResp.rt) #
  errorDat <- subset(errorDat, complete.cases(errorDat$task1_stim_keyResp.rt))
  incong_errorDat <- filter(errorDat, congruent ==0) # subset incongruent error trials
  corrDat <- filter(keep_rows_with_acc_vals, accuracy ==1) # subset correct trials
  corrDat$task1_stim_keyResp.rt <- gsub("[", "", corrDat$task1_stim_keyResp.rt, fixed = TRUE)
  corrDat$task1_stim_keyResp.rt <- gsub("]", "", corrDat$task1_stim_keyResp.rt, fixed = TRUE)
  corrDat$task1_stim_keyResp.rt <-  gsub(",.*","",corrDat$task1_stim_keyResp.rt)
  corrDat$task1_stim_keyResp.rt <- as.numeric(corrDat$task1_stim_keyResp.rt) #
  corrDat <- subset(corrDat, complete.cases(corrDat$task1_stim_keyResp.rt))



  # subset the data for correct trials only, separately for congruent and incongruent trials, creating new data frames for each
  cong_corrDat <- corrDat[corrDat$congruent == 1,]
  incong_corrDat <- corrDat[corrDat$congruent == 0,]
  #for correct trials, compute mean RT (raw and log-corrected)
  congCorr_meanRT <- mean(cong_corrDat$task1_stim_keyResp.rt)
  incongCorr_meanRT <- mean(incong_corrDat$task1_stim_keyResp.rt)

  congCorr_logMeanRT <- mean(log((1+cong_corrDat$task1_stim_keyResp.rt)))
  incongCorr_logMeanRT <- mean(log((1+incong_corrDat$task1_stim_keyResp.rt)))

  # compute flanker-effect scores for accuracy, RT, log-RT
  flankEff_meanACC <- incongAcc - congAcc
  flankEff_meanRT <- incongCorr_meanRT - congCorr_meanRT
  flankEff_logMeanRT <- incongCorr_logMeanRT - congCorr_logMeanRT
  mainDat[nrow(mainDat) + 1,] <-c(id, accuracy, congAcc, incongAcc, congCorr_meanRT, incongCorr_meanRT, congCorr_logMeanRT, incongCorr_logMeanRT, flankEff_meanACC, flankEff_meanRT, flankEff_logMeanRT)
} # closing for loop to go exclude everyone with undesired accuracy!
######################### End of Excluding data based on accuracy in the flanker task ##################################
mean_acc <- mean(as.numeric(mainDat$accuracy))
sd_acc <- sd(as.numeric(mainDat$accuracy))
range_acc <- range(as.numeric(mainDat$accuracy))

mean_incong_acc <- mean(as.numeric(mainDat$incongAcc))
sd_incong_acc <- sd(as.numeric(mainDat$incongAcc))
range_incong_acc <- range(as.numeric(mainDat$incongAcc))

mean_cong_acc <- mean(as.numeric(mainDat$congAcc))
sd_cong_acc <- sd(as.numeric(mainDat$congAcc))
range_cong_acc <- range(as.numeric(mainDat$congAcc))