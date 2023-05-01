# This script will load, and preprocess the pavlovia data.
# Author: Kianoosh Hosseini at NDCLab @FIU (May-September 2022; https://Kianoosh.info; https://NDClab.com)
# Last Update: 2023-02-03 (YYYY-MM-DD)
# Based on longFriendly script.



library(tidyverse)
library(dplyr)
library(stringr)
library(psycho)


#Working directory should be the Psychopy experiment directory.

proje_wd <- "/Users/kihossei/Documents/GitHub/memory-for-error-mini/materials/task/mini_mfe"
setwd(proje_wd)

today <- Sys.Date()
today <- format(today, "%Y%m%d")

# Defining the input and output folders.
input_path <- paste(proje_wd, "data", sep ="/", collapse = NULL) # input data directory
output_path <- paste(proje_wd, "stat_output", sep ="/", collapse = NULL) # output directory
rt_fileName <- paste(today, "_rt_mfe_onlineProj.csv", sep ="", collapse = NULL) # output filename
proc_fileName <- paste(today, "mini_mfe_Proj.csv", sep ="", collapse = NULL) # output filename
long_fileName <- paste(today, "long_mini_mfe_Proj.csv", sep ="", collapse = NULL) # output filename
outlier_data_path <- paste(proje_wd, "outlier_data", sep ="/", collapse = NULL) #outlier_data directory

# creating a list of all data csv files in the input folder.
datafiles_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(input_path, pattern = ".csv") # listing only csv files
for (i in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[i], "face_flanker_v1", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[i]
    datafiles_list <- c(datafiles_list, temp_list)
  }
}
rt_df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("id", "shortest_allowed_rt", "number_of_removed_trials_in_the_surp", "number_of_removed_trials_in_friendlyDat")) # this dataframe will store the number of removed trials in the surp!
master_surpDat <- data.frame() # an empty dataframe that will store all surpDat from all participants.
master_friendlyDat <- data.frame() # an empty dataframe
master_flankerDat <- data.frame() # an empty dataframe that will store all flanker data from all participants.
incong_error_cutoff <- 8
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
                                   "congruent",
                                   "FriendlyKey",
                                   "stimNum",
                                   "accuracy",
                                   "task1_stim_keyResp.keys",
                                   "textbox_2.text", # stores the number of reported errors by subjects
                                   "surprise_key_resp.keys",
                                   "friendly_key_resp.keys",
                                   "surprise_key_resp.rt",
                                   "friendly_key_resp.rt",
                                   "bigFace.started",
                                   "surpriseFaces",
                                   "straightFace",
                                   "task1_stim_keyResp.rt", #  this stores reaction time for each trial
                                   "task_trial_loop.thisTrialN")] # This stores the number of trial in a block; For this study it starts from 0 to 31
  #                                                                 as we have 32 trials in each block.

  #remove practice trials and any rows that do not reflect experiment data
  remove_first_row <- psychopyDatTrim[c(-1),]
  remove_prac_trials <- subset(remove_first_row, !complete.cases(remove_first_row$bigFace.started)) # removes practice trials
  # Calculate the overall accuracy in the main task
  accuracy <- mean(remove_prac_trials$accuracy, na.rm = TRUE)
  if (accuracy >= 0.6){ # checks if they have falnker accuracy above 60%!
    flanker_rt_dat <- subset(remove_prac_trials, complete.cases(remove_prac_trials$accuracy)) # keeps only flanker trials
    flanker_rt_dat$task1_stim_keyResp.rt <- gsub("[", "", flanker_rt_dat$task1_stim_keyResp.rt, fixed = TRUE) #removing brackets and converting to numeric
    flanker_rt_dat$task1_stim_keyResp.rt <- gsub("]", "", flanker_rt_dat$task1_stim_keyResp.rt, fixed = TRUE)
    flanker_rt_dat$task1_stim_keyResp.rt <- gsub(",.*","",flanker_rt_dat$task1_stim_keyResp.rt) # removing the RT for the second response within the same trial.
    flanker_rt_dat$task1_stim_keyResp.rt <- as.numeric(flanker_rt_dat$task1_stim_keyResp.rt)

    corrDat <- filter(flanker_rt_dat, accuracy ==1)
    corrDat <- subset(corrDat, complete.cases(corrDat$task1_stim_keyResp.rt))
    cong_corrDat <- corrDat[corrDat$congruent == 1,]

    mean_flanker_rt <- mean(cong_corrDat$task1_stim_keyResp.rt, na.rm = TRUE) # computing mean RT of the flanker task
    sd_flanker_rt <- sd(cong_corrDat$task1_stim_keyResp.rt, na.rm = TRUE) # computing sd of RT for the flanker task
    shortest_allowed_rt <- mean_flanker_rt - (2 * sd_flanker_rt) # This rt will be used as a threshold to see if I should remove a surprise trial based on rt or not!

    # Creating and Preparing the surprise dataframe.
    surpDat <- subset(remove_prac_trials, complete.cases(remove_prac_trials$new)) #contains all the data we need from the surprise task
    each_surp_block_new_key <- subset(remove_prac_trials, complete.cases(remove_prac_trials$newKey))

    surpDat <- subset(remove_prac_trials, !complete.cases(remove_prac_trials$FriendlyKey))
    surpDat <- subset(surpDat, complete.cases(surpDat$newKey))
    surpDat <- subset(surpDat, complete.cases(surpDat$new)) #contains all the data we need from the surprise task
    surpDat$newKey <- replace(surpDat$newKey, surpDat$newKey =='right', 8) # replace 8 values with right for the next loop.
    surpDat$newKey <- replace(surpDat$newKey, surpDat$newKey =='left', 1)
    # adding a column to surpDat to show whether a trial should be kept or removed.
    # 1 means keep the trial and 0 means to remove it. At this stage, we don't remove a trial. We just mark them in the corresponding column.
    for (kk in 1:nrow(surpDat)){
      surpDat$keep_surp_trial_based_on_rt[kk] <- ifelse (surpDat$surprise_key_resp.rt[kk] > 0.2, 1, 0)
    }
    number_of_removed_trials_in_the_surp <- nrow(surpDat) - (sum(surpDat$keep_surp_trial_based_on_rt))
    master_surpDat <- rbind(master_surpDat, surpDat)
    # Creating and Preparing the friendly dataframe.
    friendlyDat <- subset(remove_prac_trials, complete.cases(remove_prac_trials$FriendlyKey)) # keeps only the rows from the friendly task
    friendlyDat$FriendlyKey <- replace(friendlyDat$FriendlyKey, friendlyDat$FriendlyKey =='right', 8) # replace 8 values with right for the next loop.
    friendlyDat$FriendlyKey <- replace(friendlyDat$FriendlyKey, friendlyDat$FriendlyKey =='left', 1)
    friendlyDat <- subset(friendlyDat, complete.cases(friendlyDat$surpriseFaces))
    # adding a column to friendlyKey to show whether a trial should be kept or removed.
    # 1 means keep the trial and 0 means to remove it. At this stage, we don't remove a trial. We just mark them in the corresponding column.
    for (kk in 1:nrow(friendlyDat)){
      friendlyDat$keep_friendlyDat_trial_based_on_rt[kk] <- ifelse (friendlyDat$friendly_key_resp.rt[kk] > 0.2, 1, 0)
    }
    number_of_removed_trials_in_friendlyDat <- nrow(friendlyDat) - (sum(friendlyDat$keep_friendlyDat_trial_based_on_rt))
    rt_df[nrow(rt_df) + 1,] <-c(id, shortest_allowed_rt, number_of_removed_trials_in_the_surp, number_of_removed_trials_in_friendlyDat)
    master_friendlyDat <- rbind(master_friendlyDat, friendlyDat)
  }
}

write.csv(rt_df, paste(output_path, rt_fileName, sep = "/", collapse = NULL), row.names=FALSE)


rt_df <- read.csv(file = paste(output_path, rt_fileName, sep = "/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))

# removing participants based on if they just pressed the keys without actually paying attention to the task.
# We will exclude people who have more than 20% of surprise trials removed.
number_of_faces_in_surp_task <- nrow(surpDat)
twenty_percent_threshold <- round(0.2 * number_of_faces_in_surp_task)
rt_df <- filter(rt_df, number_of_removed_trials_in_the_surp <= twenty_percent_threshold)

num_of_participants_with_less_than_8_incong_errors <- 0 # counter
num_of_participants_with_less_than_8_incong_error_faces_in_surp <- 0 # counter
# Creating the main empty dataframe that will be filled with the data from the loop below:
main_df <- setNames(data.frame(matrix(ncol = 74, nrow = 0)), c("participant_id", "congAcc", "incongAcc", "congCorr_meanRT", "incongCorr_meanRT", "congCorr_logMeanRT", "incongCorr_logMeanRT",
                                                               "congErr_logMeanRT", "incongErr_logMeanRT",
                                                               "flankEff_meanACC", "flankEff_meanRT", "flankEff_logMeanRT",
                                                               "reported_errors", "committed_errors", "memoryBias_score",
                                                               "percent_incong_Faces_reported_old", "percent_incong_Faces_reported_new",
                                                               "percent_cong_faces_reported_old", "percent_post_incong_Faces_reported_new",
                                                               "percent_post_incong_Faces_reported_old", "percent_post_incong_errorFaces_reported_new",
                                                               "percent_post_cong_Faces_reported_old", "percent_post_cong_Faces_reported_new",
                                                               "percent_pre_incong_Faces_reported_old", "percent_pre_incong_Faces_reported_new",
                                                               "percent_pre_cong_Faces_reported_old", "percent_pre_cong_Faces_reported_new",
                                                               "percent_pre_cong_Faces_reported_friendly", "percent_pre_incong_Faces_reported_friendly",
                                                               "percent_post_incong_Faces_reported_friendly", "percent_post_cong_Faces_reported_friendly",
                                                               "percent_cong_Faces_reported_friendly", "percent_incong_Faces_reported_friendly",
                                                               "hit_num", "miss_num", "corr_rej_num", "false_alrams_num",
                                                               "incong_hit_num", "incong_miss_num",
                                                               "cong_hit_num", "cong_miss_num", "post_cong_hit_num", "post_cong_miss_num",
                                                               "pre_cong_hit_num", "pre_cong_miss_num", "pre_incong_hit_num", "pre_incong_miss_num",
                                                               "post_incong_hit_num", "post_incong_miss_num", "post_incong_hitRate", "pre_incong_hitRate",
                                                               "pre_cong_hitRate", "post_cong_hitRate", "cong_hitRate", "incong_hitRate",
                                                               "num_incong_errors", "num_incong_corrects", "num_post_cong_faces", "num_pre_cong_faces",
                                                               "num_pre_incong_faces", "num_post_incong_faces",
                                                               "num_pre_incong_Faces_reported_new", "num_incong_Faces_reported_new", "num_post_incong_Faces_reported_new",
                                                               "num_pre_cong_Faces_reported_new", "num_cong_faces_reported_new",
                                                               "num_post_cong_Faces_reported_new", "num_error_faces_in_surp", "pre_incong_trials_in_surp",
                                                               "post_incong_trials_in_surp", "incong_trials_in_surp", "pre_cong_trials_in_surp",
                                                               "cong_trials_in_surp", "post_cong_trials_in_surp"))

####### Looping over all participant datafiles for further compuations!
for(i in 1:length(datafiles_list)){
  #for this participant, find the csv file
  psychopy_file <- paste(input_path,datafiles_list[i], sep = "/", collapse = NULL)

  #read in the data for this participant, establish id, and remove extraneous variables
  psychopyDat <- read.csv(file = psychopy_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))
  participant_id <- psychopyDat$id[1]
  # Check whether this participant is excluded or not!
  participant_included <- nrow(filter(rt_df, id == participant_id)) # 0 indicates that this participant is excluded.
  if (participant_included == 1){
    participant_surpDat <- filter(master_surpDat, id == participant_id) # loading surpDat of this participant to a new dataframe for convenience!
    participant_friendlyDat <- filter(master_friendlyDat, id == participant_id)
    psychopyDatTrim <- psychopyDat[c("id",
                                     "new", # The displayed face is new? This column stores the correct value of the task not the response from the subject
                                     "newKey", # Which key should be pressed when the face is new!
                                     "congruent",
                                     "FriendlyKey",
                                     "stimNum",
                                     "accuracy",
                                     "task1_stim_keyResp.keys",
                                     "textbox_2.text", # stores the number of reported errors by subjects
                                     "surprise_key_resp.keys",
                                     "friendly_key_resp.keys",
                                     "surprise_key_resp.rt",
                                     "bigFace.started",
                                     "surpriseFaces",
                                     "straightFace",
                                     "task1_stim_keyResp.rt", #  this stores reaction time for each trial
                                     "task_trial_loop.thisTrialN")] # This stores the number of trial in a block; For this study it starts from 0 to 31
    #                                                                 as we have 32 trials in each block.

    #remove practice trials and any rows that do not reflect experiment data
    remove_first_row <- psychopyDatTrim[c(-1),]
    remove_prac_trials <- subset(remove_first_row, !complete.cases(remove_first_row$bigFace.started)) # removes practice trials
    # Calculate the overall accuracy in the main task
    accuracy <- mean(remove_prac_trials$accuracy, na.rm = TRUE)
    # Calculate the average accuracy in congruent trials
    congDat <- filter(remove_prac_trials, congruent ==1) # subset the data for congruent trials.
    congAcc <- mean(congDat$accuracy, na.rm = TRUE) # mean accuracy for congruent trials   !!!!!
    # Calculate the average accuracy in incongruent trials
    incongDat <- filter(remove_prac_trials, congruent ==0) # subset the data for incongruent trials.
    incongAcc <- mean(incongDat$accuracy, na.rm = TRUE) # mean accuracy for incongruent trials       !!!!!

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

    if (nrow(incong_errorDat) >= incong_error_cutoff){ # checks to see if the participant has at least 6 errors or not.

      # Checking to see if there are at least 6 incongruent error faces in the surpDat of this participant.
      participant_surpDat <- filter(participant_surpDat, keep_surp_trial_based_on_rt == 1) # removing bad surprise trials that we marked above.
      participant_friendlyDat <- filter(participant_friendlyDat, keep_friendlyDat_trial_based_on_rt == 1) # removing bad friendlyDat trials that we marked above.
      num_error_faces_in_surp <- 0
      for (rr in 1:nrow(incong_errorDat)){
        temp_face <- incong_errorDat$straightFace[rr]
        temp_for_surp <- filter(participant_surpDat, surpriseFaces == temp_face)
        errorFace_exist_in_surpDat <- ifelse(nrow(temp_for_surp) == 1, 1,0)
        if (errorFace_exist_in_surpDat == 1){
          num_error_faces_in_surp <- num_error_faces_in_surp + 1
        }
      }
      if (num_error_faces_in_surp >= incong_error_cutoff){
        # subset the data for correct trials only, separately for congruent and incongruent trials, creating new data frames for each
        cong_errDat <- errorDat[errorDat$congruent == 1,]
        incong_errDat <- errorDat[errorDat$congruent == 0,]

        cong_corrDat <- corrDat[corrDat$congruent == 1,]
        incong_corrDat <- corrDat[corrDat$congruent == 0,]
        #for correct trials, compute mean RT (raw and log-corrected)
        congCorr_meanRT <- mean(cong_corrDat$task1_stim_keyResp.rt)
        incongCorr_meanRT <- mean(incong_errDat$task1_stim_keyResp.rt)

        congErr_logMeanRT <- mean(log((1+cong_errDat$task1_stim_keyResp.rt))) #!!!!!
        incongErr_logMeanRT <- mean(log((1+incong_corrDat$task1_stim_keyResp.rt))) #!!!!!

        congCorr_logMeanRT <- mean(log((1+cong_corrDat$task1_stim_keyResp.rt))) #!!!!!
        incongCorr_logMeanRT <- mean(log((1+incong_corrDat$task1_stim_keyResp.rt))) #!!!!!

        # compute flanker-effect scores for accuracy, RT, log-RT
        flankEff_meanACC <- incongAcc - congAcc #!!!!!
        flankEff_meanRT <- incongCorr_meanRT - congCorr_meanRT
        flankEff_logMeanRT <- incongCorr_logMeanRT - congCorr_logMeanRT   #!!!!!

        # number of errors made in the main task
        committed_errors <- 0
        for (j in 1:nrow(keep_rows_with_acc_vals)){
          if (keep_rows_with_acc_vals$accuracy[j] == 0){
            committed_errors <- committed_errors +1
          }
        }
        reported_errors <- subset(remove_prac_trials, complete.cases(remove_prac_trials$textbox_2.text))
        reported_errors <- reported_errors$textbox_2.text # number of reported errors by participants
        reported_errors <- regmatches(reported_errors, gregexpr("[[:digit:]]+", reported_errors)) # creates a list of numbers and removes non-numeric characters.
        reported_errors <- as.numeric(unlist(reported_errors)) # unlists and creates a vector.
        reported_errors <- reported_errors[1] # There was a participant who had reported 70/100. Two lines of code above produced a vector of two
        # values 70 and 100. This brought about issues in percent_mainDat. To solve this issue temporarily, by using this fix, I only select
        # the first value.
        if (length(as.numeric(reported_errors)) == 0){ # in case a participant does not answer the question or have put letters instead of numbers, this code will prevent from future errors.
          reported_errors <- NA
          memoryBias_score <- NA
        } else {
          memoryBias_score <- ((reported_errors - as.numeric(committed_errors))/ abs(reported_errors)) # percent bias score calculation
        }
        num_incong_errors <- nrow(incong_errorDat)
        num_incong_corrects <- nrow(incong_corrDat)
        ######## SECTION 2: Surprise Task
        # Let's keep only the surprise trials that have faces from error trials in the main task. Then, we will be able to easily use that smaller dataframe to calculate the number of OLD faces among error trials.
        # Loop over the faces from error trials.
        # to prevent errors when there is no incong faces:
        if (nrow(incong_corrDat) != 0){
          num_incong_Faces_reported_old <- 0 # this is the number of incongruent faces that they report as OLD and will be updated in the loop below:
          for (iii in 1:nrow(incong_corrDat)){
            temp_face <- incong_corrDat$straightFace[iii]
            temp_for_surp <- filter(participant_surpDat, surpriseFaces == temp_face)
            if (nrow(temp_for_surp) == 1){
              identified_old_correctly <- ifelse (temp_for_surp$newKey != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
              if (identified_old_correctly == 1){
                num_incong_Faces_reported_old <- num_incong_Faces_reported_old + 1 # The number of incongruent error faces that they report as OLD
              }
            }
          }
        } else {
          num_incong_Faces_reported_old <- NA
        }
        if (!is.na(num_incong_Faces_reported_old)){
          num_incong_Faces_reported_new <- nrow(incong_corrDat) - num_incong_Faces_reported_old # stores the # of error faces that the participant incorrectly identifies as new.
          percent_incong_Faces_reported_old <- (num_incong_Faces_reported_old/(num_incong_Faces_reported_old + num_incong_Faces_reported_new)) * 100
        }
        if (length(percent_incong_Faces_reported_old) == 0){
          percent_incong_Faces_reported_old <- NA
          percent_incong_Faces_reported_new <- NA
        } else {
          percent_incong_Faces_reported_new <- 100 - percent_incong_Faces_reported_old
        }

        if (nrow(cong_corrDat) != 0){
          num_cong_faces_reported_old <- 0
          for (jjj in 1:nrow(cong_corrDat)){
            temp_face <- cong_corrDat$straightFace[jjj]
            temp_for_surp <- filter(participant_surpDat, surpriseFaces == temp_face) # find the error face in the surpDat
            if (nrow(temp_for_surp) == 1){
              identified_old_correctly <- ifelse (temp_for_surp$newKey != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
              if (identified_old_correctly == 1){
                num_cong_faces_reported_old <- num_cong_faces_reported_old + 1 # The number of correct faces that they report as OLD
              }
            }
          }
        } else {
          num_cong_faces_reported_old <- NA
        }
        if (!is.na(num_cong_faces_reported_old)){
          num_cong_faces_reported_new <- nrow(cong_corrDat) - num_cong_faces_reported_old # The number of correct faces that they report as new
          percent_cong_faces_reported_old <- (num_cong_faces_reported_old/(num_cong_faces_reported_old + num_cong_faces_reported_new)) * 100
        }

        if (length(percent_cong_faces_reported_old) == 0){
          percent_cong_faces_reported_old <- NA
          percent_cong_faces_reported_new <- NA
        } else {
          percent_cong_faces_reported_new <- 100 - percent_cong_faces_reported_old
        }
        ######################################
        #SECTION 3: Friendly Task

        if (nrow(incong_corrDat) != 0){
          num_incong_Faces_reported_friendly <- 0 # this is the number of error faces that they report as OLD and will be updated in the loop below:
          # Loop over the faces from error trials.
          for (hhhh in 1:nrow(incong_corrDat)){
            temp_face <- incong_corrDat$straightFace[hhhh]
            temp_for_friendly <- filter(participant_friendlyDat, surpriseFaces == temp_face) # find the error face in the friendlyDat
            if (nrow(temp_for_friendly) == 1){
              identified_friendly <- ifelse (temp_for_friendly$FriendlyKey == temp_for_friendly$friendly_key_resp.keys, 1, 0) #returns 1 when participant identifies the face as friendly!
              if (identified_friendly == 1){
                num_incong_Faces_reported_friendly <- num_incong_Faces_reported_friendly + 1 # The number of error faces that they report as friendly
              }
            }
          }
        } else {
          num_incong_Faces_reported_friendly <- NA
        }
        if (!is.na(num_incong_Faces_reported_friendly)){
          num_incong_Faces_reported_unfriendly <- nrow(incong_corrDat) - num_incong_Faces_reported_friendly # The number of correct faces that they report as new
          percent_incong_Faces_reported_friendly <- (num_incong_Faces_reported_friendly/(num_incong_Faces_reported_friendly + num_incong_Faces_reported_unfriendly)) * 100
        }

        if (length(percent_incong_Faces_reported_friendly) == 0){
          percent_incong_Faces_reported_friendly <- NA
          percent_incong_Faces_reported_unfriendly <- NA
        } else {
          percent_incong_Faces_reported_unfriendly <- 100 - percent_incong_Faces_reported_friendly
        }

        if (nrow(cong_corrDat) != 0){
          num_cong_Faces_reported_friendly <- 0
          for (tttt in 1:nrow(cong_corrDat)){
            temp_face <- cong_corrDat$straightFace[tttt]
            temp_for_friendly <- filter(participant_friendlyDat, surpriseFaces == temp_face) # find the error face in the friendlyDat
            if (nrow(temp_for_friendly) == 1){
              identified_friendly <- ifelse (temp_for_friendly$FriendlyKey == temp_for_friendly$friendly_key_resp.keys, 1, 0) #returns 1 when participant identifies the face as friendly!
              if (identified_friendly == 1){
                num_cong_Faces_reported_friendly <- num_cong_Faces_reported_friendly + 1 # The number of error faces that they report as OLD
              }
            }
          }
        } else {
          num_cong_Faces_reported_friendly <- NA
        }
        if (!is.na(num_cong_Faces_reported_friendly)){
          num_cong_Faces_reported_unfriendly <- nrow(cong_corrDat) - num_cong_Faces_reported_friendly # The number of correct faces that they report as new
          percent_cong_Faces_reported_friendly <- (num_cong_Faces_reported_friendly/(num_cong_Faces_reported_friendly + num_cong_Faces_reported_unfriendly)) * 100
        }
        if (length(percent_cong_Faces_reported_friendly) == 0){
          percent_cong_Faces_reported_friendly <- NA
          percent_cong_Faces_reported_unfriendly <- NA
        } else {
          percent_cong_Faces_reported_unfriendly <- 100 - percent_cong_Faces_reported_friendly
        }

        ######################################
        #Section 4: Do they remember post-incong faces?
        # I should loop over the data frame that has only the main task with accuracy vals, i.e., keep_rows_with_acc_vals
        # The goal is to create a data frame of post-error faces. We will just use incongruent pre/post data.
        post_incong_faces <- c() # will be filled in the loop below.
        for (aa in 1:nrow(keep_rows_with_acc_vals)){
          next_idx <- aa + 1
          if (keep_rows_with_acc_vals$accuracy[aa] ==1){
            if (keep_rows_with_acc_vals$congruent[aa] == 0){
              if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[aa] == 31){ # Trial #31 is the last trial in a block and
                # the face after that is the first trial of the next block. So, that face cannot be among post_error_faces.
                post_incong_faces <- post_incong_faces
              } else {
                if (keep_rows_with_acc_vals$accuracy[next_idx] ==1){ # to have only correct trials in the post_incong
                  if (keep_rows_with_acc_vals$congruent[next_idx] ==0){
                    post_incong_faces <- rbind(post_incong_faces, keep_rows_with_acc_vals[next_idx,])
                  }
                }
              }
            }
          }
        }
        if (!is.null(post_incong_faces)){
          post_incong_faces <- subset(post_incong_faces, complete.cases(post_incong_faces$id))
          # Let's find out how many of the post-error faces were correctly identified as OLD!
          num_post_incong_Faces_reported_old <- 0 # this is the number of error faces that they report as OLD and will be updated in the loop below:
          for (bb in 1:nrow(post_incong_faces)){
            temp_face <- post_incong_faces$straightFace[bb]
            temp_for_surp <- filter(participant_surpDat, surpriseFaces == temp_face)
            if (nrow(temp_for_surp) == 1){
              identified_old_correctly <- ifelse (temp_for_surp$newKey != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
              if (identified_old_correctly == 1){
                num_post_incong_Faces_reported_old <- num_post_incong_Faces_reported_old + 1 # The number of post-error faces that they report as OLD  1111111111111111111
              }
            }
          }
        } else {
          num_post_incong_Faces_reported_old <- NA
        }
        if (!is.na(num_post_incong_Faces_reported_old)){
          num_post_incong_Faces_reported_new <- nrow(post_incong_faces) - num_post_incong_Faces_reported_old
          percent_post_incong_Faces_reported_old <- (num_post_incong_Faces_reported_old/(num_post_incong_Faces_reported_old + num_post_incong_Faces_reported_new)) * 100
        }
        if (length(percent_post_incong_Faces_reported_old) ==0){
          percent_post_incong_Faces_reported_old <- NA
          percent_post_incong_Faces_reported_new <- NA
        } else {
          percent_post_incong_Faces_reported_new <- 100 - percent_post_incong_Faces_reported_old
        }

        # What about post-cong faces?
        post_cong_faces <- c()
        for (cc in 1:nrow(keep_rows_with_acc_vals)){
          next_idx <- cc + 1
          if (keep_rows_with_acc_vals$accuracy[cc] ==1){
            if (keep_rows_with_acc_vals$congruent[cc] == 1){
              if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[cc] == 31){ #Checks if this is the last trial of the block.
                post_cong_faces <- post_cong_faces
              } else {
                if (keep_rows_with_acc_vals$accuracy[next_idx] ==1){ # to have only correct trials in the post_correct
                  if (keep_rows_with_acc_vals$congruent[next_idx] ==0){
                    post_cong_faces <- rbind(post_cong_faces, keep_rows_with_acc_vals[next_idx,])
                  }
                }
              }
            }
          }
        }
        if (!is.null(post_cong_faces)){
          post_cong_faces <- subset(post_cong_faces, complete.cases(post_cong_faces$id))
          # Let's find out how many of the post-correct faces were correctly identified as OLD!
          num_post_cong_Faces_reported_old <- 0 # this is the number of correct faces that they report as OLD and will be updated in the loop below:
          for (dd in 1:nrow(post_cong_faces)){
            temp_face <- post_cong_faces$straightFace[dd]
            temp_for_surp <- filter(participant_surpDat, surpriseFaces == temp_face)
            if (nrow(temp_for_surp) == 1){
              identified_old_correctly <- ifelse (temp_for_surp$newKey != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
              if (identified_old_correctly == 1){
                num_post_cong_Faces_reported_old <- num_post_cong_Faces_reported_old + 1 # The number of post-correct faces that they report as OLD  1111111111111111111
              }
            }
          }
        } else {
          num_post_cong_Faces_reported_old <- NA
        }
        if (!is.na(num_post_cong_Faces_reported_old)){
          num_post_cong_Faces_reported_new <- nrow(post_cong_faces) - num_post_cong_Faces_reported_old
          percent_post_cong_Faces_reported_old <- (num_post_cong_Faces_reported_old/(num_post_cong_Faces_reported_old + num_post_cong_Faces_reported_new)) * 100
        }

        if (length(percent_post_cong_Faces_reported_old) ==0){
          percent_post_cong_Faces_reported_old <- NA
          percent_post_cong_Faces_reported_new <- NA
        } else {
          percent_post_cong_Faces_reported_new <- 100 - percent_post_cong_Faces_reported_old
        }

        ##############
        #Section 5: Do they remember pre-incong faces?
        pre_incong_faces <- c() # will be filled in the loop below.
        for (aa in 1:nrow(keep_rows_with_acc_vals)){
          prior_idx <- aa - 1
          if (keep_rows_with_acc_vals$accuracy[aa] ==1){
            if (keep_rows_with_acc_vals$congruent[aa] ==0){
              if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[aa] == 0){ #Checks if this is the first trial of the block.
                pre_incong_faces <- pre_incong_faces
              } else {
                if (keep_rows_with_acc_vals$accuracy[prior_idx] ==1){ # to have only correct trials in the pre_incong
                  if (keep_rows_with_acc_vals$congruent[prior_idx] ==0){
                    pre_incong_faces <- rbind(pre_incong_faces, keep_rows_with_acc_vals[prior_idx,])
                  }
                }
              }
            }
          }
        }
        if (!is.null(pre_incong_faces)){
          pre_incong_faces <- subset(pre_incong_faces, complete.cases(pre_incong_faces$id))
          # Let's find out how many of the pre-incong faces were correctly identified as OLD!
          num_pre_incong_Faces_reported_old <- 0 # this is the number of correct faces that they report as OLD and will be updated in the loop below:
          for (zaman2 in 1:nrow(pre_incong_faces)){
            temp_face <- pre_incong_faces$straightFace[zaman2]
            temp_for_surp <- filter(participant_surpDat, surpriseFaces == temp_face)
            if (nrow(temp_for_surp) == 1){
              identified_old_correctly <- ifelse (temp_for_surp$newKey != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
              if (identified_old_correctly == 1){
                num_pre_incong_Faces_reported_old <- num_pre_incong_Faces_reported_old + 1 # The number of pre-error faces that they report as OLD  1111111111111111111
              }
            }
          }
        } else {
          num_pre_incong_Faces_reported_old <- NA
        }
        if (!is.na(num_pre_incong_Faces_reported_old)){
          num_pre_incong_Faces_reported_new <- nrow(pre_incong_faces) - num_pre_incong_Faces_reported_old
          percent_pre_incong_Faces_reported_old <- (num_pre_incong_Faces_reported_old/(num_pre_incong_Faces_reported_old + num_pre_incong_Faces_reported_new)) * 100
        }
        if (length(percent_pre_incong_Faces_reported_old) == 0){
          percent_pre_incong_Faces_reported_old <- NA
          percent_pre_incong_Faces_reported_new <- NA
        } else {
          percent_pre_incong_Faces_reported_new <- 100 - percent_pre_incong_Faces_reported_old
        }

        pre_cong_faces <- c()
        for (bb in 1:nrow(keep_rows_with_acc_vals)){
          prior_idx <- bb - 1
          if (keep_rows_with_acc_vals$accuracy[bb] ==1){
            if (keep_rows_with_acc_vals$congruent[bb] ==1){
              if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[bb] == 0){ #Checks if this is the first trial of the block.
                pre_cong_faces <- pre_cong_faces
              } else {
                if (keep_rows_with_acc_vals$accuracy[prior_idx] ==1){ # to have only correct trials in the pre_correct
                  if (keep_rows_with_acc_vals$congruent[prior_idx] ==0){
                    pre_cong_faces <- rbind(pre_cong_faces, keep_rows_with_acc_vals[prior_idx,])
                  }
                }
              }
            }
          }
        }
        if (!is.null(pre_cong_faces)){
          # What about pre-cong faces?
          pre_cong_faces <- subset(pre_cong_faces, complete.cases(pre_cong_faces$id))
          # Let's find out how many of the post-correct faces were correctly identified as OLD!
          num_pre_cong_Faces_reported_old <- 0 # this is the number of correct faces that they report as OLD and will be updated in the loop below:
          for (aa in 1:nrow(pre_cong_faces)){
            temp_face <- pre_cong_faces$straightFace[aa]
            temp_for_surp <- filter(participant_surpDat, surpriseFaces == temp_face)
            if (nrow(temp_for_surp) == 1){
              identified_old_correctly <- ifelse (temp_for_surp$newKey != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
              if (identified_old_correctly == 1){
                num_pre_cong_tFaces_reported_old <- num_pre_cong_Faces_reported_old + 1 # The number of pre-correct faces that they report as OLD  1111111111111111111
              }
            }
          }
        } else {
          num_pre_cong_Faces_reported_old <- NA
        }
        if (!is.na(num_pre_cong_Faces_reported_old)){
          num_pre_cong_Faces_reported_new <- nrow(pre_cong_faces) - num_pre_cong_Faces_reported_old
          percent_pre_cong_Faces_reported_old <- (num_pre_cong_Faces_reported_old/(num_pre_cong_Faces_reported_old + num_pre_cong_Faces_reported_new)) * 100
        }

        if (length(percent_pre_cong_Faces_reported_old) == 0){
          percent_pre_cong_Faces_reported_old <- NA
          percent_pre_cong_Faces_reported_new <- NA
        } else {
          percent_pre_cong_Faces_reported_new <- 100 - percent_pre_cong_Faces_reported_old
        }

        num_post_cong_faces <- nrow(post_cong_faces)
        if (is.null(num_post_cong_faces)){
          num_post_cong_faces <- NA
        }
        num_pre_cong_faces <- nrow(pre_cong_faces)
        if (is.null(num_pre_cong_faces)){
          num_pre_cong_faces <- NA
        }
        num_pre_incong_faces <- nrow(pre_incong_faces)
        if (is.null(num_pre_incong_faces)){
          num_pre_incong_faces <- NA
        }
        num_post_incong_faces <- nrow(post_incong_faces)
        if (is.null(num_post_incong_faces)){
          num_post_incong_faces <- NA
        }

        ##############
        #How do they rate post-error faces? [friendly vs. unfriendly]
        if (!is.null(post_incong_faces)){
          num_post_incong_Faces_reported_friendly <- 0
          for (tttt in 1:nrow(post_incong_faces)){
            temp_face <- post_incong_faces$straightFace[tttt]
            temp_for_friendly <- filter(participant_friendlyDat, surpriseFaces == temp_face) # find the error face in the friendlyDat
            if (nrow(temp_for_friendly) == 1){
              identified_friendly <- ifelse (temp_for_friendly$FriendlyKey == temp_for_friendly$friendly_key_resp.keys, 1, 0) #returns 1 when participant identifies the face as friendly!
              if (identified_friendly == 1){
                num_post_incong_Faces_reported_friendly <- num_post_incong_Faces_reported_friendly + 1 # The number of post_error faces that they report as friendly  1111111111111111111
              }
            }
          }
        } else {
          num_post_incong_Faces_reported_friendly <- NA
        }
        if (!is.na(num_post_incong_Faces_reported_friendly)){
          num_post_incong_Faces_reported_unfriendly <- nrow(post_incong_faces) - num_post_incong_Faces_reported_friendly # The number of correct faces that they report as new
          percent_post_incong_Faces_reported_friendly <- (num_post_incong_Faces_reported_friendly/(num_post_incong_Faces_reported_friendly + num_post_incong_Faces_reported_unfriendly)) * 100
        }

        if (length(percent_post_incong_Faces_reported_friendly) == 0){
          percent_post_incong_Faces_reported_friendly <- NA
          percent_post_incong_Faces_reported_unfriendly <- NA
        } else {
          percent_post_incong_Faces_reported_unfriendly <- 100 - percent_post_incong_Faces_reported_friendly
        }

        # What about post-correct faces?
        if (!is.null(post_cong_faces)){
          num_post_cong_Faces_reported_friendly <- 0
          for (tttt in 1:nrow(post_cong_faces)){
            temp_face <- post_cong_faces$straightFace[tttt]
            temp_for_friendly <- filter(participant_friendlyDat, surpriseFaces == temp_face) # find the error face in the friendlyDat
            if (nrow(temp_for_friendly) == 1){
              identified_friendly <- ifelse (temp_for_friendly$FriendlyKey == temp_for_friendly$friendly_key_resp.keys, 1, 0) #returns 1 when participant identifies the face as friendly!
              if (identified_friendly == 1){
                num_post_cong_Faces_reported_friendly <- num_post_cong_Faces_reported_friendly + 1 # The number of post_correct faces that they report as friendly  1111111111111111111
              }
            }
          }
        } else {
          num_post_cong_Faces_reported_friendly <- NA
        }
        if (!is.na(num_post_cong_Faces_reported_friendly)){
          num_post_cong_Faces_reported_unfriendly <- nrow(post_cong_faces) - num_post_cong_Faces_reported_friendly # The number of correct faces that they report as new
          percent_post_cong_Faces_reported_friendly <- (num_post_cong_Faces_reported_friendly/(num_post_cong_Faces_reported_friendly + num_post_cong_Faces_reported_unfriendly)) * 100
        }

        if (length(percent_post_cong_Faces_reported_friendly) == 0){
          percent_post_cong_Faces_reported_friendly <- NA
          percent_post_cong_Faces_reported_unfriendly <- NA
        } else {
          percent_post_cong_Faces_reported_unfriendly <- 100 - percent_post_cong_Faces_reported_friendly
        }
        # What about pre-incong faces?
        if (!is.null(pre_incong_faces)){
          num_pre_incong_Faces_reported_friendly <- 0
          for (tttt in 1:nrow(pre_incong_faces)){
            temp_face <- pre_incong_faces$straightFace[tttt]
            temp_for_friendly <- filter(participant_friendlyDat, surpriseFaces == temp_face) # find the error face in the friendlyDat
            if (nrow(temp_for_friendly) == 1){
              identified_friendly <- ifelse (temp_for_friendly$FriendlyKey == temp_for_friendly$friendly_key_resp.keys, 1, 0) #returns 1 when participant identifies the face as friendly!
              if (identified_friendly == 1){
                num_pre_incong_Faces_reported_friendly <- num_pre_incong_Faces_reported_friendly + 1 # The number of pre_error faces that they report as friendly  1111111111111111111
              }
            }
          }
        } else {
          num_pre_incong_Faces_reported_friendly <- NA
        }
        if (!is.na(num_pre_incong_Faces_reported_friendly)){
          num_pre_incong_Faces_reported_unfriendly <- nrow(pre_incong_faces) - num_pre_incong_Faces_reported_friendly
          percent_pre_incong_Faces_reported_friendly <- (num_pre_incong_Faces_reported_friendly/(num_pre_incong_Faces_reported_friendly + num_pre_incong_Faces_reported_unfriendly)) * 100
        }

        if (length(percent_pre_incong_Faces_reported_friendly) ==0){
          percent_pre_incong_Faces_reported_friendly <- NA
          percent_pre_incong_Faces_reported_unfriendly <- NA
        } else {
          percent_pre_incong_Faces_reported_unfriendly <- 100 - percent_pre_incong_Faces_reported_friendly
        }

        # What about pre-correct faces?
        if (!is.null(pre_cong_faces)){
          num_pre_cong_Faces_reported_friendly <- 0
          for (tttt in 1:nrow(pre_cong_faces)){
            temp_face <- pre_cong_faces$straightFace[tttt]
            temp_for_friendly <- filter(participant_friendlyDat, surpriseFaces == temp_face) # find the error face in the friendlyDat
            if (nrow(temp_for_friendly) == 1){
              identified_friendly <- ifelse (temp_for_friendly$FriendlyKey == temp_for_friendly$friendly_key_resp.keys, 1, 0) #returns 1 when participant identifies the face as friendly!
              if (identified_friendly == 1){
                num_pre_cong_Faces_reported_friendly <- num_pre_cong_Faces_reported_friendly + 1 # The number of pre_error faces that they report as friendly  1111111111111111111
              }
            }
          }
        } else {
          num_pre_cong_Faces_reported_friendly <- NA
        }
        if (!is.na(num_pre_cong_Faces_reported_friendly)){
          num_pre_cong_Faces_reported_unfriendly <- nrow(pre_cong_faces) - num_pre_cong_Faces_reported_friendly
          percent_pre_cong_Faces_reported_friendly <- (num_pre_cong_Faces_reported_friendly/(num_pre_cong_Faces_reported_friendly + num_pre_cong_Faces_reported_unfriendly)) * 100
        }

        if (length(percent_pre_cong_Faces_reported_friendly) == 0){
          percent_pre_cong_Faces_reported_friendly <- NA
          percent_pre_cong_Faces_reported_unfriendly <- NA
        } else {
          percent_pre_cong_Faces_reported_unfriendly <- 100 - percent_pre_cong_Faces_reported_friendly
        }
        ########################## SIGNAL DETECTION THEORY ########################################
        # We make use of SDT in the surprise memory task.
        # OLD is our target.
        # OLD faces are going to be divided into old_errorFaces and old_correctFaces
        ###########################################################################################
        # Overall hits, misses, FAs, and Correct rejections regardless of errors and corrects.
        hit_num <- 0
        miss_num <- 0
        corr_rej_num <- 0 # Correct rejections number is the same for Errors, corrects as it is overall. It means they identify new faces as new!
        false_alrams_num <- 0
        for (ss in 1:nrow(participant_surpDat)){
          temp_cut_from_surp <- participant_surpDat$surpriseFaces[ss]
          temp_cut_from_flanker <- filter(keep_rows_with_acc_vals, straightFace == temp_cut_from_surp)
          if (nrow(temp_cut_from_flanker) == 1){ # The face is old
            if (participant_surpDat$newKey[ss] == participant_surpDat$surprise_key_resp.keys[ss]){
              miss_num <- miss_num + 1
            } else if (participant_surpDat$newKey[ss] != participant_surpDat$surprise_key_resp.keys[ss]){
              hit_num <- hit_num + 1
            }
          } else if (nrow(temp_cut_from_flanker) == 0){ # The face is new
            if (participant_surpDat$newKey[ss] == participant_surpDat$surprise_key_resp.keys[ss]){
              corr_rej_num <- corr_rej_num + 1
            } else if (participant_surpDat$newKey[ss] != participant_surpDat$surprise_key_resp.keys[ss]){
              false_alrams_num <- false_alrams_num + 1
            }
          }
        }
        # I need the followings for the long format dataframe.
        pre_incong_trials_in_surp <- num_pre_incong_Faces_reported_old + num_pre_incong_Faces_reported_new
        post_incong_trials_in_surp <- num_post_incong_Faces_reported_old + num_post_incong_Faces_reported_new
        incong_trials_in_surp <- num_incong_Faces_reported_old + num_incong_Faces_reported_new
        pre_cong_trials_in_surp <- num_pre_cong_Faces_reported_old + num_pre_cong_Faces_reported_new
        cong_trials_in_surp <- num_cong_faces_reported_old + num_cong_faces_reported_new
        post_cong_trials_in_surp <- num_post_cong_Faces_reported_old + num_post_cong_Faces_reported_new

        # False alaram numbers is the same for Error, and correct faces as it is the overall FA! I computed it above with "false_alrams_num" name.
        incong_hit_num <- num_incong_Faces_reported_old
        incong_miss_num <- num_incong_Faces_reported_new
        incong_hitRate <- (incong_hit_num) / ((incong_hit_num) + incong_miss_num) # hit rate


        cong_hit_num <- num_cong_faces_reported_old
        cong_miss_num <- num_cong_faces_reported_new
        cong_hitRate <- (cong_hit_num) / ((cong_hit_num) + cong_miss_num) # hit rate

        post_cong_hit_num <- num_post_cong_Faces_reported_old
        post_cong_miss_num <- num_post_cong_Faces_reported_new
        post_cong_hitRate <- (post_cong_hit_num) / ((post_cong_hit_num) + post_cong_miss_num) # hit rate

        pre_cong_hit_num <- num_pre_cong_Faces_reported_old
        pre_cong_miss_num <- num_pre_cong_Faces_reported_new
        pre_cong_hitRate <- (pre_cong_hit_num) / ((pre_cong_hit_num) + pre_cong_miss_num) # hit rate

        pre_incong_hit_num <- num_pre_incong_Faces_reported_old
        pre_incong_miss_num <- num_pre_incong_Faces_reported_new
        pre_incong_hitRate <- (pre_incong_hit_num) / ((pre_incong_hit_num) + pre_incong_miss_num) # hit rate

        post_incong_hit_num <- num_post_incong_Faces_reported_old
        post_incong_miss_num <- num_post_incong_Faces_reported_new
        post_incong_hitRate <- (post_incong_hit_num) / ((post_incong_hit_num) + post_incong_miss_num) # hit rate



        ##########
        main_df[nrow(main_df) + 1,] <-c(participant_id, congAcc, incongAcc, congCorr_meanRT, incongCorr_meanRT, congCorr_logMeanRT, congErr_logMeanRT,
                                        incongErr_logMeanRT, incongCorr_logMeanRT, flankEff_meanACC, flankEff_meanRT, flankEff_logMeanRT,
                                        reported_errors, committed_errors, memoryBias_score, percent_incong_Faces_reported_old,
                                        percent_incong_Faces_reported_new, percent_cong_faces_reported_old,
                                        percent_cong_faces_reported_new, percent_post_incong_Faces_reported_old,
                                        percent_post_incong_Faces_reported_new, percent_post_cong_Faces_reported_old,
                                        percent_post_cong_Faces_reported_new,
                                        percent_pre_incong_Faces_reported_old, percent_pre_incong_Faces_reported_new,
                                        percent_pre_cong_Faces_reported_old, percent_pre_cong_Faces_reported_new,
                                        percent_pre_cong_Faces_reported_friendly, percent_pre_incong_Faces_reported_friendly,
                                        percent_post_incong_Faces_reported_friendly, percent_post_cong_Faces_reported_friendly,
                                        percent_incong_Faces_reported_friendly, percent_cong_Faces_reported_friendly,
                                        hit_num, miss_num, corr_rej_num, false_alrams_num, incong_hit_num, incong_miss_num,
                                        cong_hit_num, cong_miss_num, post_cong_hit_num, post_cong_miss_num,
                                        pre_cong_hit_num, pre_cong_miss_num, pre_incong_hit_num, pre_incong_miss_num,
                                        post_incong_hit_num, post_incong_miss_num, post_incong_hitRate, pre_incong_hitRate,
                                        pre_cong_hitRate, post_cong_hitRate, cong_hitRate, incong_hitRate,
                                        num_incong_errors, num_incong_corrects, num_post_cong_faces, num_pre_cong_faces, num_pre_incong_faces, num_post_incong_faces,
                                        num_pre_incong_Faces_reported_new, num_incong_Faces_reported_new, num_post_incong_Faces_reported_new,
                                        num_pre_cong_Faces_reported_new, num_cong_faces_reported_new,
                                        num_post_cong_Faces_reported_new, num_error_faces_in_surp, pre_incong_trials_in_surp, post_incong_trials_in_surp, incong_trials_in_surp, pre_cong_trials_in_surp, cong_trials_in_surp, post_cong_trials_in_surp)
      } else {
        num_of_participants_with_less_than_8_incong_error_faces_in_surp <- num_of_participants_with_less_than_8_incong_error_faces_in_surp + 1
      }
    } else {
      num_of_participants_with_less_than_8_incong_errors <- num_of_participants_with_less_than_8_incong_errors + 1
    }
  }
}



################# Computing d prime using Psycho library #################################
dprime.stats <- psycho::dprime(as.numeric(main_df$hit_num), as.numeric(main_df$false_alrams_num), as.numeric(main_df$miss_num), as.numeric(main_df$corr_rej_num))
main_df$dprime <- dprime.stats$dprime

d_prime_incong.stats <- psycho::dprime(as.numeric(main_df$incong_hit_num), as.numeric(main_df$false_alrams_num), as.numeric(main_df$incong_miss_num), as.numeric(main_df$corr_rej_num))
main_df$d_prime_incong <- d_prime_incong.stats$dprime

pre_d_prime_incong.stats <- psycho::dprime(as.numeric(main_df$pre_incong_hit_num), as.numeric(main_df$false_alrams_num), as.numeric(main_df$pre_incong_miss_num), as.numeric(main_df$corr_rej_num))
main_df$pre_d_prime_incong <- pre_d_prime_incong.stats$dprime

post_d_prime_incong.stats <- psycho::dprime(as.numeric(main_df$post_incong_hit_num), as.numeric(main_df$false_alrams_num), as.numeric(main_df$post_incong_miss_num), as.numeric(main_df$corr_rej_num))
main_df$post_d_prime_incong <- post_d_prime_incong.stats$dprime

d_prime_cong.stats <- psycho::dprime(as.numeric(main_df$cong_hit_num), as.numeric(main_df$false_alrams_num), as.numeric(main_df$cong_miss_num), as.numeric(main_df$corr_rej_num))
main_df$d_prime_cong <- d_prime_cong.stats$dprime

pre_d_prime_cong.stats <- psycho::dprime(as.numeric(main_df$pre_cong_hit_num), as.numeric(main_df$false_alrams_num), as.numeric(main_df$pre_cong_miss_num), as.numeric(main_df$corr_rej_num))
main_df$pre_d_prime_cong <- pre_d_prime_cong.stats$dprime

post_d_prime_cong.stats <- psycho::dprime(as.numeric(main_df$post_cong_hit_num), as.numeric(main_df$false_alrams_num), as.numeric(main_df$post_cong_miss_num), as.numeric(main_df$corr_rej_num))
main_df$post_d_prime_cong <- post_d_prime_cong.stats$dprime

for (ee in 1:nrow(main_df)){
  main_df$d_prime_incong_minus_cong[ee] <- main_df$d_prime_incong[ee] - main_df$d_prime_cong[ee]
  main_df$post_d_prime_incong_minus_cong[ee] <- main_df$post_d_prime_incong[ee] - main_df$post_d_prime_cong[ee]
}
for (ee in 1:nrow(main_df)){
  main_df$hitRate_incong_minus_cong[ee] <- main_df$incong_hitRate[ee] - main_df$cong_hitRate[ee]
  main_df$hitRate_post_incong_minus_cong[ee] <- main_df$post_incong_hitRate[ee] - main_df$post_cong_hitRate[ee]
}




### Loading RedCap questionnaire data
redcapDat <- read.csv(file = "/Users/kihossei/OneDrive - Florida International University/Projects/Memory_for_error/redcap_data_from_sfe/202203v0socialflanke_SCRD_2022-09-23_1133.csv")

# Keeping the columns that we need!
redcapDat <- redcapDat[c("record_id", "scaared_b_scrdSoc_s1_r1_e1", "scaared_b_scrdGA_s1_r1_e1", "scaared_b_scrdTotal_s1_r1_e1", "bfne_b_scrdTotal_s1_r1_e1")]

# adding new columns to the "percent_mainDat" dataframe from redcapDat
for (rr in 1:nrow(main_df)){
  temp_id <- main_df$participant_id[rr]
  tempDat <- filter(redcapDat, record_id == temp_id)
  main_df$scaared_b_scrdSoc_s1_r1_e1[rr] <- tempDat$scaared_b_scrdSoc_s1_r1_e1
}
for (rr in 1:nrow(main_df)){
  temp_id <- main_df$participant_id[rr]
  tempDat <- filter(redcapDat, record_id == temp_id)
  main_df$scaared_b_scrdGA_s1_r1_e1[rr] <- tempDat$scaared_b_scrdGA_s1_r1_e1
}
for (rr in 1:nrow(main_df)){
  temp_id <- main_df$participant_id[rr]
  tempDat <- filter(redcapDat, record_id == temp_id)
  main_df$scaared_b_scrdTotal_s1_r1_e1[rr] <- tempDat$scaared_b_scrdTotal_s1_r1_e1
}
for (rr in 1:nrow(main_df)){
  temp_id <- main_df$participant_id[rr]
  tempDat <- filter(redcapDat, record_id == temp_id)
  main_df$bfne_b_scrdTotal_s1_r1_e1[rr] <- tempDat$bfne_b_scrdTotal_s1_r1_e1
}
# Removing outliers for variables of interest
# list of variables of interest: memoryBias_score, d_prime_incong, d_prime_cong, post_d_prime_incong, post_d_prime_cong, scaared_b_scrdSoc_s1_r1_e1, scaared_b_scrdTotal_s1_r1_e1, scaared_b_scrdGA_s1_r1_e1, d_prime_incong_minus_correct ,post_d_prime_incong_minus_correct, unfriendly_error_minus_correct, unfriendly_post_error_minus_correct!
mean_memoryBias_score <- mean(as.numeric(main_df$memoryBias_score), na.rm = TRUE)
sd_memoryBias_score_threeTimes <- 3*sd(as.numeric(main_df$memoryBias_score), na.rm = TRUE)

for (zesht4 in 1:nrow(main_df)){
  if (!is.na(as.numeric(main_df$memoryBias_score[zesht4])) && !is.na(as.numeric(main_df$percent_pre_cong_Faces_reported_friendly[zesht4])) && !is.na(as.numeric(main_df$percent_pre_incong_Faces_reported_friendly[zesht4])) && !is.na(as.numeric(main_df$percent_post_incong_Faces_reported_friendly[zesht4])) && !is.na(as.numeric(main_df$percent_post_cong_Faces_reported_friendly[zesht4])) && !is.na(as.numeric(main_df$percent_cong_Faces_reported_friendly[zesht4])) && !is.na(as.numeric(main_df$percent_incong_Faces_reported_friendly[zesht4])) && !is.na(as.numeric(main_df$incong_hitRate[zesht4])) && !is.na(as.numeric(main_df$cong_hitRate[zesht4])) && !is.na(as.numeric(main_df$post_incong_hitRate[zesht4])) && !is.na(as.numeric(main_df$post_cong_hitRate[zesht4])) && !is.na(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1[zesht4])) && !is.na(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1[zesht4])) && !is.na(as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1[zesht4])) && !is.na(as.numeric(main_df$hitRate_incong_minus_cong[zesht4])) && !is.na(as.numeric(main_df$hitRate_post_incong_minus_cong[zesht4])) && !is.na(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1[zesht4]))){
    if (as.numeric(main_df$memoryBias_score[zesht4]) >= (mean_memoryBias_score - sd_memoryBias_score_threeTimes) && as.numeric(main_df$memoryBias_score[zesht4]) <= (mean_memoryBias_score + sd_memoryBias_score_threeTimes)){
      main_df$memoryBias_score[zesht4] <- as.numeric(main_df$memoryBias_score[zesht4])
    } else {
      main_df$memoryBias_score[zesht4] <- NA
    }
    if (as.numeric(main_df$incong_hitRate[zesht4]) >= (mean(as.numeric(main_df$incong_hitRate), na.rm = TRUE) - (3*sd(as.numeric(main_df$incong_hitRate), na.rm = TRUE))) && as.numeric(main_df$incong_hitRate[zesht4]) <= (mean(as.numeric(main_df$incong_hitRate), na.rm = TRUE) + (3*sd(as.numeric(main_df$incong_hitRate), na.rm = TRUE)))){
      main_df$incong_hitRate[zesht4] <- as.numeric(main_df$incong_hitRate[zesht4])
    } else {
      main_df$incong_hitRate[zesht4] <- NA
    }

    if (as.numeric(main_df$percent_pre_cong_Faces_reported_friendly[zesht4]) >= (mean(as.numeric(main_df$percent_pre_cong_Faces_reported_friendly), na.rm = TRUE) - (3*sd(as.numeric(main_df$percent_pre_cong_Faces_reported_friendly), na.rm = TRUE))) && as.numeric(main_df$percent_pre_cong_Faces_reported_friendly[zesht4]) <= (mean(as.numeric(main_df$percent_pre_cong_Faces_reported_friendly), na.rm = TRUE) + (3*sd(as.numeric(main_df$percent_pre_cong_Faces_reported_friendly), na.rm = TRUE)))){
      main_df$percent_pre_cong_Faces_reported_friendly[zesht4] <- as.numeric(main_df$percent_pre_cong_Faces_reported_friendly[zesht4])
    } else {
      main_df$percent_pre_cong_Faces_reported_friendly[zesht4] <- NA
    }

    if (as.numeric(main_df$percent_pre_incong_Faces_reported_friendly[zesht4]) >= (mean(as.numeric(main_df$percent_pre_incong_Faces_reported_friendly), na.rm = TRUE) - (3*sd(as.numeric(main_df$percent_pre_incong_Faces_reported_friendly), na.rm = TRUE))) && as.numeric(main_df$percent_pre_incong_Faces_reported_friendly[zesht4]) <= (mean(as.numeric(main_df$percent_pre_incong_Faces_reported_friendly), na.rm = TRUE) + (3*sd(as.numeric(main_df$percent_pre_incong_Faces_reported_friendly), na.rm = TRUE)))){
      main_df$percent_pre_incong_Faces_reported_friendly[zesht4] <- as.numeric(main_df$percent_pre_incong_Faces_reported_friendly[zesht4])
    } else {
      main_df$percent_pre_incong_Faces_reported_friendly[zesht4] <- NA
    }

    if (as.numeric(main_df$percent_post_incong_Faces_reported_friendly[zesht4]) >= (mean(as.numeric(main_df$percent_post_incong_Faces_reported_friendly), na.rm = TRUE) - (3*sd(as.numeric(main_df$percent_post_incong_Faces_reported_friendly), na.rm = TRUE))) && as.numeric(main_df$percent_post_incong_Faces_reported_friendly[zesht4]) <= (mean(as.numeric(main_df$percent_post_incong_Faces_reported_friendly), na.rm = TRUE) + (3*sd(as.numeric(main_df$percent_post_incong_Faces_reported_friendly), na.rm = TRUE)))){
      main_df$percent_post_incong_Faces_reported_friendly[zesht4] <- as.numeric(main_df$percent_post_incong_Faces_reported_friendly[zesht4])
    } else {
      main_df$percent_post_incong_Faces_reported_friendly[zesht4] <- NA
    }

    if (as.numeric(main_df$percent_post_cong_Faces_reported_friendly[zesht4]) >= (mean(as.numeric(main_df$percent_post_cong_Faces_reported_friendly), na.rm = TRUE) - (3*sd(as.numeric(main_df$percent_post_cong_Faces_reported_friendly), na.rm = TRUE))) && as.numeric(main_df$percent_post_cong_Faces_reported_friendly[zesht4]) <= (mean(as.numeric(main_df$percent_post_cong_Faces_reported_friendly), na.rm = TRUE) + (3*sd(as.numeric(main_df$percent_post_cong_Faces_reported_friendly), na.rm = TRUE)))){
      main_df$percent_post_cong_Faces_reported_friendly[zesht4] <- as.numeric(main_df$percent_post_cong_Faces_reported_friendly[zesht4])
    } else {
      main_df$percent_post_cong_Faces_reported_friendly[zesht4] <- NA
    }

    if (as.numeric(main_df$percent_cong_Faces_reported_friendly[zesht4]) >= (mean(as.numeric(main_df$percent_cong_Faces_reported_friendly), na.rm = TRUE) - (3*sd(as.numeric(main_df$percent_cong_Faces_reported_friendly), na.rm = TRUE))) && as.numeric(main_df$percent_cong_Faces_reported_friendly[zesht4]) <= (mean(as.numeric(main_df$percent_cong_Faces_reported_friendly), na.rm = TRUE) + (3*sd(as.numeric(main_df$percent_cong_Faces_reported_friendly), na.rm = TRUE)))){
      main_df$percent_cong_Faces_reported_friendly[zesht4] <- as.numeric(main_df$percent_cong_Faces_reported_friendly[zesht4])
    } else {
      main_df$percent_cong_Faces_reported_friendly[zesht4] <- NA
    }

    if (as.numeric(main_df$percent_incong_Faces_reported_friendly[zesht4]) >= (mean(as.numeric(main_df$percent_incong_Faces_reported_friendly), na.rm = TRUE) - (3*sd(as.numeric(main_df$percent_incong_Faces_reported_friendly), na.rm = TRUE))) && as.numeric(main_df$percent_incong_Faces_reported_friendly[zesht4]) <= (mean(as.numeric(main_df$percent_incong_Faces_reported_friendly), na.rm = TRUE) + (3*sd(as.numeric(main_df$percent_incong_Faces_reported_friendly), na.rm = TRUE)))){
      main_df$percent_incong_Faces_reported_friendly[zesht4] <- as.numeric(main_df$percent_incong_Faces_reported_friendly[zesht4])
    } else {
      main_df$percent_incong_Faces_reported_friendly[zesht4] <- NA
    }

    if (as.numeric(main_df$cong_hitRate[zesht4]) >= (mean(as.numeric(main_df$cong_hitRate), na.rm = TRUE) - (3*sd(as.numeric(main_df$cong_hitRate), na.rm = TRUE))) && as.numeric(main_df$cong_hitRate[zesht4]) <= (mean(as.numeric(main_df$cong_hitRate), na.rm = TRUE) + (3*sd(as.numeric(main_df$cong_hitRate), na.rm = TRUE)))){
      main_df$cong_hitRate[zesht4] <- as.numeric(main_df$cong_hitRate[zesht4])
    } else {
      main_df$cong_hitRate[zesht4] <- NA
    }

    if (as.numeric(main_df$post_incong_hitRate[zesht4]) >= (mean(as.numeric(main_df$post_incong_hitRate), na.rm = TRUE) - (3*sd(as.numeric(main_df$post_incong_hitRate), na.rm = TRUE))) && as.numeric(main_df$post_incong_hitRate[zesht4]) <= (mean(as.numeric(main_df$post_incong_hitRate), na.rm = TRUE) + (3*sd(as.numeric(main_df$post_incong_hitRate), na.rm = TRUE)))){
      main_df$post_incong_hitRate[zesht4] <- as.numeric(main_df$post_incong_hitRate[zesht4])
    } else {
      main_df$post_incong_hitRate[zesht4] <- NA
    }

    if (as.numeric(main_df$post_cong_hitRate[zesht4]) >= (mean(as.numeric(main_df$post_cong_hitRate), na.rm = TRUE) - (3*sd(as.numeric(main_df$post_cong_hitRate), na.rm = TRUE))) && as.numeric(main_df$post_cong_hitRate[zesht4]) <= (mean(as.numeric(main_df$post_cong_hitRate), na.rm = TRUE) + (3*sd(as.numeric(main_df$post_cong_hitRate), na.rm = TRUE)))){
      main_df$post_cong_hitRate[zesht4] <- as.numeric(main_df$post_cong_hitRate[zesht4])
    } else {
      main_df$post_cong_hitRate[zesht4] <- NA
    }

    if (as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1[zesht4]) >= (mean(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), na.rm = TRUE) - (3*sd(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), na.rm = TRUE))) && as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1[zesht4]) <= (mean(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), na.rm = TRUE) + (3*sd(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), na.rm = TRUE)))){
      main_df$scaared_b_scrdSoc_s1_r1_e1[zesht4] <- as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1[zesht4])
    } else {
      main_df$scaared_b_scrdSoc_s1_r1_e1[zesht4] <- NA
    }

    if (as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1[zesht4]) >= (mean(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), na.rm = TRUE) - (3*sd(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), na.rm = TRUE))) && as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1[zesht4]) <= (mean(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), na.rm = TRUE) + (3*sd(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), na.rm = TRUE)))){
      main_df$scaared_b_scrdTotal_s1_r1_e1[zesht4] <- as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1[zesht4])
    } else {
      main_df$scaared_b_scrdTotal_s1_r1_e1[zesht4] <- NA
    }

    if (as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1[zesht4]) >= (mean(as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1), na.rm = TRUE) - (3*sd(as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1), na.rm = TRUE))) && as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1[zesht4]) <= (mean(as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1), na.rm = TRUE) + (3*sd(as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1), na.rm = TRUE)))){
      main_df$scaared_b_scrdGA_s1_r1_e1[zesht4] <- as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1[zesht4])
    } else {
      main_df$scaared_b_scrdGA_s1_r1_e1[zesht4] <- NA
    }
    if (as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1[zesht4]) >= (mean(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), na.rm = TRUE) - (3*sd(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), na.rm = TRUE))) && as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1[zesht4]) <= (mean(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), na.rm = TRUE) + (3*sd(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), na.rm = TRUE)))){
      main_df$bfne_b_scrdTotal_s1_r1_e1[zesht4] <- as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1[zesht4])
    } else {
      main_df$bfne_b_scrdTotal_s1_r1_e1[zesht4] <- NA
    }
    if (as.numeric(main_df$hitRate_incong_minus_cong[zesht4]) >= (mean(as.numeric(main_df$hitRate_incong_minus_cong), na.rm = TRUE) - (3*sd(as.numeric(main_df$hitRate_incong_minus_cong), na.rm = TRUE))) && as.numeric(main_df$hitRate_incong_minus_cong[zesht4]) <= (mean(as.numeric(main_df$hitRate_incong_minus_cong), na.rm = TRUE) + (3*sd(as.numeric(main_df$hitRate_incong_minus_cong), na.rm = TRUE)))){
      main_df$hitRate_incong_minus_cong[zesht4] <- as.numeric(main_df$hitRate_incong_minus_cong[zesht4])
    } else {
      main_df$hitRate_incong_minus_cong[zesht4] <- NA
    }

    if (as.numeric(main_df$hitRate_post_incong_minus_cong[zesht4]) >= (mean(as.numeric(main_df$hitRate_post_incong_minus_cong), na.rm = TRUE) - (3*sd(as.numeric(main_df$hitRate_post_incong_minus_cong), na.rm = TRUE))) && as.numeric(main_df$hitRate_post_incong_minus_cong[zesht4]) <= (mean(as.numeric(main_df$hitRate_post_incong_minus_cong), na.rm = TRUE) + (3*sd(as.numeric(main_df$hitRate_post_incong_minus_cong), na.rm = TRUE)))){
      main_df$hitRate_post_incong_minus_cong[zesht4] <- as.numeric(main_df$hitRate_post_incong_minus_cong[zesht4])
    } else {
      main_df$hitRate_post_incong_minus_cong[zesht4] <- NA
    }
  }
}

# Replacing NAs for pre/post error/correct hitRAtes, d's when they are less than 8 (i.e, cutoff for the number of incongruent errors)!
for (j in 1:nrow(main_df)){
  if (is.na(main_df$num_post_cong_faces[j]) || main_df$num_post_cong_faces[j] < incong_error_cutoff ){
    main_df$post_cong_hitRate[j] <- NA
    main_df$post_d_prime_cong[j] <- NA
  }
  if (is.na(main_df$num_pre_cong_faces[j]) || main_df$num_pre_cong_faces[j] < incong_error_cutoff ){
    main_df$pre_cong_hitRate[j] <- NA
    main_df$pre_d_prime_cong[j] <- NA
  }
  if (is.na(main_df$num_pre_incong_faces[j]) || main_df$num_pre_incong_faces[j] < incong_error_cutoff ){
    main_df$pre_incong_hitRate[j] <- NA
    main_df$pre_d_prime_incong[j] <- NA
  }
  if (is.na(main_df$num_post_incong_faces[j]) || main_df$num_post_incong_faces[j] < incong_error_cutoff ){
    main_df$post_incong_hitRate[j] <- NA
    main_df$post_d_prime_incong[j] <- NA
  }
}


####################
# Save the dataset
#write the extracted summary scores to disk
write.csv(main_df, paste(output_path, proc_fileName, sep = "/", collapse = NULL), row.names=FALSE)
##################
# Load the dataset
main_df <- read.csv(file = paste(output_path, proc_fileName, sep = "/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))



# Let's create the long format data based off of data we have in main_df!
long_df <- setNames(data.frame(matrix(ncol = 15, nrow = 0)), c("id", "congruent", "position", "hitRate", "dPrime", "friendliness_percentage", "condition_trial_num", "congAcc", "incongAcc", "congCorr_logMeanRT", "incongCorr_logMeanRT", "flankEff_meanACC", "flankEff_logMeanRT", "incongErr_logMeanRT", "congErr_logMeanRT")) # will store data of all participants
participant_long_df <- setNames(data.frame(matrix(ncol = 15, nrow = 6)), c("id", "congruent", "position", "hitRate", "dPrime", "friendliness_percentage", "condition_trial_num", "congAcc", "incongAcc", "congCorr_logMeanRT", "incongCorr_logMeanRT", "flankEff_meanACC", "flankEff_logMeanRT", "incongErr_logMeanRT", "congErr_logMeanRT"))
for(rrr in 1:nrow(main_df)){
  #pre_incong
  zzz <- 1 # this is a row counter
  participant_long_df$id[zzz] <- main_df$participant_id[rrr]
  participant_long_df$congruent[zzz] <- 0
  participant_long_df$position[zzz] <- -1
  participant_long_df$hitRate[zzz] <- main_df$pre_incong_hitRate[rrr]
  participant_long_df$dPrime[zzz] <- main_df$pre_d_prime_incong[rrr]
  participant_long_df$condition_trial_num[zzz] <- main_df$pre_incong_trials_in_surp[rrr]
  participant_long_df$friendliness_percentage[zzz] <- main_df$percent_pre_incong_Faces_reported_friendly[rrr]
  participant_long_df$congAcc[zzz] <- main_df$congAcc[rrr]
  participant_long_df$incongAcc[zzz] <- main_df$incongAcc[rrr]
  participant_long_df$congCorr_logMeanRT[zzz] <- main_df$congCorr_logMeanRT[rrr]
  participant_long_df$incongCorr_logMeanRT[zzz] <- main_df$incongCorr_logMeanRT[rrr]
  participant_long_df$flankEff_meanACC[zzz] <- main_df$flankEff_meanACC[rrr]
  participant_long_df$flankEff_logMeanRT[zzz] <- main_df$flankEff_logMeanRT[rrr]
  participant_long_df$incongErr_logMeanRT[zzz] <- main_df$incongErr_logMeanRT[rrr]
  participant_long_df$congErr_logMeanRT[zzz] <- main_df$congErr_logMeanRT[rrr]
  #incong
  zzz <- zzz + 1
  participant_long_df$id[zzz] <- main_df$participant_id[rrr]
  participant_long_df$congruent[zzz] <- 0
  participant_long_df$position[zzz] <- 0
  participant_long_df$hitRate[zzz] <- main_df$incong_hitRate[rrr]
  participant_long_df$dPrime[zzz] <- main_df$d_prime_incong[rrr]
  participant_long_df$condition_trial_num[zzz] <- main_df$incong_trials_in_surp[rrr]
  participant_long_df$friendliness_percentage[zzz] <- main_df$percent_incong_Faces_reported_friendly[rrr]
  participant_long_df$congAcc[zzz] <- main_df$congAcc[rrr]
  participant_long_df$incongAcc[zzz] <- main_df$incongAcc[rrr]
  participant_long_df$congCorr_logMeanRT[zzz] <- main_df$congCorr_logMeanRT[rrr]
  participant_long_df$incongCorr_logMeanRT[zzz] <- main_df$incongCorr_logMeanRT[rrr]
  participant_long_df$flankEff_meanACC[zzz] <- main_df$flankEff_meanACC[rrr]
  participant_long_df$flankEff_logMeanRT[zzz] <- main_df$flankEff_logMeanRT[rrr]
  participant_long_df$incongErr_logMeanRT[zzz] <- main_df$incongErr_logMeanRT[rrr]
  participant_long_df$congErr_logMeanRT[zzz] <- main_df$congErr_logMeanRT[rrr]
  #post_incong
  zzz <- zzz + 1
  participant_long_df$id[zzz] <- main_df$participant_id[rrr]
  participant_long_df$congruent[zzz] <- 0
  participant_long_df$position[zzz] <- 1
  participant_long_df$hitRate[zzz] <- main_df$post_incong_hitRate[rrr]
  participant_long_df$dPrime[zzz] <- main_df$post_d_prime_incong[rrr]
  participant_long_df$condition_trial_num[zzz] <- main_df$post_incong_trials_in_surp[rrr]
  participant_long_df$friendliness_percentage[zzz] <- main_df$percent_post_incong_Faces_reported_friendly[rrr]
  participant_long_df$congAcc[zzz] <- main_df$congAcc[rrr]
  participant_long_df$incongAcc[zzz] <- main_df$incongAcc[rrr]
  participant_long_df$congCorr_logMeanRT[zzz] <- main_df$congCorr_logMeanRT[rrr]
  participant_long_df$incongCorr_logMeanRT[zzz] <- main_df$incongCorr_logMeanRT[rrr]
  participant_long_df$flankEff_meanACC[zzz] <- main_df$flankEff_meanACC[rrr]
  participant_long_df$flankEff_logMeanRT[zzz] <- main_df$flankEff_logMeanRT[rrr]
  participant_long_df$incongErr_logMeanRT[zzz] <- main_df$incongErr_logMeanRT[rrr]
  participant_long_df$congErr_logMeanRT[zzz] <- main_df$congErr_logMeanRT[rrr]
  #pre_cong
  zzz <- zzz + 1
  participant_long_df$id[zzz] <- main_df$participant_id[rrr]
  participant_long_df$congruent[zzz] <- 1
  participant_long_df$position[zzz] <- -1
  participant_long_df$hitRate[zzz] <- main_df$pre_cong_hitRate[rrr]
  participant_long_df$dPrime[zzz] <- main_df$pre_d_prime_cong[rrr]
  participant_long_df$condition_trial_num[zzz] <- main_df$pre_cong_trials_in_surp[rrr]
  participant_long_df$friendliness_percentage[zzz] <- main_df$percent_pre_cong_Faces_reported_friendly[rrr]
  participant_long_df$congAcc[zzz] <- main_df$congAcc[rrr]
  participant_long_df$incongAcc[zzz] <- main_df$incongAcc[rrr]
  participant_long_df$congCorr_logMeanRT[zzz] <- main_df$congCorr_logMeanRT[rrr]
  participant_long_df$incongCorr_logMeanRT[zzz] <- main_df$incongCorr_logMeanRT[rrr]
  participant_long_df$flankEff_meanACC[zzz] <- main_df$flankEff_meanACC[rrr]
  participant_long_df$flankEff_logMeanRT[zzz] <- main_df$flankEff_logMeanRT[rrr]
  participant_long_df$incongErr_logMeanRT[zzz] <- main_df$incongErr_logMeanRT[rrr]
  participant_long_df$congErr_logMeanRT[zzz] <- main_df$congErr_logMeanRT[rrr]
  #cong
  zzz <- zzz + 1
  participant_long_df$id[zzz] <- main_df$participant_id[rrr]
  participant_long_df$congruent[zzz] <- 1
  participant_long_df$position[zzz] <- 0
  participant_long_df$hitRate[zzz] <- main_df$cong_hitRate[rrr]
  participant_long_df$dPrime[zzz] <- main_df$d_prime_cong[rrr]
  participant_long_df$condition_trial_num[zzz] <- main_df$cong_trials_in_surp[rrr]
  participant_long_df$friendliness_percentage[zzz] <- main_df$percent_cong_Faces_reported_friendly[rrr]
  participant_long_df$congAcc[zzz] <- main_df$congAcc[rrr]
  participant_long_df$incongAcc[zzz] <- main_df$incongAcc[rrr]
  participant_long_df$congCorr_logMeanRT[zzz] <- main_df$congCorr_logMeanRT[rrr]
  participant_long_df$incongCorr_logMeanRT[zzz] <- main_df$incongCorr_logMeanRT[rrr]
  participant_long_df$flankEff_meanACC[zzz] <- main_df$flankEff_meanACC[rrr]
  participant_long_df$flankEff_logMeanRT[zzz] <- main_df$flankEff_logMeanRT[rrr]
  participant_long_df$incongErr_logMeanRT[zzz] <- main_df$incongErr_logMeanRT[rrr]
  participant_long_df$congErr_logMeanRT[zzz] <- main_df$congErr_logMeanRT[rrr]
  #post_cong
  zzz <- zzz + 1
  participant_long_df$id[zzz] <- main_df$participant_id[rrr]
  participant_long_df$congruent[zzz] <- 1
  participant_long_df$position[zzz] <- 1
  participant_long_df$hitRate[zzz] <- main_df$post_cong_hitRate[rrr]
  participant_long_df$dPrime[zzz] <- main_df$post_d_prime_cong[rrr]
  participant_long_df$condition_trial_num[zzz] <- main_df$post_cong_trials_in_surp[rrr]
  participant_long_df$friendliness_percentage[zzz] <- main_df$percent_post_cong_Faces_reported_friendly[rrr]
  participant_long_df$congAcc[zzz] <- main_df$congAcc[rrr]
  participant_long_df$incongAcc[zzz] <- main_df$incongAcc[rrr]
  participant_long_df$congCorr_logMeanRT[zzz] <- main_df$congCorr_logMeanRT[rrr]
  participant_long_df$incongCorr_logMeanRT[zzz] <- main_df$incongCorr_logMeanRT[rrr]
  participant_long_df$flankEff_meanACC[zzz] <- main_df$flankEff_meanACC[rrr]
  participant_long_df$flankEff_logMeanRT[zzz] <- main_df$flankEff_logMeanRT[rrr]
  participant_long_df$incongErr_logMeanRT[zzz] <- main_df$incongErr_logMeanRT[rrr]
  participant_long_df$congErr_logMeanRT[zzz] <- main_df$congErr_logMeanRT[rrr]
  long_df <- rbind(long_df, participant_long_df)
}

# adding new columns to "long_df" dataframe!
for (rr in 1:nrow(long_df)){
  temp_id <- long_df$id[rr]
  tempDat <- filter(main_df, participant_id == temp_id)
  long_df$scaared_b_scrdSoc_s1_r1_e1[rr] <- tempDat$scaared_b_scrdSoc_s1_r1_e1
}
for (rr in 1:nrow(long_df)){
  temp_id <- long_df$id[rr]
  tempDat <- filter(main_df, participant_id == temp_id)
  long_df$scaared_b_scrdTotal_s1_r1_e1[rr] <- tempDat$scaared_b_scrdTotal_s1_r1_e1
}
for (rr in 1:nrow(long_df)){
  temp_id <- long_df$id[rr]
  tempDat <- filter(main_df, participant_id == temp_id)
  long_df$bfne_b_scrdTotal_s1_r1_e1[rr] <- tempDat$bfne_b_scrdTotal_s1_r1_e1
}

write.csv(long_df, paste(output_path, long_fileName, sep = "/", collapse = NULL), row.names=FALSE)


