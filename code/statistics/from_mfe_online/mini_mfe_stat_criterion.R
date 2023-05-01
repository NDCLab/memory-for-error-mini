# This script allows to run statistical analysis on Pavlovia output of the mini memory for error online project.
# Author: Kianoosh Hosseini at NDCLab @FIU (May-September 2022; https://Kianoosh.info; https://NDClab.com)
# Last Update: 2022-12-05 (YYYY-MM-DD)
#install.packages("psycho")
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(psycho)

#Working directory should be the Psychopy experiment directory.
proje_wd <- "/Users/kihossei/Documents/GitHub/mfe-online-dataset/materials/PsychopyTask/mini_mfe_online"
setwd(proje_wd)

today <- Sys.Date()
today <- format(today, "%Y%m%d")

# Defining the input and output folders.
input_path <- paste(proje_wd, "data", "data_for_above_8errors", sep ="/", collapse = NULL) # input data directory
output_path <- paste(proje_wd, "stat_output", sep ="/", collapse = NULL) # output directory
outlier_data_path <- paste(proje_wd, "outlier_data", sep ="/", collapse = NULL) #outlier_data directory
proc_fileName <- paste(today, "_mfe_onlineProj.csv", sep ="", collapse = NULL) # output filename

# Creating the main empty dataframe that will be filled with the data from the loop below:
mainDat <- setNames(data.frame(matrix(ncol = 25, nrow = 0)), c("id", "congAcc", "incongAcc", "congCorr_meanRT", "incongCorr_meanRT", "congCorr_logMeanRT", "incongCorr_logMeanRT",
                                                               "flankEff_meanACC", "flankEff_meanRT", "flankEff_logMeanRT",
                                                               "reported_errors", "committed_errors", "memoryBias_score",
                                                               "num_incong_errorFaces_reported_old", "num_incong_errorFaces_reported_new",
                                                               "num_incong_corrFaces_reported_old", "num_incong_corrFaces_reported_new",
                                                               "num_post_incong_errorFaces_reported_old", "num_post_incong_errorFaces_reported_new", "num_post_incong_correctFaces_reported_old", "num_post_incong_correctFaces_reported_new",
                                                               "num_pre_incong_errorFaces_reported_old", "num_pre_incong_errorFaces_reported_new", "num_pre_incong_correctFaces_reported_old", "num_pre_incong_correctFaces_reported_new"))

percent_mainDat <- setNames(data.frame(matrix(ncol = 63, nrow = 0)), c("id", "congAcc", "incongAcc", "congCorr_meanRT", "incongCorr_meanRT", "congCorr_logMeanRT", "incongCorr_logMeanRT",
                                                                       "flankEff_meanACC", "flankEff_meanRT", "flankEff_logMeanRT",
                                                                       "reported_errors", "committed_errors", "memoryBias_score",
                                                                       "percent_incong_errorFaces_reported_old", "percent_incong_errorFaces_reported_new",
                                                                       "percent_incong_corrFaces_reported_old", "percent_incong_corrFaces_reported_new",
                                                                       "percent_post_incong_errorFaces_reported_old", "percent_post_incong_errorFaces_reported_new", "percent_post_incong_correctFaces_reported_old", "percent_post_incong_correctFaces_reported_new",
                                                                       "percent_pre_incong_errorFaces_reported_old", "percent_pre_incong_errorFaces_reported_new", "percent_pre_incong_correctFaces_reported_old", "percent_pre_incong_correctFaces_reported_new",
                                                                       "post_error_rt_mean", "post_error_rt_sd", "post_correct_rt_mean", "post_correct_rt_sd",
                                                                       "post_error_slowing_score", "post_error_accuracy_mean", "post_error_accuracy_sd",
                                                                       "post_correct_accuracy_mean", "post_correct_accuracy_sd", "post_error_accuracy_score", "hit_num", "miss_num", "corr_rej_num", "false_alrams_num",
                                                                       "incong_error_hit_num", "incong_error_miss_num", "incong_correct_hit_num", "incong_correct_miss_num",
                                                                       "incong_post_correct_hit_num", "incong_post_correct_miss_num",
                                                                       "incong_pre_correct_hit_num", "incong_pre_correct_miss_num", "incong_pre_error_hit_num", "incong_pre_error_miss_num",
                                                                       "incong_post_error_hit_num", "incong_post_error_miss_num", "post_error_hitRate", "pre_error_hitRate",
                                                                       "pre_correct_hitRate", "post_correct_hitRate" ,"correct_hitRate" ,"error_hitRate", "post_error_criterion",
                                                                       "pre_error_criterion", "pre_correct_criterion", "post_correct_criterion", "correct_criterion", "error_criterion"))

outlier_mainDat <- setNames(data.frame(matrix(ncol = 13, nrow = 0)), c("id", "congAcc", "incongAcc", "congCorr_meanRT", "incongCorr_meanRT", "congCorr_logMeanRT", "incongCorr_logMeanRT",
                                                                       "flankEff_meanACC", "flankEff_meanRT", "flankEff_logMeanRT",
                                                                       "reported_errors", "committed_errors", "memoryBias_score"))

####### Main part and d'
# updating datafiles_list
datafiles_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(input_path, pattern = ".csv") # listing only csv files
for (lisar1 in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[lisar1], "mini_mfe_online_s1_r1_e1", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[lisar1]
    datafiles_list <- c(datafiles_list, temp_list)
  }
}
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
                                   "stimNum",
                                   "accuracy",
                                   "task1_stim_keyResp.keys",
                                   "errorNum_text_box.text", # stores the number of reported errors by subjects
                                   "surprise_key_resp.keys",
                                   "surpriseFaces",
                                   "straightFace",
                                   "task1_stim_keyResp.rt", #  this stores reaction time for each trial
                                   "task_trial_loop.thisTrialN",
                                   "prac_trial_loop.ran", "trials.thisN")]

  #remove practice trials and any rows that do not reflect experiment data
  remove_first_row <- psychopyDatTrim[-1:-7,] # removes first seven rows
  remove_prac_trials <- subset(remove_first_row, !complete.cases(remove_first_row$prac_trial_loop.ran)) # removes practice trials
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

  # number of errors made in the main task
  committed_errors <- 0
  for (khata in 1:nrow(keep_rows_with_acc_vals)){
    if (keep_rows_with_acc_vals$accuracy[khata] == 0){
      committed_errors <- committed_errors +1
    }
  }
  reported_errors <- subset(remove_prac_trials, complete.cases(remove_prac_trials$errorNum_text_box.text))
  reported_errors <- reported_errors$errorNum_text_box.text # number of reported errors by participants
  reported_errors <- regmatches(reported_errors, gregexpr("[[:digit:]]+", reported_errors))
  reported_errors <- as.numeric(unlist(reported_errors))
  if (length(as.numeric(reported_errors)) == 0){ # in case a participant does not answer the question or have put letters instead of numbers, this code will prevent from future errors.
    reported_errors <- NA
    memoryBias_score <- NA
  } else {
    memoryBias_score <- ((reported_errors - as.numeric(committed_errors))/ abs(reported_errors)) # percent bias score calculation
  }
  surpDat <- subset(remove_prac_trials, complete.cases(remove_prac_trials$new)) #contains all the data we need from the surprise task
  each_surp_block_new_key <- subset(remove_prac_trials, complete.cases(remove_prac_trials$newKey))

  surpDat$which_key_is_new <- rep(NA, nrow(surpDat))
  for (javan in 1:nrow(surpDat)){
    if (javan < 73){
      surpDat$which_key_is_new[javan] <- each_surp_block_new_key$newKey[1]
    } else if (javan > (72 * 1) & javan < (73 + 72)){
      surpDat$which_key_is_new[javan] <- each_surp_block_new_key$newKey[2]
    } else if (javan > (72 * 2) & javan < (73 + (72 * 2))){
      surpDat$which_key_is_new[javan] <- each_surp_block_new_key$newKey[3]
    } else if (javan > (72 * 3) & javan < (73 + (72 * 3))){
      surpDat$which_key_is_new[javan] <- each_surp_block_new_key$newKey[4]
    } else if (javan > (72 * 4) & javan < (73 + (72 * 4))){
      surpDat$which_key_is_new[javan] <- each_surp_block_new_key$newKey[5]
    } else if (javan > (72 * 5) & javan < (73 + (72 * 5))){
      surpDat$which_key_is_new[javan] <- each_surp_block_new_key$newKey[6]
    } else if (javan > (72 * 6) & javan < (73 + (72 * 6))){
      surpDat$which_key_is_new[javan] <- each_surp_block_new_key$newKey[7]
    } else if (javan > (72 * 7) & javan < (73 + (72 * 7))){
      surpDat$which_key_is_new[javan] <- each_surp_block_new_key$newKey[8]
    }
  }
    ######## SECTION 2: Surprise Task
    # Let's keep only the surprise trials that have faces from error trials in the main task. Then, we will be able to easily use that smaller dataframe to calculate the number of OLD faces among error trials.
    # Loop over the faces from error trials.
    # to prevent errors when there is no incong_errors:
    if (nrow(incong_errorDat) != 0){
      num_incong_errorFaces_reported_old <- 0 # this is the number of error faces that they report as OLD and will be updated in the loop below:
      for (Jafa in 1:nrow(incong_errorDat)){
        temp_face <- incong_errorDat$straightFace[Jafa]
        temp_for_surp <- filter(surpDat, surpriseFaces == temp_face)
        identified_old_correctly <- ifelse (temp_for_surp$which_key_is_new != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
        if (identified_old_correctly == 1){
          num_incong_errorFaces_reported_old <- num_incong_errorFaces_reported_old + 1 # The number of error faces that they report as OLD
        }
      }
    } else {
      num_incong_errorFaces_reported_old <- 0
    }

    num_incong_errorFaces_reported_new <- nrow(incong_errorDat) - num_incong_errorFaces_reported_old # stores the # of error faces that the participant incorrectly identifies as new.
    percent_incong_errorFaces_reported_old <- (num_incong_errorFaces_reported_old/(num_incong_errorFaces_reported_old + num_incong_errorFaces_reported_new)) * 100
    if (is.nan(percent_incong_errorFaces_reported_old)){
      percent_incong_errorFaces_reported_old <- "no error"
      percent_incong_errorFaces_reported_new <- "no error"
    } else {
      percent_incong_errorFaces_reported_new <- 100 - percent_incong_errorFaces_reported_old
    }

    if (nrow(incong_corrDat) != 0){
      num_incong_corrFaces_reported_old <- 0
      for (Jafa2 in 1:nrow(incong_corrDat)){
        temp_face <- incong_corrDat$straightFace[Jafa2]
        temp_for_surp <- filter(surpDat, surpriseFaces == temp_face) # find the error face in the surpDat
        identified_old_correctly <- ifelse (temp_for_surp$which_key_is_new != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
        if (identified_old_correctly == 1){
          num_incong_corrFaces_reported_old <- num_incong_corrFaces_reported_old + 1 # The number of correct faces that they report as OLD
        }
      }
    } else {
      num_incong_corrFaces_reported_old <- 0
    }
    num_incong_corrFaces_reported_new <- nrow(incong_corrDat) - num_incong_corrFaces_reported_old # The number of correct faces that they report as new
    percent_incong_corrFaces_reported_old <- (num_incong_corrFaces_reported_old/(num_incong_corrFaces_reported_old + num_incong_corrFaces_reported_new)) * 100
    percent_incong_corrFaces_reported_new <- 100 - percent_incong_corrFaces_reported_old
    ######################################
    #Section 4: Do they remember post-error faces?
    # I should loop over the data frame that has only the main task with accuracy vals, i.e., keep_rows_with_acc_vals
    # The goal is to create a data frame of post-error faces. We will just use incongruent pre/post data.
    post_error_faces <- c() # will be filled in the loop below.
    for (zaman in 1:nrow(keep_rows_with_acc_vals)){
      next_idx <- zaman + 1
      if (keep_rows_with_acc_vals$accuracy[zaman] ==0){
        if (keep_rows_with_acc_vals$congruent[zaman] == 0){
          if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[zaman] == 31){ # Trial #31 is the last trial in a block and
            # the face after that is the first trial of the next block. So, that face cannot be among post_error_faces.
            post_error_faces <- post_error_faces
          } else {
            if (keep_rows_with_acc_vals$accuracy[next_idx] ==1){ # to have only correct trials in the post_error
              post_error_faces <- rbind(post_error_faces, keep_rows_with_acc_vals[next_idx,])
            }
          }
        }
      }
    }
    if (!is.null(post_error_faces)){
      post_error_faces <- subset(post_error_faces, complete.cases(post_error_faces$id))
      # Let's find out how many of the post-error faces were correctly identified as OLD!
      num_post_incong_errorFaces_reported_old <- 0 # this is the number of error faces that they report as OLD and will be updated in the loop below:
      for (zaman2 in 1:nrow(post_error_faces)){
        temp_face <- post_error_faces$straightFace[zaman2]
        temp_for_surp <- filter(surpDat, surpriseFaces == temp_face)
        identified_old_correctly <- ifelse (temp_for_surp$which_key_is_new != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
        if (identified_old_correctly == 1){
          num_post_incong_errorFaces_reported_old <- num_post_incong_errorFaces_reported_old + 1 # The number of post-error faces that they report as OLD  1111111111111111111
        }
      }
    } else {
      num_post_incong_errorFaces_reported_old <- 0
    }

    num_post_incong_errorFaces_reported_new <- nrow(post_error_faces) - num_post_incong_errorFaces_reported_old
    percent_post_incong_errorFaces_reported_old <- (num_post_incong_errorFaces_reported_old/(num_post_incong_errorFaces_reported_old + num_post_incong_errorFaces_reported_new)) * 100
    if (length(percent_post_incong_errorFaces_reported_old) ==0){
      percent_post_incong_errorFaces_reported_old <- "no error"
      percent_post_incong_errorFaces_reported_old <- "no error"
    } else {
      percent_post_incong_errorFaces_reported_new <- 100 - percent_post_incong_errorFaces_reported_old
    }

    # What about post-correct faces?
    post_correct_faces <- c()
    for (zaman in 1:nrow(keep_rows_with_acc_vals)){
      next_idx <- zaman + 1
      if (keep_rows_with_acc_vals$accuracy[zaman] ==1){
        if (keep_rows_with_acc_vals$congruent[zaman] == 0){
          if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[zaman] == 31){ #Checks if this is the last trial of the block.
            post_correct_faces <- post_correct_faces
          } else {
            if (keep_rows_with_acc_vals$accuracy[next_idx] ==1){ # to have only correct trials in the post_correct
              post_correct_faces <- rbind(post_correct_faces, keep_rows_with_acc_vals[next_idx,])
            }
          }
        }
      }
    }
    if (!is.null(post_correct_faces)){
      post_correct_faces <- subset(post_correct_faces, complete.cases(post_correct_faces$id))
      # Let's find out how many of the post-correct faces were correctly identified as OLD!
      num_post_incong_correctFaces_reported_old <- 0 # this is the number of correct faces that they report as OLD and will be updated in the loop below:
      for (zaman2 in 1:nrow(post_correct_faces)){
        temp_face <- post_correct_faces$straightFace[zaman2]
        temp_for_surp <- filter(surpDat, surpriseFaces == temp_face)
        identified_old_correctly <- ifelse (temp_for_surp$which_key_is_new != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
        if (identified_old_correctly == 1){
          num_post_incong_correctFaces_reported_old <- num_post_incong_correctFaces_reported_old + 1 # The number of post-correct faces that they report as OLD  1111111111111111111
        }
      }
    } else {
      num_post_incong_correctFaces_reported_old <- 0
    }

    num_post_incong_correctFaces_reported_new <- nrow(post_correct_faces) - num_post_incong_correctFaces_reported_old
    percent_post_incong_correctFaces_reported_old <- (num_post_incong_correctFaces_reported_old/(num_post_incong_correctFaces_reported_old + num_post_incong_correctFaces_reported_new)) * 100
    percent_post_incong_correctFaces_reported_new <- 100 - percent_post_incong_correctFaces_reported_old
    ##############
    #Section 5: Do they remember pre-error faces?
    pre_error_faces <- c() # will be filled in the loop below.
    for (zaman in 1:nrow(keep_rows_with_acc_vals)){
      prior_idx <- zaman - 1
      if (keep_rows_with_acc_vals$accuracy[zaman] ==0){
        if (keep_rows_with_acc_vals$congruent[zaman] ==0){
          if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[zaman] == 0){ #Checks if this is the first trial of the block.
            pre_error_faces <- pre_error_faces
          } else {
            if (keep_rows_with_acc_vals$accuracy[prior_idx] ==1){ # to have only correct trials in the pre_error
              pre_error_faces <- rbind(pre_error_faces, keep_rows_with_acc_vals[prior_idx,])
            }
          }
        }
      }
    }
    if (!is.null(pre_error_faces)){
      pre_error_faces <- subset(pre_error_faces, complete.cases(pre_error_faces$id))
      # Let's find out how many of the pre-error faces were correctly identified as OLD!
      num_pre_incong_errorFaces_reported_old <- 0 # this is the number of correct faces that they report as OLD and will be updated in the loop below:
      for (zaman2 in 1:nrow(pre_error_faces)){
        temp_face <- pre_error_faces$straightFace[zaman2]
        temp_for_surp <- filter(surpDat, surpriseFaces == temp_face)
        identified_old_correctly <- ifelse (temp_for_surp$which_key_is_new != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
        if (identified_old_correctly == 1){
          num_pre_incong_errorFaces_reported_old <- num_pre_incong_errorFaces_reported_old + 1 # The number of pre-error faces that they report as OLD  1111111111111111111
        }
      }
    } else {
      num_pre_incong_errorFaces_reported_old <- 0
    }

    num_pre_incong_errorFaces_reported_new <- nrow(pre_error_faces) - num_pre_incong_errorFaces_reported_old
    percent_pre_incong_errorFaces_reported_old <- (num_pre_incong_errorFaces_reported_old/(num_pre_incong_errorFaces_reported_old + num_pre_incong_errorFaces_reported_new)) * 100
    if (length(percent_pre_incong_errorFaces_reported_old) == 0){
      percent_pre_incong_errorFaces_reported_old <- "no error"
      percent_pre_incong_errorFaces_reported_old <- "no error"
    } else {
      percent_pre_incong_errorFaces_reported_new <- 100 - percent_pre_incong_errorFaces_reported_old
    }

    pre_correct_faces <- c()
    for (zaman in 1:nrow(keep_rows_with_acc_vals)){
      prior_idx <- zaman - 1
      if (keep_rows_with_acc_vals$accuracy[zaman] ==1){
        if (keep_rows_with_acc_vals$congruent[zaman] ==0){
          if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[zaman] == 0){ #Checks if this is the first trial of the block.
            pre_correct_faces <- pre_correct_faces
          } else {
            if (keep_rows_with_acc_vals$accuracy[prior_idx] ==1){ # to have only correct trials in the pre_correct
              pre_correct_faces <- rbind(pre_correct_faces, keep_rows_with_acc_vals[prior_idx,])
            }
          }
        }
      }
    }
    if (!is.null(pre_error_faces)){
      # What about pre-correct faces?
      pre_correct_faces <- subset(pre_correct_faces, complete.cases(pre_correct_faces$id))
      # Let's find out how many of the post-correct faces were correctly identified as OLD!
      num_pre_incong_correctFaces_reported_old <- 0 # this is the number of correct faces that they report as OLD and will be updated in the loop below:
      for (zaman2 in 1:nrow(pre_correct_faces)){
        temp_face <- pre_correct_faces$straightFace[zaman2]
        temp_for_surp <- filter(surpDat, surpriseFaces == temp_face)
        identified_old_correctly <- ifelse (temp_for_surp$which_key_is_new != temp_for_surp$surprise_key_resp.keys, 1, 0) #returns 1 when participant correctly identifies the face as OLD!
        if (identified_old_correctly == 1){
          num_pre_incong_correctFaces_reported_old <- num_pre_incong_correctFaces_reported_old + 1 # The number of pre-correct faces that they report as OLD  1111111111111111111
        }
      }
    } else {
      num_pre_incong_correctFaces_reported_old <- 0
    }
    num_pre_incong_correctFaces_reported_new <- nrow(pre_correct_faces) - num_pre_incong_correctFaces_reported_old
    percent_pre_incong_correctFaces_reported_old <- (num_pre_incong_correctFaces_reported_old/(num_pre_incong_correctFaces_reported_old + num_pre_incong_correctFaces_reported_new)) * 100
    percent_pre_incong_correctFaces_reported_new <- 100 - percent_pre_incong_correctFaces_reported_old

    ########################## SIGNAL DETECTION THEORY ########################################
    # We make use of SDT in the surprise memory task.
    # OLD is our target.
    # OLD faces are going to be divided into old_errorFaces and old_correctFaces
    # We first compute hit and false alarm for each individual.
    ###########################################################################################
    # Overall hits, misses, FAs, and Correct rejections regardless of errors and corrects.
    hit_num <- 0
    miss_num <- 0
    corr_rej_num <- 0 # Correct rejections number is the same for Errors, corrects as it is overall. It means they identify new faces as new!
    false_alrams_num <- 0
    for (donya in 1:nrow(surpDat)){
      temp_cut_from_surp <- surpDat$surpriseFaces[donya]
      temp_cut_from_flanker <- filter(keep_rows_with_acc_vals, straightFace == temp_cut_from_surp)
      if (nrow(temp_cut_from_flanker) == 1){ # The face is old
        if (surpDat$which_key_is_new[donya] == surpDat$surprise_key_resp.keys[donya]){
          miss_num <- miss_num + 1
        } else if (surpDat$which_key_is_new[donya] != surpDat$surprise_key_resp.keys[donya]){
          hit_num <- hit_num + 1
        }
      } else if (nrow(temp_cut_from_flanker) == 0){ # The face is new
        if (surpDat$which_key_is_new[donya] == surpDat$surprise_key_resp.keys[donya]){
          corr_rej_num <- corr_rej_num + 1
        } else if (surpDat$which_key_is_new[donya] != surpDat$surprise_key_resp.keys[donya]){
          false_alrams_num <- false_alrams_num + 1
        }
      }
    }
  # Corrections area
  #if (num_incong_errorFaces_reported_old == 0){
   # num_incong_errorFaces_reported_old <- 0.5
  #}
  #if (num_incong_errorFaces_reported_new == 0){
    #num_incong_errorFaces_reported_new <- 0.5
  #}
  #if (num_incong_corrFaces_reported_old == 0){
  #  num_incong_corrFaces_reported_old <- 0.5
  #}
  #if (num_incong_corrFaces_reported_new == 0){
   # num_incong_corrFaces_reported_new <- 0.5
 # }
  #if (num_post_incong_correctFaces_reported_old == 0){
  #  num_post_incong_correctFaces_reported_old <- 0.5
  #}
  #if (num_pre_incong_correctFaces_reported_old == 0){
   # num_pre_incong_correctFaces_reported_old <- 0.5
  #}
  #if (num_post_incong_correctFaces_reported_new == 0){
  #  num_post_incong_correctFaces_reported_new <- 0.5
  #}
  #if (num_pre_incong_correctFaces_reported_new == 0){
  #  num_pre_incong_correctFaces_reported_new <- 0.5
  #}
  #if (num_pre_incong_errorFaces_reported_old == 0){
   # num_pre_incong_errorFaces_reported_old <- 0.5
  #}
  #if (num_pre_incong_errorFaces_reported_new == 0){
   # num_pre_incong_errorFaces_reported_new <- 0.5
  #}
  #if (num_post_incong_errorFaces_reported_old == 0){
   # num_post_incong_errorFaces_reported_old <- 0.5
  #}
  #if (num_post_incong_errorFaces_reported_new == 0){
   # num_post_incong_errorFaces_reported_new <- 0.5
  #}
  false_alarm_rate <- (false_alrams_num + 0.5) / ((false_alrams_num + 0.5) + corr_rej_num + 1)
  ############ d' calculations for Error faces ###########################
  # False alaram numbers is the same for Error, and correct faces as it is the overall FA! I computed it above with "false_alrams_num" name.
  incong_error_hit_num <- num_incong_errorFaces_reported_old
  incong_error_miss_num <- num_incong_errorFaces_reported_new
  error_hitRate <- (incong_error_hit_num + 0.5) / ((incong_error_hit_num + 0.5) + incong_error_miss_num + 1) # hit rate
  error_criterion <- -(qnorm(error_hitRate) + qnorm(false_alarm_rate))/2 # criterion
  ########### End of d' calculations for Error faces #####################

  ############ d' calculations for Correct faces #########################
  # False alaram numbers is the same for Error, and correct faces as it is the overall FA! I computed it above with "false_alrams_num" name.
  incong_correct_hit_num <- num_incong_corrFaces_reported_old
  incong_correct_miss_num <- num_incong_corrFaces_reported_new
  correct_hitRate <- (incong_correct_hit_num + 0.5) / ((incong_correct_hit_num + 0.5) + incong_correct_miss_num + 1) # hit rate
  correct_criterion <- -(qnorm(correct_hitRate) + qnorm(false_alarm_rate))/2 # criterion
  ############ End of d' calculations for Correct faces ##################

  ############ d' calculations for post-Correct faces #########################
  # False alaram numbers is the same for Error, and correct faces as it is the overall FA! I computed it above with "false_alrams_num" name.
  incong_post_correct_hit_num <- num_post_incong_correctFaces_reported_old
  incong_post_correct_miss_num <- num_post_incong_correctFaces_reported_new
  post_correct_hitRate <- (incong_post_correct_hit_num + 0.5) / ((incong_post_correct_hit_num + 0.5) + incong_post_correct_miss_num + 1) # hit rate
  post_correct_criterion <- -(qnorm(post_correct_hitRate) + qnorm(false_alarm_rate))/2 # criterion
  ############ End of d' calculations for post-Correct faces ##################

  ############ d' calculations for pre-Correct faces #########################
  # False alaram numbers is the same for Error, and correct faces as it is the overall FA! I computed it above with "false_alrams_num" name.
  incong_pre_correct_hit_num <- num_pre_incong_correctFaces_reported_old
  incong_pre_correct_miss_num <- num_pre_incong_correctFaces_reported_new
  pre_correct_hitRate <- (incong_pre_correct_hit_num + 0.5) / ((incong_pre_correct_hit_num + 0.5) + incong_pre_correct_miss_num + 1) # hit rate
  pre_correct_criterion <- -(qnorm(pre_correct_hitRate) + qnorm(false_alarm_rate))/2 # criterion
  ############ End of d' calculations for pre-Correct faces ##################

  ############ d' calculations for pre-error faces #########################
  # False alaram numbers is the same for Error, and correct faces as it is the overall FA! I computed it above with "false_alrams_num" name.
  incong_pre_error_hit_num <- num_pre_incong_errorFaces_reported_old
  incong_pre_error_miss_num <- num_pre_incong_errorFaces_reported_new
  pre_error_hitRate <- (incong_pre_error_hit_num + 0.5) / ((incong_pre_error_hit_num + 0.5) + incong_pre_error_miss_num + 1) # hit rate
  pre_error_criterion <- -(qnorm(pre_error_hitRate) + qnorm(false_alarm_rate))/2 # criterion
  ############ End of d' calculations for pre-error faces ##################

  ############ d' calculations for post-error faces #########################
  # False alaram numbers is the same for Error, and correct faces as it is the overall FA! I computed it above with "false_alrams_num" name.
  incong_post_error_hit_num <- num_post_incong_errorFaces_reported_old
  incong_post_error_miss_num <- num_post_incong_errorFaces_reported_new
  post_error_hitRate <- (incong_post_error_hit_num + 0.5) / ((incong_post_error_hit_num + 0.5) + incong_post_error_miss_num + 1) # hit rate
  post_error_criterion <- -(qnorm(post_error_hitRate) + qnorm(false_alarm_rate))/2 # criterion
  ############ End of d' calculations for post-error faces ##################

  ################################################################################

    ####### The zone below is for computing post-error slowing (PES) value for each subject! ###########
    # Let's create a dataframe that has only post-error rows. After having this data frame, we can compute the mean
    # post-error RT for each subject.
    post_error_rt_vals <- c() # will be filled in the loop below.
    for (ayshan in 1:nrow(keep_rows_with_acc_vals)){
      next_idx <- ayshan + 1
      if (keep_rows_with_acc_vals$accuracy[ayshan] ==0){
        if (keep_rows_with_acc_vals$congruent[ayshan] == 0){ # just incongruent errors
          if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[ayshan] == 31){ # Trial #31 is the last trial in a block and
            # the trial after that is the first trial of the next block.
            post_error_rt_vals <- post_error_rt_vals
          } else {
            if (keep_rows_with_acc_vals$accuracy[next_idx] ==1){ # to have only correct trials in the post_error
              post_error_rt_vals <- rbind(post_error_rt_vals, keep_rows_with_acc_vals[next_idx,])
            }
          }
        }
      }
    }
    # As the task1_stim_keyResp.rt variable does have values in a bracket. I have to modify it and then change to numbers.
    for (ayshan2 in 1:nrow(post_error_rt_vals)){
      post_error_rt_vals$task1_stim_keyResp.rt[ayshan2] <- gsub("[", "", post_error_rt_vals$task1_stim_keyResp.rt[ayshan2], fixed = TRUE) # removes the first bracket
      post_error_rt_vals$task1_stim_keyResp.rt[ayshan2] <- gsub("]", "", post_error_rt_vals$task1_stim_keyResp.rt[ayshan2], fixed = TRUE) # removes the last bracket
      post_error_rt_vals$task1_stim_keyResp.rt[ayshan2] <- gsub(",.*", "", post_error_rt_vals$task1_stim_keyResp.rt[ayshan2])
    }
    # Compute the mean and SD of the post-error RT.
    post_error_rt_mean <- mean(as.numeric(post_error_rt_vals$task1_stim_keyResp.rt), na.rm = TRUE)
    post_error_rt_sd <- sd(as.numeric(post_error_rt_vals$task1_stim_keyResp.rt), na.rm = TRUE)
    ########
    ## Post-correct RT for each subject:
    post_correct_rt_vals <- c() # will be filled in the loop below.
    for (ayshan in 1:nrow(keep_rows_with_acc_vals)){
      next_idx <- ayshan + 1
      if (keep_rows_with_acc_vals$accuracy[ayshan] ==1){ #post-correct
        if (keep_rows_with_acc_vals$congruent[ayshan] == 0){ # just incongruent correct trials.
          if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[ayshan] == 31){ # Trial #31 is the last trial in a block and
            # the trial after that is the first trial of the next block.
            post_correct_rt_vals <- post_correct_rt_vals
          } else {
            if (keep_rows_with_acc_vals$accuracy[next_idx] ==1){ # to have only correct trials in the post_correct
              post_correct_rt_vals <- rbind(post_correct_rt_vals, keep_rows_with_acc_vals[next_idx,])
            }
          }
        }
      }
    }
    # As the task1_stim_keyResp.rt variable does have values in a bracket. I have to modify it and then change to numbers.
    for (ayshan2 in 1:nrow(post_correct_rt_vals)){
      post_correct_rt_vals$task1_stim_keyResp.rt[ayshan2] <- gsub("[", "", post_correct_rt_vals$task1_stim_keyResp.rt[ayshan2], fixed = TRUE) # removes the first bracket
      post_correct_rt_vals$task1_stim_keyResp.rt[ayshan2] <- gsub("]", "", post_correct_rt_vals$task1_stim_keyResp.rt[ayshan2], fixed = TRUE) # removes the last bracket
      post_correct_rt_vals$task1_stim_keyResp.rt[ayshan2] <- gsub(",.*", "", post_correct_rt_vals$task1_stim_keyResp.rt[ayshan2])
    }
    # Compute the mean and SD of the post-error RT.
    post_correct_rt_mean <- mean(as.numeric(post_correct_rt_vals$task1_stim_keyResp.rt), na.rm = TRUE)
    post_correct_rt_sd <- sd(as.numeric(post_correct_rt_vals$task1_stim_keyResp.rt), na.rm = TRUE)

    # Computing PES score for each subject:
    post_error_slowing_score <- post_error_rt_mean - post_correct_rt_mean

    ################### END of PES code zone! #############################################################

    #######################################################################################################

    ####### The zone below is for computing post-error/correct accuracy value for each subject! ###########

    # Let's create a dataframe that has only post-error rows. After having this data frame, we can compute the mean
    # post-error accuracy for each subject.
    post_error_accuracy_vals <- c() # will be filled in the loop below.
    for (ayshan in 1:nrow(keep_rows_with_acc_vals)){
      next_idx <- ayshan + 1
      if (keep_rows_with_acc_vals$accuracy[ayshan] ==0){
        if (keep_rows_with_acc_vals$congruent[ayshan] == 0){ # just incongruent errors
          if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[ayshan] == 31){ # Trial #31 is the last trial in a block and
            # the trial after that is the first trial of the next block.
            post_error_accuracy_vals <- post_error_accuracy_vals
          } else {
            post_error_accuracy_vals <- rbind(post_error_accuracy_vals, keep_rows_with_acc_vals[next_idx,])
          }
        }
      }
    }

    # Compute the mean and SD of the post-error accuracy.
    post_error_accuracy_mean <- mean(as.numeric(post_error_accuracy_vals$accuracy), na.rm = TRUE)
    post_error_accuracy_sd <- sd(as.numeric(post_error_accuracy_vals$accuracy), na.rm = TRUE)
    ########
    # Post-correct accuracy for each subject:
    post_correct_accuracy_vals <- c() # will be filled in the loop below.
    for (ayshan in 1:nrow(keep_rows_with_acc_vals)){
      next_idx <- ayshan + 1
      if (keep_rows_with_acc_vals$accuracy[ayshan] ==1){
        if (keep_rows_with_acc_vals$congruent[ayshan] == 0){ # just incongruent errors
          if (keep_rows_with_acc_vals$task_trial_loop.thisTrialN[ayshan] == 31){ # Trial #31 is the last trial in a block and
            # the trial after that is the first trial of the next block.
            post_correct_accuracy_vals <- post_correct_accuracy_vals
          } else {
            post_correct_accuracy_vals <- rbind(post_correct_accuracy_vals, keep_rows_with_acc_vals[next_idx,])
          }
        }
      }
    }

    # Compute the mean and SD of the post-error accuracy.
    post_correct_accuracy_mean <- mean(as.numeric(post_correct_accuracy_vals$accuracy), na.rm = TRUE)
    post_correct_accuracy_sd <- sd(as.numeric(post_correct_accuracy_vals$accuracy), na.rm = TRUE)
    ########
    # Computing PEA score for each subject:
    post_error_accuracy_score <- post_error_accuracy_mean - post_correct_accuracy_mean

    ################### END of post-error/correct accuracy code zone! #####################################

    ##########
    mainDat[nrow(mainDat) + 1,] <-c(id, congAcc, incongAcc, congCorr_meanRT, incongCorr_meanRT, congCorr_logMeanRT, incongCorr_logMeanRT, flankEff_meanACC, flankEff_meanRT, flankEff_logMeanRT, reported_errors, committed_errors, memoryBias_score, num_incong_errorFaces_reported_old, num_incong_errorFaces_reported_new,num_incong_corrFaces_reported_old, num_incong_corrFaces_reported_new, num_post_incong_errorFaces_reported_old, num_post_incong_errorFaces_reported_new, num_post_incong_correctFaces_reported_old, num_post_incong_correctFaces_reported_new, num_pre_incong_errorFaces_reported_old, num_pre_incong_errorFaces_reported_new, num_pre_incong_correctFaces_reported_old, num_pre_incong_correctFaces_reported_new)
    percent_mainDat[nrow(percent_mainDat) + 1,] <-c(id, congAcc, incongAcc, congCorr_meanRT, incongCorr_meanRT, congCorr_logMeanRT,
                                                    incongCorr_logMeanRT, flankEff_meanACC, flankEff_meanRT, flankEff_logMeanRT,
                                                    reported_errors, committed_errors, memoryBias_score, percent_incong_errorFaces_reported_old,
                                                    percent_incong_errorFaces_reported_new,percent_incong_corrFaces_reported_old,
                                                    percent_incong_corrFaces_reported_new, percent_post_incong_errorFaces_reported_old,
                                                    percent_post_incong_errorFaces_reported_new, percent_post_incong_correctFaces_reported_old, percent_post_incong_correctFaces_reported_new,
                                                    percent_pre_incong_errorFaces_reported_old, percent_pre_incong_errorFaces_reported_new,
                                                    percent_pre_incong_correctFaces_reported_old, percent_pre_incong_correctFaces_reported_new,
                                                    post_error_rt_mean, post_error_rt_sd, post_correct_rt_mean, post_correct_rt_sd, post_error_slowing_score,
                                                    post_error_accuracy_mean, post_error_accuracy_sd,
                                                    post_correct_accuracy_mean, post_correct_accuracy_sd, post_error_accuracy_score,
                                                    hit_num, miss_num, corr_rej_num, false_alrams_num, incong_error_hit_num, incong_error_miss_num,
                                                    incong_correct_hit_num, incong_correct_miss_num, incong_post_correct_hit_num, incong_post_correct_miss_num,
                                                    incong_pre_correct_hit_num, incong_pre_correct_miss_num, incong_pre_error_hit_num, incong_pre_error_miss_num,
                                                    incong_post_error_hit_num, incong_post_error_miss_num, post_error_hitRate, pre_error_hitRate,
                                                    pre_correct_hitRate, post_correct_hitRate, correct_hitRate, error_hitRate, post_error_criterion, pre_error_criterion,
                                                    pre_correct_criterion, post_correct_criterion, correct_criterion, error_criterion)
}

################# Computing d prime using Psycho library #################################
dprime.stats <- psycho::dprime(as.numeric(percent_mainDat$hit_num),as.numeric(percent_mainDat$false_alrams_num), as.numeric(percent_mainDat$miss_num), as.numeric(percent_mainDat$corr_rej_num))
percent_mainDat$dprime <- dprime.stats$dprime

d_prime_error.stats <- psycho::dprime(as.numeric(percent_mainDat$incong_error_hit_num),as.numeric(percent_mainDat$false_alrams_num), as.numeric(percent_mainDat$incong_error_miss_num), as.numeric(percent_mainDat$corr_rej_num))
percent_mainDat$d_prime_error <- d_prime_error.stats$dprime

pre_d_prime_error.stats <- psycho::dprime(as.numeric(percent_mainDat$incong_pre_error_hit_num),as.numeric(percent_mainDat$false_alrams_num), as.numeric(percent_mainDat$incong_pre_error_miss_num), as.numeric(percent_mainDat$corr_rej_num))
percent_mainDat$pre_d_prime_error <- pre_d_prime_error.stats$dprime

post_d_prime_error.stats <- psycho::dprime(as.numeric(percent_mainDat$incong_post_error_hit_num),as.numeric(percent_mainDat$false_alrams_num), as.numeric(percent_mainDat$incong_post_error_miss_num), as.numeric(percent_mainDat$corr_rej_num))
percent_mainDat$post_d_prime_error <- post_d_prime_error.stats$dprime

d_prime_correct.stats <- psycho::dprime(as.numeric(percent_mainDat$incong_correct_hit_num),as.numeric(percent_mainDat$false_alrams_num), as.numeric(percent_mainDat$incong_correct_miss_num), as.numeric(percent_mainDat$corr_rej_num))
percent_mainDat$d_prime_correct <- d_prime_correct.stats$dprime

pre_d_prime_correct.stats <- psycho::dprime(as.numeric(percent_mainDat$incong_pre_correct_hit_num),as.numeric(percent_mainDat$false_alrams_num), as.numeric(percent_mainDat$incong_pre_correct_miss_num), as.numeric(percent_mainDat$corr_rej_num))
percent_mainDat$pre_d_prime_correct <- pre_d_prime_correct.stats$dprime

post_d_prime_correct.stats <- psycho::dprime(as.numeric(percent_mainDat$incong_post_correct_hit_num),as.numeric(percent_mainDat$false_alrams_num), as.numeric(percent_mainDat$incong_post_correct_miss_num), as.numeric(percent_mainDat$corr_rej_num))
percent_mainDat$post_d_prime_correct <- post_d_prime_correct.stats$dprime

for (farideh in 1:nrow(percent_mainDat)){
  percent_mainDat$criterion_error_minus_correct[farideh] <- percent_mainDat$error_criterion[farideh] - percent_mainDat$correct_criterion[farideh]
  percent_mainDat$criterion_post_error_minus_correct[farideh] <- percent_mainDat$post_error_criterion[farideh] - percent_mainDat$post_correct_criterion[farideh]
  percent_mainDat$criterion_error_minus_pre_error[farideh] <- percent_mainDat$error_criterion[farideh] - percent_mainDat$pre_error_criterion[farideh]
  percent_mainDat$criterion_post_error_minus_pre_error[farideh] <- percent_mainDat$post_error_criterion[farideh] - percent_mainDat$pre_error_criterion[farideh]
}

######################## Excluding based on hits and d' ########################################
#hit_num_mean <- mean(as.numeric(percent_mainDat$hit_num))
#hit_num_sd <- sd(as.numeric(percent_mainDat$hit_num))
#fa_num_mean <- mean(as.numeric(percent_mainDat$false_alrams_num))
#fa_num_sd <- sd(as.numeric(percent_mainDat$false_alrams_num))
#percent_mainDat <- filter(percent_mainDat, hit_num > (hit_num_mean - hit_num_sd))
#percent_mainDat <- filter(percent_mainDat, hit_num < (hit_num_mean + hit_num_sd))
#percent_mainDat <- filter(percent_mainDat, false_alrams_num > (fa_num_mean - fa_num_sd))
#percent_mainDat <- filter(percent_mainDat, false_alrams_num < (fa_num_mean + fa_num_sd))
#percent_mainDat <- filter(percent_mainDat, dprime > 0)

##############################################################################################

#write the extracted summary scores to disk
write.csv(percent_mainDat,paste(output_path,proc_fileName, sep = "/", collapse = NULL), row.names=FALSE)
write.csv(outlier_mainDat,paste(output_path,"outlier_data.csv", sep = "/", collapse = NULL), row.names=FALSE)
#### Plotting

# pre-error friendly
# error friendly
# Post_error friendly

# pre-correct friendly
# correct friendly
# Post_correct friendly

# pre-error unfriendly
# error unfriendly
# Post_error unfriendly

# pre-correct unfriendly
# correct unfriendly
# Post_correct unfriendly

# The same thing as above for old/new (error and correct)

## Let's do some Bar plotting!
sample_size <- length(datafiles_list) # total number of participants. This will be needed to compute standard error.
# Convert mainDat with specified columns to long format. So, I can use ggplot, etc. easily.

longDat_old <- gather(mainDat, column_name, value, num_pre_incong_errorFaces_reported_old,
                      num_incong_errorFaces_reported_old, num_post_incong_errorFaces_reported_old,
                      num_pre_incong_correctFaces_reported_old, num_incong_corrFaces_reported_old,
                      num_post_incong_correctFaces_reported_old)

longDat_new <- gather(mainDat, column_name, value, num_pre_incong_errorFaces_reported_new,
                      num_incong_errorFaces_reported_new, num_post_incong_errorFaces_reported_new,
                      num_pre_incong_correctFaces_reported_new, num_incong_corrFaces_reported_new,
                      num_post_incong_correctFaces_reported_new)
longDat_sdt_error <- gather(percent_mainDat, column_name, value, error_criterion, pre_error_criterion, post_error_criterion)

longDat_sdt_correct <- gather(percent_mainDat, column_name, value, correct_criterion, pre_correct_criterion, post_correct_criterion)

longDat_post <- gather(percent_mainDat, column_name, value, pre_error_criterion, error_criterion, post_error_criterion, pre_correct_criterion, correct_criterion, post_correct_criterion)

# for percent longDat

percent_longDat_old <- gather(percent_mainDat, column_name, value, percent_pre_incong_errorFaces_reported_old,
                              percent_incong_errorFaces_reported_old, percent_post_incong_errorFaces_reported_old,
                              percent_pre_incong_correctFaces_reported_old, percent_incong_corrFaces_reported_old,
                              percent_post_incong_correctFaces_reported_old)

percent_longDat_new <- gather(percent_mainDat, column_name, value, percent_pre_incong_errorFaces_reported_new,
                              percent_incong_errorFaces_reported_new, percent_post_incong_errorFaces_reported_new,
                              percent_pre_incong_correctFaces_reported_new, percent_incong_corrFaces_reported_new,
                              percent_post_incong_correctFaces_reported_new)
# Old
for_plot_old <- percent_longDat_old %>%
  group_by(column_name) %>%
  summarise(
    n=n(),
    mean=mean(as.numeric(value)),
    sd=sd(as.numeric(value))
  ) %>%
  mutate( se=sd/sqrt(n))
ggplot(for_plot_old) +
  geom_bar( aes(x= column_name, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=column_name, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  ggtitle("Old")

# New
for_plot_new <- percent_longDat_new %>%
  group_by(column_name) %>%
  summarise(
    n=n(),
    mean=mean(as.numeric(value)),
    sd=sd(as.numeric(value))
  ) %>%
  mutate( se=sd/sqrt(n))
ggplot(for_plot_new) +
  geom_bar( aes(x= column_name, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=column_name, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  ggtitle("New")

# SDT plots
for_plot_sdt_corr <- longDat_sdt_correct %>%
  group_by(column_name) %>%
  summarise(
    n=n(),
    mean=mean(as.numeric(value)),
    sd=sd(as.numeric(value))
  ) %>%
  mutate( se=sd/sqrt(n))
ggplot(for_plot_sdt_corr) +
  geom_bar( aes(x= column_name, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=column_name, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  ggtitle("sdt_correct")

for_plot_sdt_err <- longDat_sdt_error %>%
  group_by(column_name) %>%
  summarise(
    n=n(),
    mean=mean(as.numeric(value)),
    sd=sd(as.numeric(value))
  ) %>%
  mutate( se=sd/sqrt(n))
ggplot(for_plot_sdt_err) +
  geom_bar( aes(x= column_name, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=column_name, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  ggtitle("sdt_error")

for_plot_sdt_err_corr <- longDat_post %>%
  group_by(column_name) %>%
  summarise(
    n=n(),
    mean=mean(as.numeric(value)),
    sd=sd(as.numeric(value))
  ) %>%
  mutate( se=sd/sqrt(n))
ggplot(for_plot_sdt_err_corr) +
  geom_bar( aes(x= column_name, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=column_name, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  ggtitle("sdt_error vs. correct")

# stats

# Computing mean and sd for the accuracy in the face flanker task:


t.test((as.numeric(percent_mainDat$post_correct_criterion)), (as.numeric(percent_mainDat$post_error_criterion)), paired = TRUE)
t.test((as.numeric(percent_mainDat$error_criterion)), (as.numeric(percent_mainDat$correct_criterion)), paired = TRUE)
t.test((as.numeric(percent_mainDat$pre_error_criterion)), (as.numeric(percent_mainDat$pre_correct_criterion)), paired = TRUE)
t.test((as.numeric(percent_mainDat$post_error_rt_mean)), (as.numeric(percent_mainDat$post_correct_rt_mean)), paired = TRUE)
t.test((as.numeric(percent_mainDat$post_error_accuracy_mean)), (as.numeric(percent_mainDat$post_correct_accuracy_mean)), paired = TRUE)




### Loading RedCap questionnaire data
redcapDat <- read.csv(file = "/Users/kihossei/OneDrive - Florida International University/Projects/Memory_for_error/redCap_for_mfe_online/202211v0mfeonlinedat_SCRD_2022-12-13_1037.csv")

# Keeping the columns that we need!
redcapDat <- redcapDat[c("record_id", "scaared_b_scrdSoc_s1_r1_e1", "scaared_b_scrdGA_s1_r1_e1", "scaared_b_scrdTotal_s1_r1_e1", "bfne_b_scrdTotal_s1_r1_e1")]

# adding new columns to the "percent_mainDat" dataframe from redcapDat
for (ziba1 in 1:nrow(percent_mainDat)){
  temp_id <- percent_mainDat$id[ziba1]
  tempDat <- filter(redcapDat, record_id == temp_id)
  percent_mainDat$scaared_b_scrdSoc_s1_r1_e1[ziba1] <- tempDat$scaared_b_scrdSoc_s1_r1_e1
}
for (ziba2 in 1:nrow(percent_mainDat)){
  temp_id <- percent_mainDat$id[ziba2]
  tempDat <- filter(redcapDat, record_id == temp_id)
  percent_mainDat$scaared_b_scrdGA_s1_r1_e1[ziba2] <- tempDat$scaared_b_scrdGA_s1_r1_e1
}
for (ziba4 in 1:nrow(percent_mainDat)){
  temp_id <- percent_mainDat$id[ziba4]
  tempDat <- filter(redcapDat, record_id == temp_id)
  percent_mainDat$scaared_b_scrdTotal_s1_r1_e1[ziba4] <- tempDat$scaared_b_scrdTotal_s1_r1_e1
}
for (ziba5 in 1:nrow(percent_mainDat)){
  temp_id <- percent_mainDat$id[ziba5]
  tempDat <- filter(redcapDat, record_id == temp_id)
  percent_mainDat$bfne_b_scrdTotal_s1_r1_e1[ziba5] <- tempDat$bfne_b_scrdTotal_s1_r1_e1
}
# Removing outliers for variables of interest
# list of variables of interest: memoryBias_score, d_prime_error, d_prime_correct, post_d_prime_error, post_d_prime_correct, scaared_b_scrdSoc_s1_r1_e1, scaared_b_scrdTotal_s1_r1_e1, scaared_b_scrdGA_s1_r1_e1, d_prime_error_minus_correct ,post_d_prime_error_minus_correct, unfriendly_error_minus_correct, unfriendly_post_error_minus_correct!
mean_memoryBias_score <- mean(as.numeric(percent_mainDat$memoryBias_score), na.rm = TRUE)
sd_memoryBias_score_threeTimes <- 3*sd(as.numeric(percent_mainDat$memoryBias_score), na.rm = TRUE)

for (zesht4 in 1:nrow(percent_mainDat)){
  if (!is.na(as.numeric(percent_mainDat$memoryBias_score[zesht4])) && !is.na(as.numeric(percent_mainDat$error_criterion[zesht4])) && !is.na(as.numeric(percent_mainDat$correct_criterion[zesht4])) && !is.na(as.numeric(percent_mainDat$post_error_criterion[zesht4])) && !is.na(as.numeric(percent_mainDat$post_correct_criterion[zesht4])) && !is.na(as.numeric(percent_mainDat$criterion_error_minus_pre_error[zesht4])) && !is.na(as.numeric(percent_mainDat$criterion_post_error_minus_pre_error[zesht4])) && !is.na(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1[zesht4])) && !is.na(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1[zesht4])) && !is.na(as.numeric(percent_mainDat$scaared_b_scrdGA_s1_r1_e1[zesht4])) && !is.na(as.numeric(percent_mainDat$criterion_error_minus_correct[zesht4])) && !is.na(as.numeric(percent_mainDat$criterion_post_error_minus_correct[zesht4])) && !is.na(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1[zesht4]))){
    if (as.numeric(percent_mainDat$memoryBias_score[zesht4]) >= (mean_memoryBias_score - sd_memoryBias_score_threeTimes) && as.numeric(percent_mainDat$memoryBias_score[zesht4]) <= (mean_memoryBias_score + sd_memoryBias_score_threeTimes)){
      percent_mainDat$memoryBias_score[zesht4] <- as.numeric(percent_mainDat$memoryBias_score[zesht4])
    } else {
      percent_mainDat$memoryBias_score[zesht4] <- NA
    }

    if (as.numeric(percent_mainDat$error_criterion[zesht4]) >= (mean(as.numeric(percent_mainDat$error_criterion), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$error_criterion), na.rm = TRUE))) && as.numeric(percent_mainDat$error_criterion[zesht4]) <= (mean(as.numeric(percent_mainDat$error_criterion), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$error_criterion), na.rm = TRUE)))){
      percent_mainDat$error_criterion[zesht4] <- as.numeric(percent_mainDat$error_criterion[zesht4])
    } else {
      percent_mainDat$error_criterion[zesht4] <- NA
    }

    if (as.numeric(percent_mainDat$criterion_post_error_minus_pre_error[zesht4]) >= (mean(as.numeric(percent_mainDat$criterion_post_error_minus_pre_error), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$criterion_post_error_minus_pre_error), na.rm = TRUE))) && as.numeric(percent_mainDat$criterion_post_error_minus_pre_error[zesht4]) <= (mean(as.numeric(percent_mainDat$criterion_post_error_minus_pre_error), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$criterion_post_error_minus_pre_error), na.rm = TRUE)))){
      percent_mainDat$criterion_post_error_minus_pre_error[zesht4] <- as.numeric(percent_mainDat$criterion_post_error_minus_pre_error[zesht4])
    } else {
      percent_mainDat$criterion_post_error_minus_pre_error[zesht4] <- NA
    }

    if (as.numeric(percent_mainDat$criterion_error_minus_pre_error[zesht4]) >= (mean(as.numeric(percent_mainDat$criterion_error_minus_pre_error), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$criterion_error_minus_pre_error), na.rm = TRUE))) && as.numeric(percent_mainDat$criterion_error_minus_pre_error[zesht4]) <= (mean(as.numeric(percent_mainDat$criterion_error_minus_pre_error), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$criterion_error_minus_pre_error), na.rm = TRUE)))){
      percent_mainDat$criterion_error_minus_pre_error[zesht4] <- as.numeric(percent_mainDat$criterion_error_minus_pre_error[zesht4])
    } else {
      percent_mainDat$criterion_error_minus_pre_error[zesht4] <- NA
    }

    if (as.numeric(percent_mainDat$correct_criterion[zesht4]) >= (mean(as.numeric(percent_mainDat$correct_criterion), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$correct_criterion), na.rm = TRUE))) && as.numeric(percent_mainDat$correct_criterion[zesht4]) <= (mean(as.numeric(percent_mainDat$correct_criterion), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$correct_criterion), na.rm = TRUE)))){
      percent_mainDat$correct_criterion[zesht4] <- as.numeric(percent_mainDat$correct_criterion[zesht4])
    } else {
      percent_mainDat$correct_criterion[zesht4] <- NA
    }

    if (as.numeric(percent_mainDat$post_error_criterion[zesht4]) >= (mean(as.numeric(percent_mainDat$post_error_criterion), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$post_error_criterion), na.rm = TRUE))) && as.numeric(percent_mainDat$post_error_criterion[zesht4]) <= (mean(as.numeric(percent_mainDat$post_error_criterion), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$post_error_criterion), na.rm = TRUE)))){
      percent_mainDat$post_error_criterion[zesht4] <- as.numeric(percent_mainDat$post_error_criterion[zesht4])
    } else {
      percent_mainDat$post_error_criterion[zesht4] <- NA
    }

    if (as.numeric(percent_mainDat$post_correct_criterion[zesht4]) >= (mean(as.numeric(percent_mainDat$post_correct_criterion), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$post_correct_criterion), na.rm = TRUE))) && as.numeric(percent_mainDat$post_correct_criterion[zesht4]) <= (mean(as.numeric(percent_mainDat$post_correct_criterion), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$post_correct_criterion), na.rm = TRUE)))){
      percent_mainDat$post_correct_criterion[zesht4] <- as.numeric(percent_mainDat$post_correct_criterion[zesht4])
    } else {
      percent_mainDat$post_correct_criterion[zesht4] <- NA
    }

    if (as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1[zesht4]) >= (mean(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), na.rm = TRUE))) && as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1[zesht4]) <= (mean(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), na.rm = TRUE)))){
      percent_mainDat$scaared_b_scrdSoc_s1_r1_e1[zesht4] <- as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1[zesht4])
    } else {
      percent_mainDat$scaared_b_scrdSoc_s1_r1_e1[zesht4] <- NA
    }

    if (as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1[zesht4]) >= (mean(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), na.rm = TRUE))) && as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1[zesht4]) <= (mean(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), na.rm = TRUE)))){
      percent_mainDat$scaared_b_scrdTotal_s1_r1_e1[zesht4] <- as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1[zesht4])
    } else {
      percent_mainDat$scaared_b_scrdTotal_s1_r1_e1[zesht4] <- NA
    }

    if (as.numeric(percent_mainDat$scaared_b_scrdGA_s1_r1_e1[zesht4]) >= (mean(as.numeric(percent_mainDat$scaared_b_scrdGA_s1_r1_e1), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$scaared_b_scrdGA_s1_r1_e1), na.rm = TRUE))) && as.numeric(percent_mainDat$scaared_b_scrdGA_s1_r1_e1[zesht4]) <= (mean(as.numeric(percent_mainDat$scaared_b_scrdGA_s1_r1_e1), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$scaared_b_scrdGA_s1_r1_e1), na.rm = TRUE)))){
      percent_mainDat$scaared_b_scrdGA_s1_r1_e1[zesht4] <- as.numeric(percent_mainDat$scaared_b_scrdGA_s1_r1_e1[zesht4])
    } else {
      percent_mainDat$scaared_b_scrdGA_s1_r1_e1[zesht4] <- NA
    }
    if (as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1[zesht4]) >= (mean(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), na.rm = TRUE))) && as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1[zesht4]) <= (mean(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), na.rm = TRUE)))){
      percent_mainDat$bfne_b_scrdTotal_s1_r1_e1[zesht4] <- as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1[zesht4])
    } else {
      percent_mainDat$bfne_b_scrdTotal_s1_r1_e1[zesht4] <- NA
    }
    if (as.numeric(percent_mainDat$criterion_error_minus_correct[zesht4]) >= (mean(as.numeric(percent_mainDat$criterion_error_minus_correct), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$criterion_error_minus_correct), na.rm = TRUE))) && as.numeric(percent_mainDat$criterion_error_minus_correct[zesht4]) <= (mean(as.numeric(percent_mainDat$criterion_error_minus_correct), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$criterion_error_minus_correct), na.rm = TRUE)))){
      percent_mainDat$criterion_error_minus_correct[zesht4] <- as.numeric(percent_mainDat$criterion_error_minus_correct[zesht4])
    } else {
      percent_mainDat$criterion_error_minus_correct[zesht4] <- NA
    }

    if (as.numeric(percent_mainDat$criterion_post_error_minus_correct[zesht4]) >= (mean(as.numeric(percent_mainDat$criterion_post_error_minus_correct), na.rm = TRUE) - (3*sd(as.numeric(percent_mainDat$criterion_post_error_minus_correct), na.rm = TRUE))) && as.numeric(percent_mainDat$criterion_post_error_minus_correct[zesht4]) <= (mean(as.numeric(percent_mainDat$criterion_post_error_minus_correct), na.rm = TRUE) + (3*sd(as.numeric(percent_mainDat$criterion_post_error_minus_correct), na.rm = TRUE)))){
      percent_mainDat$criterion_post_error_minus_correct[zesht4] <- as.numeric(percent_mainDat$criterion_post_error_minus_correct[zesht4])
    } else {
      percent_mainDat$criterion_post_error_minus_correct[zesht4] <- NA
    }
  }
}

# Running correlation tests and plotting scatter plots


lm_for_cor_fit_line <- lm(scaared_b_scrdSoc_s1_r1_e1 ~ memoryBias_score, percent_mainDat)
cor.test(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), as.numeric(percent_mainDat$memoryBias_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$memoryBias_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$memoryBias_score), method = 'pearson')
ggplot(percent_mainDat, aes(x=memoryBias_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(percent_mainDat, aes(x=memoryBias_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=memoryBias_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


cor.test(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), as.numeric(percent_mainDat$criterion_post_error_minus_correct), method = 'pearson')
cor.test(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$criterion_post_error_minus_correct), method = 'pearson')
cor.test(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$criterion_post_error_minus_correct), method = 'pearson')
ggplot(percent_mainDat, aes(x=criterion_post_error_minus_correct, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=criterion_post_error_minus_correct, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=criterion_post_error_minus_correct, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), as.numeric(percent_mainDat$criterion_post_error_minus_pre_error), method = 'pearson')
cor.test(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$criterion_post_error_minus_pre_error), method = 'pearson')
cor.test(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$criterion_post_error_minus_pre_error), method = 'pearson')
ggplot(percent_mainDat, aes(x=criterion_post_error_minus_pre_error, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=criterion_post_error_minus_pre_error, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=criterion_post_error_minus_pre_error, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), as.numeric(percent_mainDat$criterion_error_minus_pre_error), method = 'pearson')
cor.test(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$criterion_error_minus_pre_error), method = 'pearson')
cor.test(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$criterion_error_minus_pre_error), method = 'pearson')
ggplot(percent_mainDat, aes(x=criterion_error_minus_pre_error, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=criterion_error_minus_pre_error, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=criterion_error_minus_pre_error, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


lm_for_cor_fit_line <- lm(scaared_b_scrdSoc_s1_r1_e1 ~ criterion_error_minus_correct, percent_mainDat)
cor.test(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), as.numeric(percent_mainDat$criterion_error_minus_correct), method = 'pearson')
cor.test(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$criterion_error_minus_correct), method = 'pearson')
cor.test(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$criterion_error_minus_correct), method = 'pearson')
ggplot(percent_mainDat, aes(x=criterion_error_minus_correct, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(percent_mainDat, aes(x=criterion_error_minus_correct, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=criterion_error_minus_correct, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


cor.test(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), as.numeric(percent_mainDat$post_error_criterion), method = 'pearson')
cor.test(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$post_error_criterion), method = 'pearson')
cor.test(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$post_error_criterion), method = 'pearson')
ggplot(percent_mainDat, aes(x=post_error_criterion, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_criterion, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_criterion, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


cor.test(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), as.numeric(percent_mainDat$error_criterion), method = 'pearson')
cor.test(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$error_criterion), method = 'pearson')
cor.test(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$error_criterion), method = 'pearson')
ggplot(percent_mainDat, aes(x=error_criterion, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=error_criterion, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=error_criterion, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


cor.test(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), as.numeric(percent_mainDat$post_error_accuracy_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$post_error_accuracy_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$post_error_accuracy_score), method = 'pearson')
ggplot(percent_mainDat, aes(x=post_error_accuracy_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_accuracy_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_accuracy_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


cor.test(as.numeric(percent_mainDat$scaared_b_scrdSoc_s1_r1_e1), as.numeric(percent_mainDat$post_error_slowing_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$scaared_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$post_error_slowing_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$bfne_b_scrdTotal_s1_r1_e1), as.numeric(percent_mainDat$post_error_slowing_score), method = 'pearson')
ggplot(percent_mainDat, aes(x=post_error_slowing_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_slowing_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_slowing_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(percent_mainDat$error_criterion), as.numeric(percent_mainDat$post_error_slowing_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$post_error_criterion), as.numeric(percent_mainDat$post_error_slowing_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_error_minus_correct), as.numeric(percent_mainDat$post_error_slowing_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_post_error_minus_correct), as.numeric(percent_mainDat$post_error_slowing_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_error_minus_pre_error), as.numeric(percent_mainDat$post_error_slowing_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_post_error_minus_pre_error), as.numeric(percent_mainDat$post_error_slowing_score), method = 'pearson')
ggplot(percent_mainDat, aes(x=post_error_slowing_score, y=error_criterion)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_slowing_score, y=post_error_criterion)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_slowing_score, y=criterion_error_minus_correct)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_slowing_score, y=criterion_post_error_minus_correct)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_slowing_score, y=criterion_error_minus_pre_error)) + geom_point(size = 4) #scatterplot
ggplot(percent_mainDat, aes(x=post_error_slowing_score, y=criterion_post_error_minus_pre_error)) + geom_point(size = 4) #scatterplot

cor.test(as.numeric(percent_mainDat$error_criterion), as.numeric(percent_mainDat$post_error_accuracy_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$post_error_criterion), as.numeric(percent_mainDat$post_error_accuracy_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_error_minus_correct), as.numeric(percent_mainDat$post_error_accuracy_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_post_error_minus_correct), as.numeric(percent_mainDat$post_error_accuracy_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_error_minus_pre_error), as.numeric(percent_mainDat$post_error_accuracy_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_post_error_minus_pre_error), as.numeric(percent_mainDat$post_error_accuracy_score), method = 'pearson')
ggplot(percent_mainDat, aes(x=post_error_accuracy_score, y=error_criterion)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_accuracy_score, y=post_error_criterion)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_accuracy_score, y=criterion_error_minus_correct)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_accuracy_score, y=criterion_post_error_minus_correct)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=post_error_accuracy_score, y=criterion_error_minus_pre_error)) + geom_point(size = 4) #scatterplot
ggplot(percent_mainDat, aes(x=post_error_accuracy_score, y=criterion_post_error_minus_pre_error)) + geom_point(size = 4) #scatterplot

lm_for_cor_fit_line <- lm(criterion_error_minus_correct ~ memoryBias_score, percent_mainDat)
cor.test(as.numeric(percent_mainDat$error_criterion), as.numeric(percent_mainDat$memoryBias_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$post_error_criterion), as.numeric(percent_mainDat$memoryBias_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_error_minus_correct), as.numeric(percent_mainDat$memoryBias_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_post_error_minus_correct), as.numeric(percent_mainDat$memoryBias_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_error_minus_pre_error), as.numeric(percent_mainDat$memoryBias_score), method = 'pearson')
cor.test(as.numeric(percent_mainDat$criterion_post_error_minus_pre_error), as.numeric(percent_mainDat$memoryBias_score), method = 'pearson')
ggplot(percent_mainDat, aes(x=memoryBias_score, y=error_criterion)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=memoryBias_score, y=post_error_criterion)) + geom_point() #scatterplot
ggplot(percent_mainDat, aes(x=memoryBias_score, y=criterion_error_minus_correct)) + geom_point()
ggplot(percent_mainDat, aes(x=memoryBias_score, y=criterion_post_error_minus_correct)) + geom_point(size = 4) #scatterplot
ggplot(percent_mainDat, aes(x=memoryBias_score, y=criterion_error_minus_pre_error)) + geom_point(size = 4) #scatterplot
ggplot(percent_mainDat, aes(x=memoryBias_score, y=criterion_post_error_minus_pre_error)) + geom_point(size = 4) #scatterplot