# This script will run stats on mini_mfe data.
# Author: Kianoosh Hosseini at NDCLab @FIU (https://Kianoosh.info; https://NDClab.com)
# Last Update: 2023-04-20 (YYYY-MM-DD)

library(tidyverse)
library(dplyr)
library(stringr)
library(psycho)
library(car)
library(lme4)
library(ggplot2)
library(emmeans)
library(report)
library(sjPlot)
library(effsize)


#Working directory should be the Psychopy experiment directory.

proje_wd <- "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/Github_Repos/memory-for-error-mini/materials/tasks/mini_mfe"
setwd(proje_wd)

processed_file_input <- paste(proje_wd, "stat_output", sep ="/", collapse = NULL) # input data directory

main_df <-  read.csv(file = paste(processed_file_input, "processed_data_mini_mfe_Proj.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))

##### temp added on Sept 2024

### Loading RedCap questionnaire data
redcapDat <- read.csv(file = "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/Github_Repos/memory-for-error-mini/derivatives/redcap/202203v0socialflanke_SCRD_2022-09-23_1133.csv")

# Keeping the columns that we need!
redcapDat <- redcapDat[c("record_id", "epepq15_scrdTotal_s1_r1_e1")]

# adding new columns to the "percent_mainDat" dataframe from redcapDat
for (rr in 1:nrow(main_df)){
  temp_id <- main_df$participant_id[rr]
  tempDat <- filter(redcapDat, record_id == temp_id)
  if (nrow(tempDat) == 1){
    main_df$epepq15_scrdTotal_s1_r1_e1[rr] <- tempDat$epepq15_scrdTotal_s1_r1_e1
  } else if (nrow(tempDat) == 0){
    main_df$epepq15_scrdTotal_s1_r1_e1[rr] <- NA
  }
}


mean(main_df$epepq15_scrdTotal_s1_r1_e1, na.rm = TRUE) #
sd(main_df$epepq15_scrdTotal_s1_r1_e1, na.rm = TRUE) #
median(main_df$epepq15_scrdTotal_s1_r1_e1, na.rm = TRUE) #

################### end temp



# flanker task stats
# Accuracy
mean(main_df$congAcc, na.rm = TRUE) # 0.96875
sd(main_df$congAcc, na.rm = TRUE) # 0.02995292
# normality test
shapiro.test(main_df$congAcc) # not normal -> p-value = 3.027e-05


mean(main_df$incongAcc, na.rm = TRUE) # 0.8066406
sd(main_df$incongAcc, na.rm = TRUE) # 0.07880051

shapiro.test(main_df$incongAcc) # not normal -> p-value = 0.01746
median(main_df$incongAcc, na.rm = TRUE)
IQR(main_df$incongAcc, na.rm = TRUE)
median(main_df$congAcc, na.rm = TRUE)
IQR(main_df$congAcc, na.rm = TRUE)

# as they are not normal, we perform non-parametric Wilcoxon test instead of t-test
wil_acc <- wilcox.test(main_df$congAcc, main_df$incongAcc, alternative = 'greater', paired = TRUE, na.action = na.omit) # p-value = 8.251e-07
Z_acc <- qnorm(wil_acc$p.value/2) # z-score
r_acc <- abs(Z_acc)/sqrt(32) # r (effect size) However, I reported Cohen's d in the paper. # formulas are from https://stats.stackexchange.com/questions/330129/how-to-get-the-z-score-in-wilcox-test-in-r#:~:text=How%20can%20i%20get%20the,for%20wilcox%20test%20in%20R%3F&text=The%20R%20code%20never%20stores,to%20the%20equivalent%20z%2Dscore.
cohen.d(main_df$congAcc, main_df$incongAcc, paired=TRUE)


# RT (unit in seconds)
mean(main_df$congCorr_meanRT, na.rm = TRUE) # 0.4932228
sd(main_df$congCorr_meanRT, na.rm = TRUE) # 0.04880118
shapiro.test(main_df$congCorr_meanRT) #normal
median(main_df$congCorr_meanRT, na.rm = TRUE)
IQR(main_df$congCorr_meanRT, na.rm = TRUE)

mean(main_df$incongCorr_meanRT, na.rm = TRUE) # 0.5649002
sd(main_df$incongCorr_meanRT, na.rm = TRUE) # 0.05843619
shapiro.test(main_df$incongCorr_meanRT) #non-normal
median(main_df$incongCorr_meanRT, na.rm = TRUE)
IQR(main_df$incongCorr_meanRT, na.rm = TRUE)

mean(main_df$congErr_meanRT, na.rm = TRUE) # 0.5773483
sd(main_df$congErr_meanRT, na.rm = TRUE) # 0.2028307
shapiro.test(main_df$congErr_meanRT) #non-normal


mean(main_df$incongErr_meanRT, na.rm = TRUE) # 0.4667652
sd(main_df$incongErr_meanRT, na.rm = TRUE) # 0.1143685
shapiro.test(main_df$incongErr_meanRT) # non-normal


wil_RT <- wilcox.test(main_df$congCorr_meanRT, main_df$incongCorr_meanRT, alternative = 'less', paired = TRUE, na.action = na.omit) # p-value = 4.657e-10
Z_RT <- qnorm(wil_RT$p.value/2)
r_RT <- abs(Z_RT)/sqrt(32)
cohen.d(main_df$congCorr_meanRT, main_df$incongCorr_meanRT,paired=TRUE)
report(wilcox.test(main_df$congCorr_meanRT, main_df$incongCorr_meanRT, alternative = 'less', paired = TRUE, na.action = na.omit))
##################################################
# Surprise memory task in the mfe_mini
mean(main_df$overall_hitRate, na.rm = TRUE) # 0.4637121
sd(main_df$overall_hitRate, na.rm = TRUE) # 0.1340361

mean(main_df$error_hitRate, na.rm = TRUE) # 0.4602296
sd(main_df$error_hitRate, na.rm = TRUE) # 0.1633875
shapiro.test(main_df$error_hitRate) #normal

mean(main_df$correct_hitRate, na.rm = TRUE) # 0.4739246
sd(main_df$correct_hitRate, na.rm = TRUE) # 0.1488155
shapiro.test(main_df$correct_hitRate) #normal

t.test(main_df$correct_hitRate, main_df$error_hitRate, alternative = 'less', paired = TRUE, na.action = na.omit) # p-value = 0.7211
t.test(main_df$correct_hitRate, main_df$error_hitRate, paired = TRUE, na.action = na.omit) # p-value = 0.5578

report(t.test(main_df$correct_hitRate, main_df$error_hitRate, alternative = 'less', paired = TRUE, na.action = na.omit))
cohen.d(main_df$correct_hitRate, main_df$error_hitRate, paired=TRUE)

##################################################
# Hit Rate correlation with SCAARED social

lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(main_df$scaared_b_scrdSoc_s1_r1_e1, main_df$hitRate_error_minus_correct, alternative = 'greater', method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=hitRate_error_minus_correct)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Error vs. Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))

# Hit Rate correlation with SCAARED GA

lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ scaared_b_scrdGA_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(main_df$scaared_b_scrdGA_s1_r1_e1, main_df$hitRate_error_minus_correct, method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdGA_s1_r1_e1, y=hitRate_error_minus_correct)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED General anxiety score", y = "Error vs. Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))



##################################################
# EEG
# Theta power
mean(main_df$incong_error_theta_power_mfc250, na.rm = TRUE) # 2.620739
sd(main_df$incong_error_theta_power_mfc250, na.rm = TRUE) # 2.331685

mean(main_df$incong_correct_theta_power_mfc250, na.rm = TRUE) # -1.286565
sd(main_df$incong_correct_theta_power_mfc250, na.rm = TRUE) # 1.285078


t.test(main_df$incong_error_theta_power_mfc250, main_df$incong_correct_theta_power_mfc250, alternative = 'greater', paired = TRUE, na.action = na.omit) # p-value = 8.461e-09
report(t.test(main_df$incong_error_theta_power_mfc250, main_df$incong_correct_theta_power_mfc250, alternative = 'greater', paired = TRUE, na.action = na.omit))
cohen.d(main_df$incong_error_theta_power_mfc250, main_df$incong_correct_theta_power_mfc250, paired=TRUE, na.rm = TRUE)

# ITPS
mean(main_df$incong_error_theta_ITPS_mfc250, na.rm = TRUE) # 0.1105076
sd(main_df$incong_error_theta_ITPS_mfc250, na.rm = TRUE) # 0.1480933

mean(main_df$incong_correct_theta_ITPS_mfc250, na.rm = TRUE) # 0.052553
sd(main_df$incong_correct_theta_ITPS_mfc250, na.rm = TRUE) # 0.09557298

t.test(main_df$incong_error_theta_ITPS_mfc250, main_df$incong_correct_theta_ITPS_mfc250, alternative = 'greater', paired = TRUE, na.action = na.omit) # p-value = 0.07422
report(t.test(main_df$incong_error_theta_ITPS_mfc250, main_df$incong_correct_theta_ITPS_mfc250, alternative = 'greater', paired = TRUE, na.action = na.omit))
cohen.d(main_df$incong_error_theta_ITPS_mfc250, main_df$incong_correct_theta_ITPS_mfc250, paired=TRUE, na.rm = TRUE)
# wPLI

mean(main_df$incong_error_theta_wPLI_posterior250, na.rm = TRUE) # 0.01847264
sd(main_df$incong_error_theta_wPLI_posterior250, na.rm = TRUE) # 0.07653171

mean(main_df$incong_correct_theta_wPLI_posterior250, na.rm = TRUE) # -0.0370349
sd(main_df$incong_correct_theta_wPLI_posterior250, na.rm = TRUE) # 0.04575801

t.test(main_df$incong_error_theta_wPLI_posterior250, main_df$incong_correct_theta_wPLI_posterior250, alternative = 'greater', paired = TRUE, na.action = na.omit) #p-value = 0.004481
report(t.test(main_df$incong_error_theta_wPLI_posterior250, main_df$incong_correct_theta_wPLI_posterior250, alternative = 'greater', paired = TRUE, na.action = na.omit))
cohen.d(main_df$incong_error_theta_wPLI_posterior250, main_df$incong_correct_theta_wPLI_posterior250, paired=TRUE, na.rm = TRUE)

# Neural responses to errors correlations with social anxiety SCAARED
lm_for_cor_fit_line <- lm(theta_power_mfc250_difference_score ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_power_mfc250_difference_score), alternative = 'greater', method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=theta_power_mfc250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Error Vs. Correct within MFC Power") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))


lm_for_cor_fit_line <- lm(theta_ITPS_mfc250_difference_score ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc250_difference_score), alternative = 'greater', method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=theta_ITPS_mfc250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Error Vs. Correct within MFC ITPS") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))


lm_for_cor_fit_line <- lm(theta_wPLI_posterior250_difference_score ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_wPLI_posterior250_difference_score), alternative = 'greater', method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=theta_wPLI_posterior250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Error Vs. Correct Between MFC-Visual Sensory cortex wPLI") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))



# Neural responses to errors correlations with General anxiety SCAARED
lm_for_cor_fit_line <- lm(theta_power_mfc250_difference_score ~ scaared_b_scrdGA_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1), as.numeric(main_df$theta_power_mfc250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdGA_s1_r1_e1, y=theta_power_mfc250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED General anxiety score", y = "Error Vs. Correct within MFC Power") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))


lm_for_cor_fit_line <- lm(theta_ITPS_mfc250_difference_score ~ scaared_b_scrdGA_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdGA_s1_r1_e1, y=theta_ITPS_mfc250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED General anxiety score", y = "Error Vs. Correct within MFC ITPS") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))


lm_for_cor_fit_line <- lm(theta_wPLI_posterior250_difference_score ~ scaared_b_scrdGA_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(as.numeric(main_df$scaared_b_scrdGA_s1_r1_e1), as.numeric(main_df$theta_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdGA_s1_r1_e1, y=theta_wPLI_posterior250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED General anxiety score", y = "Error Vs. Correct Between MFC-Visual Sensory cortex wPLI") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))

# multiple comparisons using holm-sidak
# https://www.graphpad.com/guides/prism/latest/statistics/stat_how_the_holm_method_woks.htm

####################
# Neural responses to errors correlations with hit rate (recognition memory performance)
# standardize variables for the regression models below

# remove complete cases
new_df_ipc <- main_df[complete.cases(main_df[ , c('hitRate_error_minus_correct', 'theta_ITPS_mfc250_difference_score')]), ]
new_df_wpli <- main_df[complete.cases(main_df[ , c('hitRate_error_minus_correct', 'theta_wPLI_posterior250_difference_score')]), ]
# Scale
new_df_ipc$hitRate_error_minus_correct <- scale(new_df_ipc$hitRate_error_minus_correct)
new_df_wpli$hitRate_error_minus_correct <- scale(new_df_wpli$hitRate_error_minus_correct)

new_df_wpli$theta_wPLI_posterior250_difference_score <- scale(new_df_wpli$theta_wPLI_posterior250_difference_score)
new_df_ipc$theta_ITPS_mfc250_difference_score <- scale(new_df_ipc$theta_ITPS_mfc250_difference_score)


# Regressions
lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ theta_ITPS_mfc250_difference_score, new_df_ipc)
summary(lm_for_cor_fit_line) # Divide the p-value by 2 as we conduct one-sided test.
confint(lm_for_cor_fit_line, 'theta_ITPS_mfc250_difference_score', level=0.95)
cor.test(as.numeric(main_df$hitRate_error_minus_correct), as.numeric(main_df$theta_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_ITPS_mfc250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "Error vs. Correct Hit rate", y = "Error Vs. Correct within MFC ITPS") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))


lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ theta_wPLI_posterior250_difference_score, new_df_wpli)
summary(lm_for_cor_fit_line) # Divide the p-value by 2 as we conduct one-sided test.
confint(lm_for_cor_fit_line, 'theta_wPLI_posterior250_difference_score', level=0.95)
cor.test(as.numeric(main_df$hitRate_error_minus_correct), as.numeric(main_df$theta_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_wPLI_posterior250_difference_score, y=hitRate_error_minus_correct)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "Error vs. Correct between MFC-Sensory (visual) wPLI ", y = "Memory Bias for Error Events (Error - Correct Hit Rate %") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))

# Supplementary
lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ theta_power_mfc250_difference_score, new_df_ipc)
summary(lm_for_cor_fit_line) # Don't divide by two as it is exploratory.
confint(lm_for_cor_fit_line, 'theta_power_mfc250_difference_score', level=0.95)
cor.test(as.numeric(main_df$hitRate_error_minus_correct), as.numeric(main_df$theta_power_mfc250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_power_mfc250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "Error vs. Correct Hit rate", y = "Error Vs. Correct within MFC Power") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))


#######################

# Compute cronbach.alpha for SCAARED
# Define file paths for your CSV files
redcap_mfe_mini_scaaredSoc <- "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/Github_Repos/memory-for-error-mini/derivatives/redcap/202203v0socialflanke_SCRD_2022-09-23_1133.csv"

# Define the columns to keep
selected_columns <- c("scaared_b_i3_s1_r1_e1",
                      "scaared_b_i10_s1_r1_e1",
                      "scaared_b_i27_s1_r1_e1",
                      "scaared_b_i34_s1_r1_e1",
                      "scaared_b_i41_s1_r1_e1",
                      "scaared_b_i42_s1_r1_e1",
                      "scaared_b_i43_s1_r1_e1")

# Function to load and select columns from a CSV file
load_and_select <- function(file_path, columns) {
  tryCatch({
    df <- read_csv(file_path)

    # Check if all specified columns exist in the dataframe
    if (!all(columns %in% names(df))) {
      missing_cols <- setdiff(columns, names(df))
      stop(paste("The following columns are missing in file:", file_path, paste(missing_cols, collapse = ", ")))
    }

    df_selected <- df %>% dplyr::select(all_of(columns))
    return(df_selected)
  }, error = function(e) {
    message(paste("Error processing file:", file_path))
    message(e)
    return(NULL) # Return NULL in case of an error
  })
}

# Load and select columns from both files
df1_selected <- load_and_select(redcap_mfe_mini_scaaredSoc, selected_columns)


# Check if both dataframes were loaded successfully before binding
if (!is.null(df1_selected)) {
  # Bind the dataframes using bind_rows (from dplyr)
  combined_df <- bind_rows(df1_selected)
} else {
  message("One or both files could not be loaded. Please check the file paths and column names.")
}

install.packages("ltm")
library(ltm) # for cronbach.alpha
cronbach.alpha(combined_df, standardized = TRUE, na.rm = TRUE)
cronbach.alpha(combined_df, standardized = FALSE, na.rm = TRUE)

######################################################



# ERP N170 (supplementary)

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$N170_error), method = 'pearson', na.action = na.omit)

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$N170_correct), method = 'pearson', na.action = na.omit)

lm_for_cor_fit_line <- lm(N170_delta ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$N170_delta), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=N170_delta)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED Anxiety Score", y = "Error vs. Correct N170") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 20)) + theme(text = element_text(size = 30))





lm_for_cor_fit_line <- lm(N170_delta ~ hitRate_error_minus_correct, main_df)
summary(lm_for_cor_fit_line)
cor.test(as.numeric(main_df$hitRate_error_minus_correct), as.numeric(main_df$N170_delta), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=N170_delta)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "Memory Bias For Error Events", y = "Error vs. Correct N170") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 20)) + theme(text = element_text(size = 30))

