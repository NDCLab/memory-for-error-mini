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

#Working directory should be the Psychopy experiment directory.

proje_wd <- "/Users/kihossei/Documents/GitHub/memory-for-error-mini/materials/mini_mfe"
setwd(proje_wd)

processed_file_input <- paste(proje_wd, "stat_output", sep ="/", collapse = NULL) # input data directory

main_df <-  read.csv(file = paste(processed_file_input, "processed_data_mini_mfe_Proj.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))
friendly_df <-  read.csv(file = paste(processed_file_input, "processed_data_mini_mfe_Proj_for_friendly.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))


# flanker task stats
# Accuracy
mean(main_df$congAcc, na.rm = TRUE) # 0.96875
sd(main_df$congAcc, na.rm = TRUE) # 0.02995292

mean(main_df$incongAcc, na.rm = TRUE) # 0.8066406
sd(main_df$incongAcc, na.rm = TRUE) # 0.07880051

t.test(main_df$congAcc, main_df$incongAcc, paired = TRUE, na.action = na.omit) # p-value = 1.064e-12
report(t.test(main_df$congAcc, main_df$incongAcc, paired = TRUE, na.action = na.omit))
# RT (unit in seconds)
mean(main_df$congCorr_meanRT, na.rm = TRUE) # 0.4932228
sd(main_df$congCorr_meanRT, na.rm = TRUE) # 0.04880118

mean(main_df$incongCorr_meanRT, na.rm = TRUE) # 0.5649002
sd(main_df$incongCorr_meanRT, na.rm = TRUE) # 0.05843619

mean(main_df$congErr_meanRT, na.rm = TRUE) # 0.5773483
sd(main_df$congErr_meanRT, na.rm = TRUE) # 0.2028307

mean(main_df$incongErr_meanRT, na.rm = TRUE) # 0.4667652
sd(main_df$incongErr_meanRT, na.rm = TRUE) # 0.1143685

report(t.test(main_df$congCorr_meanRT, main_df$incongCorr_meanRT, paired = TRUE, na.action = na.omit))


##################################################
# Surprise memory task
mean(main_df$error_hitRate, na.rm = TRUE) # 0.4602296
sd(main_df$error_hitRate, na.rm = TRUE) # 0.1633875

mean(main_df$correct_hitRate, na.rm = TRUE) # 0.4739246
sd(main_df$correct_hitRate, na.rm = TRUE) # 0.1488155

t.test(main_df$correct_hitRate, main_df$error_hitRate, paired = TRUE, na.action = na.omit) # p-value = 0.5578
report(t.test(main_df$correct_hitRate, main_df$error_hitRate, paired = TRUE, na.action = na.omit))


##################################################
# EEG
# Theta power
mean(main_df$incong_error_theta_power_mfc250, na.rm = TRUE) # 2.620739
sd(main_df$incong_error_theta_power_mfc250, na.rm = TRUE) # 2.331685

mean(main_df$incong_correct_theta_power_mfc250, na.rm = TRUE) # -1.286565
sd(main_df$incong_correct_theta_power_mfc250, na.rm = TRUE) # 1.285078


t.test(main_df$incong_error_theta_power_mfc250, main_df$incong_correct_theta_power_mfc250, paired = TRUE, na.action = na.omit) # p-value = 8.461e-09
report(t.test(main_df$incong_error_theta_power_mfc250, main_df$incong_correct_theta_power_mfc250, paired = TRUE, na.action = na.omit))
# ITPS
mean(main_df$incong_error_theta_ITPS_mfc250, na.rm = TRUE) # 0.1105076
sd(main_df$incong_error_theta_ITPS_mfc250, na.rm = TRUE) # 0.1480933

mean(main_df$incong_correct_theta_ITPS_mfc250, na.rm = TRUE) # 0.052553
sd(main_df$incong_correct_theta_ITPS_mfc250, na.rm = TRUE) # 0.09557298

t.test(main_df$incong_error_theta_ITPS_mfc250, main_df$incong_correct_theta_ITPS_mfc250, paired = TRUE, na.action = na.omit) # p-value = 0.07422
report(t.test(main_df$incong_error_theta_ITPS_mfc250, main_df$incong_correct_theta_ITPS_mfc250, paired = TRUE, na.action = na.omit))
# wPLI

mean(main_df$incong_error_theta_wPLI_posterior250, na.rm = TRUE) # 0.01847264
sd(main_df$incong_error_theta_wPLI_posterior250, na.rm = TRUE) # 0.07653171

mean(main_df$incong_correct_theta_wPLI_posterior250, na.rm = TRUE) # -0.0370349
sd(main_df$incong_correct_theta_wPLI_posterior250, na.rm = TRUE) # 0.04575801

t.test(main_df$incong_error_theta_wPLI_posterior250, main_df$incong_correct_theta_wPLI_posterior250, paired = TRUE, na.action = na.omit) #p-value = 0.004481
report(t.test(main_df$incong_error_theta_wPLI_posterior250, main_df$incong_correct_theta_wPLI_posterior250, paired = TRUE, na.action = na.omit))


cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
lm_for_cor_fit_line <- lm(theta_ITPS_mfc250_difference_score ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
set_theme(base = theme_classic(), #To remove the background color and the grids
          theme.font = 'serif',   #To change the font type
          axis.title.size = 1.5,  #To change axis title size
          axis.textsize.x = 1.2,  #To change x axis text size
          axis.textsize.y = 1.2)  #To change y axis text size

sjPlot::plot_model(lm_for_cor_fit_line, type = "pred", terms = c("scaared_b_scrdSoc_s1_r1_e1"), axis.title = c("SCAARED-Social Anxiety Score", "Error Vs. Correct within MFC ITPS"), title = "", colors = "blue")

#######################
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
lm_for_cor_fit_line <- lm(theta_wPLI_posterior250_difference_score ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
set_theme(base = theme_classic(), #To remove the background color and the grids
          theme.font = 'serif',   #To change the font type
          axis.title.size = 1.5,  #To change axis title size
          axis.textsize.x = 1.2,  #To change x axis text size
          axis.textsize.y = 1.2)  #To change y axis text size

sjPlot::plot_model(lm_for_cor_fit_line, type = "pred", terms = c("scaared_b_scrdSoc_s1_r1_e1"), axis.title = c("SCAARED-Social Anxiety Score", "Error Vs. Correct Between MFC-Visual Sensory cortex wPLI"), title = "", colors = "blue")

#######################
# Compute cronbach.alpha for SCAARED
proje_wd <- "/Users/kihossei/Documents/GitHub/memory-for-error-mini/input"
setwd(proje_wd)

just_scaared <-  read.csv(file = paste(proje_wd, "202203v0socialflanke_DATA_2023-05-09_1306_only_social_phobia_items_kept_others_removed_manually.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))
just_scaared2 <- t(just_scaared)
install.packages("ltm")
library(ltm) # for cronbach.alpha
cronbach.alpha(just_scaared, standardized = TRUE, na.rm = TRUE)

# Alternative pacckage that provides further details
library(psych)
psych::alpha(just_scaared)



######################################################


lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=hitRate_error_minus_correct)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Error vs. Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))

#####################
lm_for_cor_fit_line <- lm(theta_ITPS_mfc250_difference_score ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=theta_ITPS_mfc250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Error vs. Correct within MFC ITPS") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))

####################
lm_for_cor_fit_line <- lm(theta_wPLI_posterior250_difference_score ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=theta_wPLI_posterior250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Error vs. Correct between MFC-sensory (visual) wPLI") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))

####################
lm_for_cor_fit_line <- lm(theta_wPLI_posterior250_difference_score ~ hitRate_error_minus_correct, main_df)
cor.test(as.numeric(main_df$hitRate_error_minus_correct), as.numeric(main_df$theta_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_wPLI_posterior250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "Error vs. Correct Hit rate", y = "Error vs. Correct between MFC-sensory (visual) wPLI") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))





# ERN
# Exploratory tests
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$ERN), method = 'pearson', na.action = na.omit)

lm_for_cor_fit_line <- lm(deltaERN ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$deltaERN), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=deltaERN)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED Social Anxiety", y = "Delta ERN") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))


cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$deltaERN), method = 'pearson', na.action = na.omit)


cor.test(as.numeric(friendly_df$current_trial_diff_friendly), as.numeric(main_df$deltaERN), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(friendly_df$post_trial_diff_friendly), as.numeric(main_df$deltaERN), method = 'pearson', na.action = na.omit)


lm_for_cor_fit_line <- lm(deltaERN ~ hitRate_error_minus_correct, main_df)
cor.test(as.numeric(main_df$hitRate_error_minus_correct), as.numeric(main_df$deltaERN), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=deltaERN)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "Error vs. Correct Hit Rate", y = "Delta ERN") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))



cor.test(as.numeric(main_df$deltaERN), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)


# TF for checking post_error
# Exploratory tests
cor.test(as.numeric(main_df$theta_wPLI_posterior250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)


lm_for_cor_fit_line <- lm(incong_error_theta_power_mfc250 ~ hitRate_post_error_minus_correct, main_df)
cor.test(as.numeric(main_df$hitRate_post_error_minus_correct), as.numeric(main_df$incong_error_theta_power_mfc250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=incong_error_theta_power_mfc250)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "Post-Error vs. Post-Correct Hit Rate", y = "Error vs. Correct theta Power") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))


lm_for_cor_fit_line <- lm(theta_wPLI_posterior250_difference_score ~ hitRate_post_error_minus_correct, main_df)
cor.test(as.numeric(main_df$hitRate_post_error_minus_correct), as.numeric(main_df$theta_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_wPLI_posterior250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "Post-Error vs. Post-Correct Hit Rate", y = "Error vs. Correct theta wPLI") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))




lm_for_cor_fit_line <- lm(theta_ITPS_mfc250_difference_score ~ hitRate_post_error_minus_correct, main_df)
cor.test(as.numeric(main_df$hitRate_post_error_minus_correct), as.numeric(main_df$theta_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_ITPS_mfc250_difference_score)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "Post-Error vs. Post-Correct Hit Rate", y = "Error vs. Correct theta ITPS") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))


lm_for_cor_fit_line <- lm(scaared_b_scrdSoc_s1_r1_e1 ~ hitRate_post_error_minus_correct, main_df)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "Post-Error vs. Post-Correct Hit Rate", y = "SCAARED Anxiety Score") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 20))
