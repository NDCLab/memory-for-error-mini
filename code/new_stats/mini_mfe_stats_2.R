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

#Working directory should be the Psychopy experiment directory.

proje_wd <- "/Users/kihossei/Documents/GitHub/memory-for-error-mini/materials/task/mini_mfe"
setwd(proje_wd)

processed_file_input <- paste(proje_wd, "stat_output", sep ="/", collapse = NULL) # input data directory

main_df <-  read.csv(file = paste(processed_file_input, "processed_data_mini_mfe_Proj.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))


# flanker task stats
# Accuracy
mean(main_df$congAcc, na.rm = TRUE) # 0.96875
sd(main_df$congAcc, na.rm = TRUE) # 0.02995292

mean(main_df$incongAcc, na.rm = TRUE) # 0.8066406
sd(main_df$incongAcc, na.rm = TRUE) # 0.07880051

t.test(main_df$congAcc, main_df$incongAcc, paired = TRUE, na.action = na.omit) # p-value = 1.064e-12

# RT (unit in seconds)
mean(main_df$congCorr_meanRT, na.rm = TRUE) # 0.4932228
sd(main_df$congCorr_meanRT, na.rm = TRUE) # 0.04880118

mean(main_df$incongCorr_meanRT, na.rm = TRUE) # 0.5649002
sd(main_df$incongCorr_meanRT, na.rm = TRUE) # 0.05843619

mean(main_df$congErr_meanRT, na.rm = TRUE) # 0.5773483
sd(main_df$congErr_meanRT, na.rm = TRUE) # 0.2028307

mean(main_df$incongErr_meanRT, na.rm = TRUE) # 0.4667652
sd(main_df$incongErr_meanRT, na.rm = TRUE) # 0.1143685

# 2x2 ANOVA: IVs are congruency and accuracy
# To perform this, I need to create another dataframe with 3 columns (i.e., congruency, accuracy, and meanRT_value)
# meanRT_value is the continuous outcome variable.

# Create data frame with 3 columns and row_n rows filled with NA
# row_n = number of rows in main_df * 4 (see below to know where 4 comes from)
row_n <- nrow(main_df) * 4
df_for_anova_congByAcc <- data.frame(id= rep(NA, row_n), congruency = rep(NA, row_n), accuracy = rep(NA, row_n), meanRT_value = rep(NA, row_n))
a <- 0
for (i in 1:nrow(main_df)){

  df_for_anova_congByAcc$id[a + 1] <- main_df$participant_id[i]
  df_for_anova_congByAcc$congruency[a + 1] <- 1
  df_for_anova_congByAcc$accuracy[a + 1] <- 1
  df_for_anova_congByAcc$meanRT_value [a + 1] <- main_df$congCorr_meanRT[i] # congCorr

  df_for_anova_congByAcc$id[a + 2] <- main_df$participant_id[i]
  df_for_anova_congByAcc$congruency[a + 2] <- 1
  df_for_anova_congByAcc$accuracy[a + 2] <- 0
  df_for_anova_congByAcc$meanRT_value [a + 2] <- main_df$congErr_meanRT[i] # congErr

  df_for_anova_congByAcc$id[a + 3] <- main_df$participant_id[i]
  df_for_anova_congByAcc$congruency[a + 3] <- 0
  df_for_anova_congByAcc$accuracy[a + 3] <- 1
  df_for_anova_congByAcc$meanRT_value [a + 3] <-  main_df$incongCorr_meanRT[i] # incongCorr

  df_for_anova_congByAcc$id[a + 4] <- main_df$participant_id[i]
  df_for_anova_congByAcc$congruency[a + 4] <- 0
  df_for_anova_congByAcc$accuracy[a + 4] <- 0
  df_for_anova_congByAcc$meanRT_value [a + 4] <- main_df$incongErr_meanRT[i] # incongErr
  a <- a + 4
}
df_for_anova_congByAcc[,1] <- as.factor(df_for_anova_congByAcc[,1])
df_for_anova_congByAcc[,2] <- as.factor(df_for_anova_congByAcc[,2])
df_for_anova_congByAcc[,3] <- as.factor(df_for_anova_congByAcc[,3]) # accuracy

contrasts(df_for_anova_congByAcc[,3]) <- contr.sum(2) # convert accuracy to sum contrasts
contrasts(df_for_anova_congByAcc[,2]) <- contr.sum(2)
######
# To run factorial repeasted measures ANOVA in R involves fitting a linear mixed-effects model.
# Random intercept per subject
# (1 | id) in the parantheses below indicates that we have random intercepts subject (i.e., id)

model <- lmer(meanRT_value ~ congruency + accuracy + congruency:accuracy + (1 | id), data = df_for_anova_congByAcc)
Anova(model, type = "III")
# to include, p-values in the results of summary, I need to use lmerTest package.
install.packages("lmerTest") # install lmerTest package
library(lmerTest) # load lmerTest package
summary(model, type = III)
# plot
library(sjPlot)
sjPlot::plot_model(model, type = "pred", terms = c("congruency", "accuracy"))

##################################################
# Surprise memory task
mean(main_df$error_hitRate, na.rm = TRUE) # 0.4602296
sd(main_df$error_hitRate, na.rm = TRUE) # 0.1633875

mean(main_df$correct_hitRate, na.rm = TRUE) # 0.4739246
sd(main_df$correct_hitRate, na.rm = TRUE) # 0.1488155

t.test(main_df$correct_hitRate, main_df$error_hitRate, paired = TRUE, na.action = na.omit) # p-value = 0.5578

mean(main_df$post_error_hitRate, na.rm = TRUE) # 0.5001649
sd(main_df$post_error_hitRate, na.rm = TRUE) # 0.1783416

mean(main_df$post_correct_hitRate, na.rm = TRUE) # 0.455674
sd(main_df$post_correct_hitRate, na.rm = TRUE) # 0.1355239

t.test(main_df$post_correct_hitRate, main_df$post_error_hitRate, paired = TRUE, na.action = na.omit) # p-value = 0.09549

##################################################
# EEG
# Theta power
mean(main_df$incong_error_theta_power_mfc250, na.rm = TRUE) # 2.620739
sd(main_df$incong_error_theta_power_mfc250, na.rm = TRUE) # 2.331685

mean(main_df$incong_correct_theta_power_mfc250, na.rm = TRUE) # -1.286565
sd(main_df$incong_correct_theta_power_mfc250, na.rm = TRUE) # 1.285078


t.test(main_df$incong_error_theta_power_mfc250, main_df$incong_correct_theta_power_mfc250, paired = TRUE, na.action = na.omit) # p-value = 8.461e-09

# ITPS
mean(main_df$incong_error_theta_ITPS_mfc250, na.rm = TRUE) # 0.1105076
sd(main_df$incong_error_theta_ITPS_mfc250, na.rm = TRUE) # 0.1480933

mean(main_df$incong_correct_theta_ITPS_mfc250, na.rm = TRUE) # 0.052553
sd(main_df$incong_correct_theta_ITPS_mfc250, na.rm = TRUE) # 0.09557298

t.test(main_df$incong_error_theta_ITPS_mfc250, main_df$incong_correct_theta_ITPS_mfc250, paired = TRUE, na.action = na.omit) # p-value = 0.07422

# wPLI

# 2x2 ANOVA: IVs are brain_region and accuracy
# To perform this, I need to create another dataframe with 3 columns (i.e., brain_region, accuracy, and wPLI_value)
# wPLI_value is the continuous outcome variable.
# brain_region values will be: lateral-frontal area (electrodes: 4, 6, 36, 39) and posterior area (electrodes: 22, 53, 24, and 55) -> time window: 0-250 msec.

# Create data frame with 3 columns and row_n rows filled with NA
# row_n = number of rows in main_df * 4 (see below to know where 4 comes from)
row_n <- nrow(main_df) * 4
df_for_anova_wPLI <- data.frame(id= rep(NA, row_n), brain_region = rep(NA, row_n), accuracy = rep(NA, row_n), wPLI_value = rep(NA, row_n))
a <- 0
for (i in 1:nrow(main_df)){

  df_for_anova_wPLI$id[a + 1] <- main_df$participant_id[i]
  df_for_anova_wPLI$brain_region[a + 1] <- 'lateral_frontal'
  df_for_anova_wPLI$accuracy[a + 1] <- 1
  df_for_anova_wPLI$wPLI_value [a + 1] <- main_df$incong_correct_theta_wPLI_latfrontal250[i] # Correct_lateral-frontal

  df_for_anova_wPLI$id[a + 2] <- main_df$participant_id[i]
  df_for_anova_wPLI$brain_region[a + 2] <- 'lateral_frontal'
  df_for_anova_wPLI$accuracy[a + 2] <- 0
  df_for_anova_wPLI$wPLI_value [a + 2] <- main_df$incong_error_theta_wPLI_latfrontal250[i] # Error_lateral-frontal

  df_for_anova_wPLI$id[a + 3] <- main_df$participant_id[i]
  df_for_anova_wPLI$brain_region[a + 3] <- 'posterior'
  df_for_anova_wPLI$accuracy[a + 3] <- 1
  df_for_anova_wPLI$wPLI_value [a + 3] <-  main_df$incong_correct_theta_wPLI_posterior250[i] # Correct_posterior

  df_for_anova_wPLI$id[a + 4] <- main_df$participant_id[i]
  df_for_anova_wPLI$brain_region[a + 4] <- 'posterior'
  df_for_anova_wPLI$accuracy[a + 4] <- 0
  df_for_anova_wPLI$wPLI_value [a + 4] <- main_df$incong_error_theta_wPLI_posterior250[i] # Error_posterior
  a <- a + 4
}
df_for_anova_wPLI[,1] <- as.factor(df_for_anova_wPLI[,1])
df_for_anova_wPLI[,2] <- as.factor(df_for_anova_wPLI[,2])
df_for_anova_wPLI[,3] <- as.factor(df_for_anova_wPLI[,3]) # accuracy

contrasts(df_for_anova_wPLI[,3]) <- contr.sum(2) # convert accuracy to sum contrasts

######
# To run factorial repeasted measures ANOVA in R involves fitting a linear mixed-effects model.
# Random intercept per subject
# (1 | id) in the parantheses below indicates that we have random intercepts subject (i.e., id)

wPLI_model <- lmer(wPLI_value ~ brain_region + accuracy + brain_region:accuracy + (1 | id), data = df_for_anova_wPLI)
Anova(wPLI_model, type = "III")
# to include, p-values in the results of summary, I need to use lmerTest package.
#install.packages("lmerTest") # install lmerTest package
library(lmerTest) # load lmerTest package
summary(wPLI_model, type = III)
# plot
library(sjPlot)
sjPlot::plot_model(wPLI_model, type = "pred", terms = c("brain_region", "accuracy"))
sjPlot::plot_model(wPLI_model, type = "pred", terms = c("accuracy")) #significant

t.test(main_df$incong_error_theta_wPLI_posterior250, main_df$incong_correct_theta_wPLI_posterior250, paired = TRUE, na.action = na.omit)
t.test(main_df$incong_error_theta_wPLI_latfrontal250, main_df$incong_correct_theta_wPLI_latfrontal250, paired = TRUE, na.action = na.omit)

############################################################################################

# Dataset below will be used for regressions 1-5!
# This is a mixed-effects model as we have two recording from each participant (error and correct). So, it is like
# a repeated measures ANOVA.
row_n <- nrow(main_df) * 2
df_for_regs <- data.frame(id= rep(NA, row_n), hitRate = rep(NA, row_n), mfc_theta_power = rep(NA, row_n), mfc_theta_itps = rep(NA, row_n), accuracy = rep(NA, row_n), scaared_social = rep(NA, row_n))
a <- 0
for (i in 1:nrow(main_df)){

  df_for_regs$id[a + 1] <- main_df$participant_id[i]
  df_for_regs$hitRate[a + 1] <- main_df$error_hitRate[i] #error_hitrate
  df_for_regs$mfc_theta_power[a + 1] <- main_df$incong_error_theta_power_mfc250[i] #error_mfc_theta_power
  df_for_regs$mfc_theta_itps[a + 1] <- main_df$incong_error_theta_ITPS_mfc250[i] #error_mfc_theta_itps
  df_for_regs$accuracy[a + 1] <- 0 # error
  df_for_regs$scaared_social[a + 1] <- main_df$scaared_b_scrdSoc_s1_r1_e1[i] # scaared_social

  df_for_regs$id[a + 2] <- main_df$participant_id[i]
  df_for_regs$hitRate[a + 2] <- main_df$correct_hitRate[i] #correct_hitRate
  df_for_regs$mfc_theta_power[a + 2] <- main_df$incong_correct_theta_power_mfc250[i] #correct_mfc_theta_power
  df_for_regs$mfc_theta_itps[a + 2] <- main_df$incong_correct_theta_ITPS_mfc250[i] #correct_mfc_theta_itps
  df_for_regs$accuracy[a + 2] <- 1 # correct
  df_for_regs$scaared_social[a + 2] <- main_df$scaared_b_scrdSoc_s1_r1_e1[i] # scaared_social

  a <- a + 2
}
df_for_regs[,1] <- as.factor(df_for_regs[,1]) # id
df_for_regs[,5] <- as.factor(df_for_regs[,5]) # accuracy

contrasts(df_for_regs[,5]) <- contr.sum(2) # convert accuracy to sum contrasts

# 1: predicting hit rate, with accuracy and scared-social as predictors
reg1_model <- lmer(hitRate ~ scaared_social + accuracy + scaared_social:accuracy + (1 | id), data = df_for_regs)
summary(reg1_model, type = III)
# plot
sjPlot::plot_model(reg1_model, type = "pred", terms = c("scaared_social", "accuracy")) # significant

# 2: predicting MFC cluster theta power, with accuracy and scared-social as predictors
reg2_model <- lmer(mfc_theta_power ~ scaared_social + accuracy + scaared_social:accuracy + (1 | id), data = df_for_regs)
summary(reg2_model, type = III)
# plot
sjPlot::plot_model(reg2_model, type = "pred", terms = c("scaared_social", "accuracy"))
sjPlot::plot_model(reg2_model, type = "pred", terms = c("accuracy")) # significant

# 3: predicting MFC cluster theta itps, with accuracy and scared-social as predictors
reg3_model <- lmer(mfc_theta_itps ~ scaared_social + accuracy + scaared_social:accuracy + (1 | id), data = df_for_regs)
summary(reg3_model, type = III)
# plot
sjPlot::plot_model(reg3_model, type = "pred", terms = c("scaared_social", "accuracy"))
sjPlot::plot_model(reg3_model, type = "pred", terms = c("scaared_social")) # significant

# 4: predicting MFC cluster theta itps, with accuracy and scared-social as predictors
reg4_model <- lmer(hitRate ~ mfc_theta_power + accuracy + mfc_theta_power:accuracy + (1 | id), data = df_for_regs)
summary(reg4_model, type = III)
# plot
sjPlot::plot_model(reg4_model, type = "pred", terms = c("mfc_theta_power", "accuracy"))

# 5: predicting hit rate, with accuracy and MFC cluster theta itps as predictors
reg5_model <- lmer(hitRate ~ mfc_theta_itps + accuracy + mfc_theta_itps:accuracy + (1 | id), data = df_for_regs)
summary(reg5_model, type = III)
# plot
sjPlot::plot_model(reg5_model, type = "pred", terms = c("mfc_theta_itps", "accuracy"))


########################################################################################
# Dataset below will be used for regression 6!
# This is a mixed-effects model as it is like a repeated measures ANOVA.
# lateral_frontal = 1 ; posterior = 0
row_n <- nrow(main_df) * 4
df_for_reg6 <- data.frame(id= rep(NA, row_n), hitRate = rep(NA, row_n), brain_region = rep(NA, row_n), wPLI_value = rep(NA, row_n), accuracy = rep(NA, row_n), scaared_social = rep(NA, row_n))
a <- 0
for (i in 1:nrow(main_df)){

  df_for_reg6$id[a + 1] <- main_df$participant_id[i]
  df_for_reg6$hitRate[a + 1] <- main_df$error_hitRate[i] #error_hitrate
  df_for_reg6$brain_region[a + 1] <- 1
  df_for_reg6$wPLI_value[a + 1] <- main_df$incong_error_theta_wPLI_latfrontal250[i] #error_latfrontal
  df_for_reg6$accuracy[a + 1] <- 0 # error
  df_for_reg6$scaared_social[a + 1] <- main_df$scaared_b_scrdSoc_s1_r1_e1[i]

  df_for_reg6$id[a + 2] <- main_df$participant_id[i]
  df_for_reg6$hitRate[a + 2] <- main_df$correct_hitRate[i] #correct_hitRate
  df_for_reg6$brain_region[a + 2] <- 1
  df_for_reg6$wPLI_value[a + 2] <- main_df$incong_correct_theta_wPLI_latfrontal250[i] #correct_latfrontal
  df_for_reg6$accuracy[a + 2] <- 1 # correct
  df_for_reg6$scaared_social[a + 2] <- main_df$scaared_b_scrdSoc_s1_r1_e1[i]

  df_for_reg6$id[a + 3] <- main_df$participant_id[i]
  df_for_reg6$hitRate[a + 3] <- main_df$error_hitRate[i] #error_hitrate
  df_for_reg6$brain_region[a + 3] <- 0
  df_for_reg6$wPLI_value[a + 3] <- main_df$incong_error_theta_wPLI_posterior250[i] #error_posterior
  df_for_reg6$accuracy[a + 3] <- 0 # error
  df_for_reg6$scaared_social[a + 3] <- main_df$scaared_b_scrdSoc_s1_r1_e1[i]

  df_for_reg6$id[a + 4] <- main_df$participant_id[i]
  df_for_reg6$hitRate[a + 4] <- main_df$correct_hitRate[i] #correct_hitRate
  df_for_reg6$brain_region[a + 4] <- 0
  df_for_reg6$wPLI_value[a + 4] <- main_df$incong_correct_theta_wPLI_posterior250[i] #correct_posterior
  df_for_reg6$accuracy[a + 4] <- 1 # correct
  df_for_reg6$scaared_social[a + 4] <- main_df$scaared_b_scrdSoc_s1_r1_e1[i]

  a <- a + 4
}
df_for_reg6[,1] <- as.factor(df_for_reg6[,1]) # id
df_for_reg6[,3] <- as.factor(df_for_reg6[,3]) # brain_region
df_for_reg6[,5] <- as.factor(df_for_reg6[,5]) # accuracy

contrasts(df_for_reg6[,5]) <- contr.sum(2) # convert accuracy to sum contrasts
contrasts(df_for_reg6[,3]) <- contr.sum(2)

# 6: predicting hit rate, with accuracy, anterior-posterior, and wpli as predictors
reg6_model <- lmer(hitRate ~ brain_region + accuracy + wPLI_value +  brain_region:accuracy:wPLI_value + (1 | id), data = df_for_reg6)
summary(reg6_model, type = III)
# plot
sjPlot::plot_model(reg6_model, type = "pred", terms = c("brain_region", "accuracy", "wPLI_value"))
##########################################
# dataset for 7
# predicting hit rate, with accuracy, and wpli (posterior) as predictors
row_n <- nrow(main_df) * 2
df_for_reg7 <- data.frame(id= rep(NA, row_n), hitRate = rep(NA, row_n), wPLI_value = rep(NA, row_n), accuracy = rep(NA, row_n))
a <- 0
for (i in 1:nrow(main_df)){

  df_for_reg7$id[a + 1] <- main_df$participant_id[i]
  df_for_reg7$hitRate[a + 1] <- main_df$error_hitRate[i] #error_hitrate
  df_for_reg7$wPLI_value[a + 1] <- main_df$incong_error_theta_wPLI_posterior250[i] #error_latfrontal
  df_for_reg7$accuracy[a + 1] <- 0 # error

  df_for_reg7$id[a + 2] <- main_df$participant_id[i]
  df_for_reg7$hitRate[a + 2] <- main_df$correct_hitRate[i] #correct_hitRate
  df_for_reg7$wPLI_value[a + 2] <- main_df$incong_correct_theta_wPLI_posterior250[i] #correct_latfrontal
  df_for_reg7$accuracy[a + 2] <- 1 # correct

  a <- a + 2
}
df_for_reg7[,1] <- as.factor(df_for_reg7[,1]) # id
df_for_reg7[,4] <- as.factor(df_for_reg7[,4]) # accuracy

contrasts(df_for_reg7[,4]) <- contr.sum(2) # convert accuracy to sum contrasts

contrasts(df_for_reg7[,4])

reg7_model <- lmer(hitRate ~ accuracy + wPLI_value + accuracy:wPLI_value + (1 | id), data = df_for_reg7)
summary(reg7_model, type = III)
# plot
sjPlot::plot_model(reg7_model, type = "pred", terms = c("accuracy", "wPLI_value"))
########
# predicting posterior wPLI with accuracy and social scaared

reg8_model <- lmer(wPLI_value ~  accuracy + scaared_social +  accuracy:scaared_social + (1 | id), data = df_for_reg6)
summary(reg8_model, type = III)
# plot
sjPlot::plot_model(reg8_model, type = "pred", terms = c("scaared_social", "accuracy" ))
###########
reg9_model <- lmer(scaared_social ~  brain_region*accuracy*wPLI_value + (1 | id), data = df_for_reg6)
summary(reg9_model, type = III)
# plot
sjPlot::plot_model(reg9_model, type = "pred", terms = c("scaared_social", "accuracy" ))
###########

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)


