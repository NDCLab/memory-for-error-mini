# This script will run stats on mini_mfe data.
# Author: Kianoosh Hosseini at NDCLab @FIU (https://Kianoosh.info; https://NDClab.com)
# Last Update: 2023-03-01 (YYYY-MM-DD)

library(tidyverse)
library(dplyr)
library(stringr)
library(psycho)

#Working directory should be the Psychopy experiment directory.

proje_wd <- "/Users/kihossei/Documents/GitHub/memory-for-error-mini/materials/task/mini_mfe"
setwd(proje_wd)

processed_file_input <- paste(proje_wd, "stat_output", sep ="/", collapse = NULL) # input data directory

main_df <-  read.csv(file = paste(processed_file_input, "processed_data_mini_mfe_Proj.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))


# Convert mainDat with specified columns to long format. So, I can use ggplot, etc. easily.

longDat_sdt_error <- gather(main_df, column_name, value, error_hitRate, pre_error_hitRate, post_error_hitRate)

longDat_sdt_correct <- gather(main_df, column_name, value, correct_hitRate, pre_correct_hitRate, post_correct_hitRate)

longDat_hitRate <- gather(main_df, column_name, value, pre_error_hitRate, error_hitRate, post_error_hitRate, pre_correct_hitRate, correct_hitRate, post_correct_hitRate)



# SDT plots
for_plot_sdt_corr <- longDat_sdt_correct %>%
  drop_na(value) %>%
  group_by(column_name) %>%
  summarise(
    n=n(),
    mean=mean(as.numeric(value), na.rm = TRUE),
    sd=sd(as.numeric(value), na.rm = TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))
ggplot(for_plot_sdt_corr) +
  geom_bar( aes(x= column_name, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=column_name, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  ggtitle("sdt_correct")

for_plot_sdt_err <- longDat_sdt_error %>%
  drop_na(value) %>%
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

for_plot_sdt_err_corr <- longDat_hitRate %>%
  drop_na(value) %>%
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

####################################################

# Stats

# hit Rate
t.test(main_df$post_correct_hitRate, main_df$post_error_hitRate, paired = TRUE, na.action = na.omit)
t.test(main_df$error_hitRate, main_df$correct_hitRate, paired = TRUE, na.action = na.omit)
t.test(main_df$error_hitRate, main_df$post_error_hitRate, paired = TRUE, na.action = na.omit)
t.test(main_df$correct_hitRate, main_df$post_correct_hitRate, paired = TRUE, na.action = na.omit)
t.test(main_df$incong_error_theta_power, main_df$incong_correct_theta_power, paired = TRUE, na.action = na.omit)
t.test(main_df$incong_error_ITPS, main_df$incong_correct_ITPS, paired = TRUE, na.action = na.omit)


# d'
t.test(main_df$post_d_prime_correct, main_df$post_d_prime_error, paired = TRUE, na.action = na.omit)
t.test(main_df$d_prime_error, main_df$d_prime_correct, paired = TRUE, na.action = na.omit)
t.test(main_df$d_prime_error, main_df$post_d_prime_error, paired = TRUE, na.action = na.omit)
t.test(main_df$d_prime_correct, main_df$post_d_prime_correct, paired = TRUE, na.action = na.omit)

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), method = 'pearson', na.action = na.omit)
# Running correlation tests and plotting scatter plots
# wPLI 250 21_25 exploratory elecs!
t.test(main_df$incong_error_theta_wPLI_21_25_250, main_df$incong_correct_theta_wPLI_21_25_250, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_wPLI_21_25_250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_wPLI_21_25_250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_wPLI_21_25_250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_wPLI_21_25_250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_wPLI_21_25_250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_wPLI_21_25_250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_wPLI_21_25_250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_wPLI_21_25_250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_wPLI_21_25_250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_wPLI_21_25_250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_wPLI_21_25_250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_wPLI_21_25_250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_wPLI_21_25_250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_wPLI_21_25_250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_wPLI_21_25_250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_wPLI_21_25_250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_wPLI_21_25_250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_wPLI_21_25_250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_wPLI_21_25_250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_wPLI_21_25_250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_wPLI_21_25_250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_wPLI_21_25_250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_wPLI_21_25_250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_wPLI_21_25_250)) + geom_point() #scatterplot

# Running correlation tests and plotting scatter plots
# itps 250 3_18_20 exploratory elecs!
t.test(main_df$incong_error_theta_itps_3_18_20_250, main_df$incong_correct_theta_itps_3_18_20_250, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_itps_3_18_20_250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_itps_3_18_20_250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_itps_3_18_20_250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_itps_3_18_20_250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_itps_3_18_20_250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_itps_3_18_20_250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_itps_3_18_20_250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_itps_3_18_20_250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_itps_3_18_20_250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_itps_3_18_20_250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_itps_3_18_20_250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_itps_3_18_20_250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_itps_3_18_20_250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_itps_3_18_20_250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_itps_3_18_20_250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_itps_3_18_20_250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_itps_3_18_20_250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit) # p-value = 0.03149
cor.test(as.numeric(main_df$incong_error_theta_itps_3_18_20_250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_itps_3_18_20_250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_itps_3_18_20_250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_itps_3_18_20_250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_itps_3_18_20_250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_itps_3_18_20_250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_itps_3_18_20_250)) + geom_point() #scatterplot

# Running correlation tests and plotting scatter plots
# theta power 250 3_18_20 exploratory elecs!
t.test(main_df$incong_error_theta_power_3_18_20_250, main_df$incong_correct_theta_power_3_18_20_250, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_3_18_20_250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_3_18_20_250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_3_18_20_250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power_3_18_20_250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power_3_18_20_250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power_3_18_20_250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_power_3_18_20_250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_3_18_20_250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_3_18_20_250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_power_3_18_20_250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_power_3_18_20_250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_power_3_18_20_250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_power_3_18_20_250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_3_18_20_250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_3_18_20_250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_3_18_20_250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power_3_18_20_250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power_3_18_20_250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_power_3_18_20_250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_power_3_18_20_250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_power_3_18_20_250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_power_3_18_20_250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_power_3_18_20_250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_power_3_18_20_250)) + geom_point() #scatterplot


# Running correlation tests and plotting scatter plots
# wPLI 250 latfrontal
t.test(main_df$incong_error_theta_wPLI_latfrontal250, main_df$incong_correct_theta_wPLI_latfrontal250, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_wPLI_latfrontal250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_wPLI_latfrontal250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_wPLI_latfrontal250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_wPLI_latfrontal250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_wPLI_latfrontal250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_wPLI_latfrontal250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_wPLI_latfrontal250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_wPLI_latfrontal250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_wPLI_latfrontal250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_wPLI_latfrontal250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_wPLI_latfrontal250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_wPLI_latfrontal250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_wPLI_latfrontal250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_wPLI_latfrontal250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_wPLI_latfrontal250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_wPLI_latfrontal250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit) # p-value = 0.03903
cor.test(as.numeric(main_df$incong_error_theta_wPLI_latfrontal250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_wPLI_latfrontal250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_wPLI_latfrontal250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_wPLI_latfrontal250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_wPLI_latfrontal250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_wPLI_latfrontal250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_wPLI_latfrontal250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_wPLI_latfrontal250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$alpha_wPLI_latfrontal250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_wPLI_latfrontal250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_wPLI_latfrontal250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_wPLI_latfrontal250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_wPLI_latfrontal250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_wPLI_latfrontal250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=alpha_wPLI_latfrontal250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=alpha_wPLI_latfrontal250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=alpha_wPLI_latfrontal250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=alpha_wPLI_latfrontal250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_alpha_wPLI_latfrontal250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_alpha_wPLI_latfrontal250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_alpha_wPLI_latfrontal250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_wPLI_latfrontal250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_wPLI_latfrontal250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_wPLI_latfrontal250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_wPLI_latfrontal250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_wPLI_latfrontal250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$alpha_wPLI_latfrontal250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_wPLI_latfrontal250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_wPLI_latfrontal250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_wPLI_latfrontal250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_wPLI_latfrontal250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_wPLI_latfrontal250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

# wPLI 250 posterior
t.test(main_df$incong_error_theta_wPLI_posterior250, main_df$incong_correct_theta_wPLI_posterior250, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_wPLI_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_wPLI_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_wPLI_posterior250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_wPLI_posterior250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_wPLI_posterior250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_wPLI_posterior250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit) # p-value = 0.02054
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_wPLI_posterior250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_wPLI_posterior250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_wPLI_posterior250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_wPLI_posterior250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit) # p-value = 0.03719
cor.test(as.numeric(main_df$theta_wPLI_posterior250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_wPLI_posterior250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_wPLI_posterior250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_wPLI_posterior250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_wPLI_posterior250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_wPLI_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_wPLI_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_wPLI_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_wPLI_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_wPLI_posterior250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_wPLI_posterior250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$alpha_wPLI_posterior250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_wPLI_posterior250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_wPLI_posterior250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_wPLI_posterior250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_wPLI_posterior250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_wPLI_posterior250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=alpha_wPLI_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=alpha_wPLI_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=alpha_wPLI_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=alpha_wPLI_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_alpha_wPLI_posterior250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_alpha_wPLI_posterior250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_alpha_wPLI_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_wPLI_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_wPLI_posterior250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_wPLI_posterior250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_wPLI_posterior250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_wPLI_posterior250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$alpha_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_wPLI_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_wPLI_posterior250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_wPLI_posterior250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_wPLI_posterior250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


#################################################################### wPLI end
t.test(main_df$incong_error_theta_power_mfc250, main_df$incong_correct_theta_power_mfc250, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power_mfc250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power_mfc250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power_mfc250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

t.test(main_df$incong_error_theta_ITPS_mfc250, main_df$incong_correct_theta_ITPS_mfc250, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_mfc250), method = 'pearson', na.action = na.omit) # p-value = 0.03529
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_correct_theta_ITPS_mfc250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_mfc250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_mfc250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_ITPS_mfc250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_ITPS_mfc250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_ITPS_mfc250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_power_mfc250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_mfc250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_mfc250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_power_mfc250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_power_mfc250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_power_mfc250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_ITPS_mfc250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_ITPS_mfc250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_ITPS_mfc250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


################################################################
t.test(main_df$incong_error_theta_power_mfc500, main_df$incong_correct_theta_power_mfc500, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc500), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power_mfc500, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power_mfc500, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power_mfc500, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

t.test(main_df$incong_error_theta_ITPS_mfc500, main_df$incong_correct_theta_ITPS_mfc500, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_mfc500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_mfc500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_mfc500), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_ITPS_mfc500, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_ITPS_mfc500, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_ITPS_mfc500, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_power_mfc500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_mfc500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_mfc500_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_power_mfc500_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_power_mfc500_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_power_mfc500_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_ITPS_mfc500_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_ITPS_mfc500_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_ITPS_mfc500_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_ITPS_mfc500_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

#############################################################
t.test(main_df$incong_error_theta_power_posterior250, main_df$incong_correct_theta_power_posterior250, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power_posterior250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power_posterior250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power_posterior250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

t.test(main_df$incong_error_theta_ITPS_posterior250, main_df$incong_correct_theta_ITPS_posterior250, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior250), method = 'pearson', na.action = na.omit) #p-value = 0.04115
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_correct_theta_ITPS_posterior250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_ITPS_posterior250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_ITPS_posterior250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_ITPS_posterior250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_power_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_power_posterior250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_power_posterior250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_power_posterior250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_ITPS_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_ITPS_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_ITPS_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_ITPS_posterior250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_ITPS_posterior250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_ITPS_posterior250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


################################################################
t.test(main_df$incong_error_theta_power_posterior500, main_df$incong_correct_theta_power_posterior500, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior500), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power_posterior500, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power_posterior500, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power_posterior500, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

t.test(main_df$incong_error_theta_ITPS_posterior500, main_df$incong_correct_theta_ITPS_posterior500, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior500), method = 'pearson', na.action = na.omit) # p-value = 0.04834
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_correct_theta_ITPS_posterior500), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_ITPS_posterior500, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_ITPS_posterior500, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_ITPS_posterior500, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_power_posterior500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_posterior500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_posterior500_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_power_posterior500_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_power_posterior500_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_power_posterior500_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_ITPS_posterior500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_ITPS_posterior500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_ITPS_posterior500_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_ITPS_posterior500_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_ITPS_posterior500_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_ITPS_posterior500_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

############################### alpha corr with anxiety
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_mfc250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_mfc250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_mfc250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_alpha_power_mfc250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_alpha_power_mfc250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_alpha_power_mfc250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_mfc250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_mfc250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_mfc250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_alpha_ITPS_mfc250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_alpha_ITPS_mfc250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_alpha_ITPS_mfc250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$alpha_power_mfc250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_power_mfc250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_power_mfc250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=alpha_power_mfc250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=alpha_power_mfc250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=alpha_power_mfc250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$alpha_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_ITPS_mfc250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=alpha_ITPS_mfc250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=alpha_ITPS_mfc250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=alpha_ITPS_mfc250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


################################################################
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_mfc500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_mfc500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_mfc500), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_alpha_power_mfc500, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_alpha_power_mfc500, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_alpha_power_mfc500, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_mfc500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_mfc500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_mfc500), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_alpha_ITPS_mfc500, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_alpha_ITPS_mfc500, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_alpha_ITPS_mfc500, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$alpha_power_mfc500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_power_mfc500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_power_mfc500_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=alpha_power_mfc500_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=alpha_power_mfc500_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=alpha_power_mfc500_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$alpha_ITPS_mfc500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_ITPS_mfc500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_ITPS_mfc500_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=alpha_ITPS_mfc500_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=alpha_ITPS_mfc500_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=alpha_ITPS_mfc500_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

#############################################################

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_posterior250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_alpha_power_posterior250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_alpha_power_posterior250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_alpha_power_posterior250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_posterior250), method = 'pearson', na.action = na.omit) # p-value = 0.01821
ggplot(main_df, aes(x=incong_error_alpha_ITPS_posterior250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_alpha_ITPS_posterior250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_alpha_ITPS_posterior250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$alpha_power_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_power_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_power_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=alpha_power_posterior250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=alpha_power_posterior250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=alpha_power_posterior250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$alpha_ITPS_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_ITPS_posterior250_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_ITPS_posterior250_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=alpha_ITPS_posterior250_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=alpha_ITPS_posterior250_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=alpha_ITPS_posterior250_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


################################################################
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_posterior500), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_alpha_power_posterior500, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_alpha_power_posterior500, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_alpha_power_posterior500, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_posterior500), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_alpha_ITPS_posterior500, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_alpha_ITPS_posterior500, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_alpha_ITPS_posterior500, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$alpha_power_posterior500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_power_posterior500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_power_posterior500_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=alpha_power_posterior500_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=alpha_power_posterior500_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=alpha_power_posterior500_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$alpha_ITPS_posterior500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_ITPS_posterior500_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$alpha_ITPS_posterior500_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=alpha_ITPS_posterior500_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=alpha_ITPS_posterior500_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=alpha_ITPS_posterior500_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot


############################################################################
############################################################################
############################################################################


t.test(main_df$incong_error_theta_power, main_df$incong_correct_theta_power, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_power), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

t.test(main_df$incong_error_ITPS, main_df$incong_correct_ITPS, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_ITPS), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_ITPS), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_ITPS), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_ITPS, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_ITPS, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_ITPS, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$theta_power_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$theta_power_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=theta_power_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=theta_power_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=theta_power_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$ITPS_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$ITPS_difference_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$ITPS_difference_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=ITPS_difference_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=ITPS_difference_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=ITPS_difference_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

t.test(main_df$ERN, main_df$CRN, paired = TRUE, na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$ERN), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$ERN), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$ERN), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=ERN, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=ERN, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=ERN, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$deltaERN), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$deltaERN), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$deltaERN), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=deltaERN, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=deltaERN, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=deltaERN, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$deltaPEA1), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$deltaPEA1), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$deltaPEA1), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=deltaPEA1, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=deltaPEA1, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=deltaPEA1, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$deltaPEA2), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$deltaPEA2), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$deltaPEA2), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=deltaPEA2, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=deltaPEA2, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=deltaPEA2, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$deltaPEB2), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$deltaPEB2), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$deltaPEB2), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=deltaPEB2, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=deltaPEB2, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=deltaPEB2, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$deltaPEB3), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$deltaPEB3), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$deltaPEB3), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=deltaPEB3, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=deltaPEB3, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=deltaPEB3, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=memoryBias_score, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=memoryBias_score, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=memoryBias_score, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

# Correlations with hit Rate
# beta500
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_beta_power2000), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_beta_power2000), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_beta_power2000), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$incong_error_beta_power500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power500), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power500), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=incong_error_beta_power500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=incong_error_beta_power500)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_beta_power500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=incong_error_beta_power500)) + geom_point()

cor.test(as.numeric(main_df$incong_correct_beta_power500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power500), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power500), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=incong_correct_beta_power500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=incong_correct_beta_power500)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=incong_correct_beta_power500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=incong_correct_beta_power500)) + geom_point()

cor.test(as.numeric(main_df$beta_power_difference_score500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score500), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score500), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=beta_power_difference_score500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=beta_power_difference_score500)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=beta_power_difference_score500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=beta_power_difference_score500)) + geom_point()



# beta1000
cor.test(as.numeric(main_df$incong_error_beta_power1000), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power1000), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power1000), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power1000), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=incong_error_beta_power1000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=incong_error_beta_power1000)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_beta_power1000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=incong_error_beta_power1000)) + geom_point()

cor.test(as.numeric(main_df$incong_correct_beta_power1000), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power1000), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power1000), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power1000), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=incong_correct_beta_power1000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=incong_correct_beta_power1000)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=incong_correct_beta_power1000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=incong_correct_beta_power1000)) + geom_point()

cor.test(as.numeric(main_df$beta_power_difference_score1000), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score1000), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score1000), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score1000), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=beta_power_difference_score1000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=beta_power_difference_score1000)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=beta_power_difference_score1000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=beta_power_difference_score1000)) + geom_point()

# beta1500
cor.test(as.numeric(main_df$incong_error_beta_power1500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power1500), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power1500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power1500), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=incong_error_beta_power1500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=incong_error_beta_power1500)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_beta_power1500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=incong_error_beta_power1500)) + geom_point()

cor.test(as.numeric(main_df$incong_correct_beta_power1500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power1500), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power1500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power1500), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=incong_correct_beta_power1500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=incong_correct_beta_power1500)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=incong_correct_beta_power1500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=incong_correct_beta_power1500)) + geom_point()

cor.test(as.numeric(main_df$beta_power_difference_score1500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score1500), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score1500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score1500), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=beta_power_difference_score1500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=beta_power_difference_score1500)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=beta_power_difference_score1500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=beta_power_difference_score1500)) + geom_point()


# beta2000
cor.test(as.numeric(main_df$incong_error_beta_power2000), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power2000), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power2000), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_beta_power2000), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=incong_error_beta_power2000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=incong_error_beta_power2000)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_beta_power2000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=incong_error_beta_power2000)) + geom_point()

cor.test(as.numeric(main_df$incong_correct_beta_power2000), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power2000), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power2000), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_correct_beta_power2000), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=incong_correct_beta_power2000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=incong_correct_beta_power2000)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=incong_correct_beta_power2000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=incong_correct_beta_power2000)) + geom_point()

cor.test(as.numeric(main_df$beta_power_difference_score2000), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score2000), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score2000), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$beta_power_difference_score2000), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=beta_power_difference_score2000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=beta_power_difference_score2000)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=beta_power_difference_score2000)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=beta_power_difference_score2000)) + geom_point()










########
# Theta
cor.test(as.numeric(main_df$theta_power_mfc250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_mfc250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_mfc250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_mfc250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power_mfc250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power_mfc250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_power_mfc250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_power_mfc250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_power_mfc250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_power_mfc250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_power_mfc250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_power_mfc250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_power_mfc500_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_mfc500_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_mfc500_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_mfc500_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power_mfc500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power_mfc500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_power_mfc500_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_power_mfc500_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_power_mfc500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_power_mfc500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_power_mfc500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_power_mfc500)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_ITPS_mfc250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_mfc250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_mfc250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_mfc250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_ITPS_mfc250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_ITPS_mfc250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_ITPS_mfc250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_ITPS_mfc250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_ITPS_mfc250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_ITPS_mfc250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_ITPS_mfc250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_ITPS_mfc250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_ITPS_mfc500_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_mfc500_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_mfc500_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_mfc500_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_ITPS_mfc500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_ITPS_mfc500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_ITPS_mfc500_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_ITPS_mfc500_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_ITPS_mfc500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_ITPS_mfc500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_ITPS_mfc500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_ITPS_mfc500)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_power_posterior250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_posterior250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_posterior250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_posterior250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit) # p-value = 0.006977
cor.test(as.numeric(main_df$incong_error_theta_power_posterior250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power_posterior250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_power_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_power_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_power_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_power_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_power_posterior250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_power_posterior250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_power_posterior500_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_posterior500_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_posterior500_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_posterior500_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power_posterior500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power_posterior500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_power_posterior500_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_power_posterior500_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_power_posterior500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_power_posterior500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_power_posterior500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_power_posterior500)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_ITPS_posterior250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_posterior250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_posterior250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_posterior250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_ITPS_posterior250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_ITPS_posterior250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_ITPS_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_ITPS_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_ITPS_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_ITPS_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_ITPS_posterior250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_ITPS_posterior250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$theta_ITPS_posterior500_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_posterior500_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_posterior500_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_ITPS_posterior500_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_ITPS_posterior500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_ITPS_posterior500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_ITPS_posterior500_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_ITPS_posterior500_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_ITPS_posterior500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_ITPS_posterior500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_ITPS_posterior500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_ITPS_posterior500)) + geom_point() #scatterplot



###################
cor.test(as.numeric(main_df$alpha_power_mfc250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_power_mfc250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_power_mfc250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_power_mfc250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_power_mfc250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_power_mfc250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=alpha_power_mfc250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=alpha_power_mfc250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=alpha_power_mfc250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=alpha_power_mfc250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_alpha_power_mfc250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_alpha_power_mfc250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$alpha_power_mfc500_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_power_mfc500_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_power_mfc500_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_power_mfc500_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_power_mfc500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_power_mfc500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=alpha_power_mfc500_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=alpha_power_mfc500_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=alpha_power_mfc500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=alpha_power_mfc500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_alpha_power_mfc500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_alpha_power_mfc500)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$alpha_ITPS_mfc250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_mfc250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_mfc250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_mfc250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_ITPS_mfc250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_ITPS_mfc250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=alpha_ITPS_mfc250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=alpha_ITPS_mfc250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=alpha_ITPS_mfc250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=alpha_ITPS_mfc250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_alpha_ITPS_mfc250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_alpha_ITPS_mfc250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$alpha_ITPS_mfc500_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_mfc500_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_mfc500_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_mfc500_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_ITPS_mfc500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_ITPS_mfc500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=alpha_ITPS_mfc500_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=alpha_ITPS_mfc500_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=alpha_ITPS_mfc500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=alpha_ITPS_mfc500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_alpha_ITPS_mfc500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_alpha_ITPS_mfc500)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$alpha_power_posterior250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_power_posterior250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit) # p-value = 0.0244
cor.test(as.numeric(main_df$alpha_power_posterior250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit) # p-value = 0.03941
cor.test(as.numeric(main_df$alpha_power_posterior250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_power_posterior250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_power_posterior250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=alpha_power_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=alpha_power_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=alpha_power_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=alpha_power_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_alpha_power_posterior250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_alpha_power_posterior250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$alpha_power_posterior500_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_power_posterior500_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_power_posterior500_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_power_posterior500_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_power_posterior500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_power_posterior500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=alpha_power_posterior500_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=alpha_power_posterior500_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=alpha_power_posterior500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=alpha_power_posterior500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_alpha_power_posterior500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_alpha_power_posterior500)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$alpha_ITPS_posterior250_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_posterior250_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit) #p-value = 0.01837
cor.test(as.numeric(main_df$alpha_ITPS_posterior250_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_posterior250_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_ITPS_posterior250), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_ITPS_posterior250), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=alpha_ITPS_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=alpha_ITPS_posterior250_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=alpha_ITPS_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=alpha_ITPS_posterior250_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_alpha_ITPS_posterior250)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_alpha_ITPS_posterior250)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$alpha_ITPS_posterior500_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_posterior500_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_posterior500_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$alpha_ITPS_posterior500_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_ITPS_posterior500), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_alpha_ITPS_posterior500), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=alpha_ITPS_posterior500_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=alpha_ITPS_posterior500_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=alpha_ITPS_posterior500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=alpha_ITPS_posterior500_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_alpha_ITPS_posterior500)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_alpha_ITPS_posterior500)) + geom_point() #scatterplot

#######################

cor.test(as.numeric(main_df$theta_power_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=theta_power_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=theta_power_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=theta_power_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=theta_power_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_theta_power)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_theta_power)) + geom_point() #scatterplot


cor.test(as.numeric(main_df$ITPS_difference_score), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$ITPS_difference_score), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$ITPS_difference_score), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$ITPS_difference_score), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_ITPS), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_ITPS), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=ITPS_difference_score)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=ITPS_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=ITPS_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=ITPS_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=incong_error_ITPS)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=incong_error_ITPS)) + geom_point() #scatterplot



cor.test(as.numeric(main_df$deltaERN), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaERN), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaERN), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaERN), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$ERN), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$ERN), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=deltaERN)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=deltaERN)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=deltaERN)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=deltaERN)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=ERN)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=ERN)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$deltaPEA1), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEA1), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEA1), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEA1), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$PEA1_error), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$PEA1_error), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=deltaPEA1)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=deltaPEA1)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=deltaPEA1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=deltaPEA1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=PEA1_error)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=PEA1_error)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$deltaPEA2), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEA2), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEA2), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEA2), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$PEA2_error), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$PEA2_error), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=deltaPEA2)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=deltaPEA2)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=deltaPEA2)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=deltaPEA2)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=PEA2_error)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=PEA2_error)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$deltaPEB3), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEB3), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEB3), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEB3), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$PEB3_error), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$PEB3_error), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=deltaPEB3)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=deltaPEB3)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=deltaPEB3)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=deltaPEB3)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=PEB3_error)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=PEB3_error)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$deltaPEB2), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEB2), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEB2), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$deltaPEB2), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$PEB2_error), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$PEB2_error), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=deltaPEB2)) + geom_point()
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=deltaPEB2)) + geom_point()
ggplot(main_df, aes(x=post_error_hitRate, y=deltaPEB2)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=deltaPEB2)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=PEB2_error)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=PEB2_error)) + geom_point() #scatterplot


cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$hitRate_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_post_error_minus_correct, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$hitRate_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=hitRate_error_minus_correct, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$post_error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=post_error_hitRate, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_error_hitRate, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$error_hitRate), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=error_hitRate, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=error_hitRate, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$error_hitRate), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$post_error_hitRate), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$hitRate_error_minus_correct), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$hitRate_post_error_minus_correct), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=memoryBias_score, y=error_hitRate)) + geom_point() #scatterplot
ggplot(main_df, aes(x=memoryBias_score, y=post_error_hitRate)) + geom_point() #scatterplot
ggplot(main_df, aes(x=memoryBias_score, y=hitRate_error_minus_correct)) + geom_point()
ggplot(main_df, aes(x=memoryBias_score, y=hitRate_post_error_minus_correct)) + geom_point(size = 4) #scatterplot

# Correlations with d'
cor.test(as.numeric(main_df$theta_power_difference_score), as.numeric(main_df$d_prime_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_difference_score), as.numeric(main_df$d_prime_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_difference_score), as.numeric(main_df$post_d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$theta_power_difference_score), as.numeric(main_df$d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power), as.numeric(main_df$d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power), as.numeric(main_df$post_d_prime_error), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=d_prime_error_minus_correct, y=theta_power_difference_score)) + geom_point()
ggplot(main_df, aes(x=d_prime_post_error_minus_correct, y=theta_power_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_d_prime_error, y=theta_power_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=d_prime_error, y=theta_power_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=d_prime_error, y=incong_error_theta_power)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_d_prime_error, y=incong_error_theta_power)) + geom_point() #scatterplot


cor.test(as.numeric(main_df$ITPS_difference_score), as.numeric(main_df$d_prime_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$ITPS_difference_score), as.numeric(main_df$d_prime_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$ITPS_difference_score), as.numeric(main_df$post_d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$ITPS_difference_score), as.numeric(main_df$d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_ITPS), as.numeric(main_df$d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_ITPS), as.numeric(main_df$post_d_prime_error), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=d_prime_error_minus_correct, y=ITPS_difference_score)) + geom_point()
ggplot(main_df, aes(x=d_prime_post_error_minus_correct, y=ITPS_difference_score)) + geom_point()
ggplot(main_df, aes(x=post_d_prime_error, y=ITPS_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=d_prime_error, y=ITPS_difference_score)) + geom_point() #scatterplot
ggplot(main_df, aes(x=d_prime_error, y=incong_error_ITPS)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_d_prime_error, y=incong_error_ITPS)) + geom_point() #scatterplot


cor.test(as.numeric(main_df$theta_power_difference_score), as.numeric(main_df$d_prime_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_theta_power), as.numeric(main_df$d_prime_error), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=d_prime_error_minus_correct, y=theta_power_difference_score)) + geom_point()
ggplot(main_df, aes(x=d_prime_error, y=incong_error_theta_power)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$ITPS_difference_score), as.numeric(main_df$d_prime_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_ITPS), as.numeric(main_df$d_prime_error), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=d_prime_error_minus_correct, y=ITPS_difference_score)) + geom_point()
ggplot(main_df, aes(x=d_prime_error, y=incong_error_ITPS)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$ITPS_difference_score), as.numeric(main_df$post_d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$incong_error_ITPS), as.numeric(main_df$post_d_prime_error), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=d_prime_error_minus_correct, y=ITPS_difference_score)) + geom_point()
ggplot(main_df, aes(x=d_prime_error, y=incong_error_ITPS)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$deltaERN), as.numeric(main_df$d_prime_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$ERN), as.numeric(main_df$d_prime_error), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=d_prime_error_minus_correct, y=deltaERN)) + geom_point()
ggplot(main_df, aes(x=d_prime_error, y=ERN)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$d_prime_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$d_prime_post_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$d_prime_post_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=d_prime_post_error_minus_correct, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=d_prime_post_error_minus_correct, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=d_prime_post_error_minus_correct, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$d_prime_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$d_prime_error_minus_correct), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$d_prime_error_minus_correct), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=d_prime_error_minus_correct, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=d_prime_error_minus_correct, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=d_prime_error_minus_correct, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$post_d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$post_d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$post_d_prime_error), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=post_d_prime_error, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_d_prime_error, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=post_d_prime_error, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$d_prime_error), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$d_prime_error), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=d_prime_error, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=d_prime_error, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=d_prime_error, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$d_prime_error), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$post_d_prime_error), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$d_prime_error_minus_correct), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$d_prime_post_error_minus_correct), as.numeric(main_df$memoryBias_score), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=memoryBias_score, y=d_prime_error)) + geom_point() #scatterplot
ggplot(main_df, aes(x=memoryBias_score, y=post_d_prime_error)) + geom_point() #scatterplot
ggplot(main_df, aes(x=memoryBias_score, y=d_prime_error_minus_correct)) + geom_point()
ggplot(main_df, aes(x=memoryBias_score, y=d_prime_post_error_minus_correct)) + geom_point(size = 4) #scatterplot
