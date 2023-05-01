


cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power_mfc250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power_mfc250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power_mfc250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_mfc250), method = 'pearson', na.action = na.omit)
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
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_mfc500), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power_mfc500, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power_mfc500, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power_mfc500, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

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

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior250), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power_posterior250, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power_posterior250, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power_posterior250, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior250), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior250), method = 'pearson', na.action = na.omit)
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
cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_power_posterior500), method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=incong_error_theta_power_posterior500, y=scaared_b_scrdSoc_s1_r1_e1)) + geom_point()
ggplot(main_df, aes(x=incong_error_theta_power_posterior500, y=scaared_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot
ggplot(main_df, aes(x=incong_error_theta_power_posterior500, y=bfne_b_scrdTotal_s1_r1_e1)) + geom_point() #scatterplot

cor.test(as.numeric(main_df$scaared_b_scrdSoc_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior500), method = 'pearson', na.action = na.omit)
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_theta_ITPS_posterior500), method = 'pearson', na.action = na.omit)
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
cor.test(as.numericalpha(main_df$scaared_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_power_mfc250), method = 'pearson', na.action = na.omit)
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
cor.test(as.numeric(main_df$bfne_b_scrdTotal_s1_r1_e1), as.numeric(main_df$incong_error_alpha_ITPS_posterior250), method = 'pearson', na.action = na.omit)
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


