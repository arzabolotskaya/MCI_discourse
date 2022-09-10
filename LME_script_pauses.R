library("lme4")
library("lmerTest")
library("dplyr")
library("car")
#install.packages("MuMIn") - ????????????-???? ????????????????????, ???????? ?? ???????? ?????????????????? ???????????? R

setwd("/Users/alinazabolotskaya/Desktop/?????????? ?????????? ?? ??????????/mixed")


# ----Prepare data ---------

# Participant info - read in and check
demo <- read.table("preprocessed_for_lme.csv", header = TRUE, encoding = "Unicode", sep =',')
names(demo)[2]<-"participant"
str(demo)
summary(demo)

# Participant info - center and scale all continuous variables
demo$age.sc <- scale(demo$age, center = TRUE, scale = TRUE)
demo$moca.sc <- scale(demo$moca, center = TRUE, scale = TRUE)
demo$mmse.sc <- scale(demo$mmse, center = TRUE, scale = TRUE)
demo$pause_rate.sc <- scale(demo$pause_rate, center = TRUE, scale = TRUE)
demo$mean_pause_length.sc <- scale(demo$mean_pause_length, center = TRUE, scale = TRUE)
demo$pause_count.sc <- scale(demo$pause_count, center = TRUE, scale = TRUE)
demo$mean_empty_pause_length.sc <- scale(demo$mean_empty_pause_length, center = TRUE, scale = TRUE)
demo$mean_filled_pause_length.sc <- scale(demo$mean_filled_pause_length, center = TRUE, scale = TRUE)
demo$mean_pause_length_between_utt.sc <- scale(demo$mean_pause_length_between_utt, center = TRUE, scale = TRUE)
demo$mean_pause_length_in_utt.sc <- scale(demo$mean_pause_length_in_utt, center = TRUE, scale = TRUE)


# ----LME - pause rate -----

pause_rate.model = lmer(pause_rate ~ age.sc + moca.sc + mmse.sc
                              + (1|participant) + (1|point), demo, REML=FALSE)
summary(pause_rate.model)
vifs_pause_rate.model <- car::vif(pause_rate.model)
print(vifs_pause_rate.model)


# ----LME - mean_pause_length -----

mean_pause_length.model = lmer(mean_pause_length ~ age.sc + moca.sc + mmse.sc
                        + (1|participant) + (1|point), demo, REML=FALSE)
summary(mean_pause_length.model)
vifs_mean_pause_length.model <- car::vif(mean_pause_length.model)
print(vifs_mean_pause_length.model)


# ----LME - pause_count -----

pause_count.model = lmer(pause_count ~ age.sc + moca.sc + mmse.sc
                        + (1|participant) + (1|point), demo, REML=FALSE)
summary(pause_count.model)
vifs_pause_count.model <- car::vif(pause_count.model)
print(vifs_pause_count.model)


# ----LME - mean_empty_pause_length -----

mean_empty_pause_length.model = lmer(mean_empty_pause_length ~ age.sc + moca.sc + mmse.sc
                         + (1|participant) + (1|point), demo, REML=FALSE)
summary(mean_empty_pause_length.model)
vifs_mean_empty_pause_length.model <- car::vif(mean_empty_pause_length.model)
print(mean_empty_pause_length.model)


# ----LME - mean_filled_pause_length -----

mean_filled_pause_length.model = lmer(mean_filled_pause_length ~ age.sc + moca.sc + mmse.sc
                                     + (1|participant) + (1|point), demo, REML=FALSE)
summary(mean_filled_pause_length.model)
vifs_mean_filled_pause_length.model <- car::vif(mean_filled_pause_length.model)
print(mean_filled_pause_length.model)


# ----LME - mean_pause_length_between_utt -----

mean_pause_length_between_utt.model = lmer(mean_pause_length_between_utt ~ age.sc + moca.sc + mmse.sc
                                      + (1|participant) + (1|point), demo, REML=FALSE)
summary(mean_pause_length_between_utt.model)
vifs_mean_pause_length_between_utt.model <- car::vif(mean_pause_length_between_utt.model)
print(mean_pause_length_between_utt.model)


# ----LME - mean_pause_length_in_utt -----

mean_pause_length_in_utt.model = lmer(mean_pause_length_in_utt ~ age.sc + moca.sc + mmse.sc
                                           + (1|participant) + (1|point), demo, REML=FALSE)
summary(mean_pause_length_in_utt.model)
vifs_mean_pause_length_in_utt.model <- car::vif(mean_pause_length_in_utt.model)
print(mean_pause_length_in_utt.model)
