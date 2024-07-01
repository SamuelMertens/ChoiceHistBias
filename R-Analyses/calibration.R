install.packages("purrr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggdist")
install.packages("ggthemes")
install.packages("tidyquant")
install.packages("tidyverse")
install.packages("effects")
install.packages("gghighlight")
install.packages("lme4")
install.packages("HDInterval")
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggdist)
library(ggthemes)
library(tidyverse)
library(tidyquant)
library(effects)
library(gghighlight)
library(lme4)
library(HDInterval)

#==============================================================================#
# Cheat Sheet

# Author: SM
#==============================================================================#

#Variables: subj_idx, session, bloc, reference, trial_id, vas, rt, arm, arm_trial_id

#session: only 1

#bloc: 6 blocs

#reference: reference temperature was 54°C in all trials

#trial_id: 1-25; current trial in bloc

#vas rating: 0 - 100 - 200; Keine Empfindung - Schmerzschwelle - stärkster tolerierbarer Schmerz

#rt: measured from stimulus onset (+1s)

#arm: 1 = left; 2 = right

#arm_trial_id: 1-75; trial id in current arm

#overall_trial_id: 1-150; overall trial id

#==============================================================================#
################################## create DF ###################################
#==============================================================================#

setwd("C:/Users/student/Desktop/GeheimeDaten")
files <- list.files(pattern="*.csv")
mainDF <- files %>% map_dfr(read.csv, sep="") 

#==============================================================================#
############################# create new variables #############################
#==============================================================================#

mainDF$arm <- rep(c(1, 2), each = 75, times = 4) 
mainDF$arm_trial_id <- rep(c(1:75), times = 8) 
mainDF$overall_trial_id <- rep(c(1:150), times = 4) 

#==============================================================================#
########################### mean vas for each subject ##########################
#==============================================================================#

mean(mainDF$vas[mainDF$subj_idx == 83])
mean(mainDF$vas[mainDF$subj_idx == 222])
mean(mainDF$vas[mainDF$subj_idx == 30])
mean(mainDF$vas[mainDF$subj_idx == 3460])

#==============================================================================#
############################ raincloud plot for vas ############################
#==============================================================================#

mainDF %>%
  ggplot(aes(x = factor(subj_idx), y = vas, fill = factor(subj_idx))) + 
  stat_halfeye( # add half-violin from {ggdist} package
    adjust = 0.5, # adjust bandwidth
    justification = -0.2, # move to the right
    .width = 0, # remove the slub interval
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.color = NA, # removing outliers
    alpha = 0.5
    ) +
  stat_dots(
    side = "left", # plotting on left side
    justification = 1.1, # adjusting position
    binwidth = 0.25 # adjust grouping (binning) of observations
  ) +
  # Themes and Labels
  #scale_fill_tq() +
  #theme_tq() +
  labs(
    title = "VAS Plot",
    x = "subject id",
    y = "VAS rating",
    fill = "subject id"
  ) +
  coord_flip()

#==============================================================================#
################# vas over trials for each subject in each bloc ################
#==============================================================================#

mainDF %>%
  ggplot(aes(x = trial_id, y = vas)) +
  geom_line(aes(color=subj_idx), color="#69b3a2", linewidth=1.2 ) +
  geom_smooth(method = "lm") +
  facet_wrap(~ subj_idx + bloc, nrow = 4)

#==============================================================================#
######################## vas over trials for each subject ######################
#==============================================================================#

mainDF <- mainDF %>%
  arrange(subj_idx, session, bloc, trial_id) %>%
  group_by(subj_idx, session) %>%
  mutate(trial_within_participant_and_session = row_number())

mainDF %>%
  ggplot(aes(x = trial_within_participant_and_session, y = vas)) +
  geom_line(aes(color=subj_idx), color="#69b3a2", linewidth=1.2 ) +
  geom_smooth(method = "lm") +
  facet_wrap(~ subj_idx)

sens_habit1 <- lmer(vas ~ trial_within_participant_and_session + (1 | subj_idx), mainDF, REML = FALSE)
sens_habit2 <- lmer(vas ~ 1 + (1 | subj_idx), mainDF, REML = FALSE)
BIC(sens_habit1, sens_habit2)

#==============================================================================#
########################### mixed linear models for rt #########################
#==============================================================================#

null.model <- lmer(rt ~ 1 + (1 | subj_idx), data = mainDF, REML = FALSE)

bloc.model <- lmer(rt ~ trial_id + (1 | subj_idx), data = mainDF, REML = FALSE)

arm.model <- lmer (rt ~ arm_trial_id + (1 | subj_idx), data = mainDF, REML = FALSE)

anova(null.model, bloc.model, arm.model, test = "LRT")
anova(null.model, bloc.model, test = "LRT")
anova(null.model, arm.model, test = "LRT")
summary(arm.model)
#==============================================================================#
################################ rt density plot ###############################
#==============================================================================#

rt_mean <- mean(mainDF$rt)

mainDF %>%
  ggplot( aes(x=rt)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha = 0.8) +
  ggtitle("rt density") +
  geom_vline(aes(xintercept=mean(rt)), color="#de4634", linetype="dotdash", size=1)

mainDF %>%
  filter(rt < 10 ) %>%
  ggplot(aes(x=rt)) +
    geom_density(fill="#69b3a2", color="#e9ecef", alpha = 0.8) +
    ggtitle("rt density") +
    geom_vline(aes(xintercept=mean(rt)), color="#de4634", linetype="dotdash", size=1)

