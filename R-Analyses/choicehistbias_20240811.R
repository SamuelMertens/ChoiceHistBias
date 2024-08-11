install.packages("purrr")
install.packages("lme4")
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidyquant")
install.packages("effects")
install.packages("emmeans")
install.packages("lmerTest")
install.packages("caret")
install.packages("BayesFactor")
install.packages("psych")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("ggpubr")
install.packages("cowplot")
install.packages("sjPlot")
install.packages("sjlabelled")
install.packages("sjmisc")
install.packages("multcomp")
install.packages("ggdist")
install.packages("RColorBrewer")
install.packages("colorspace")
install.packages("cv")
library(emmeans)
library(purrr)
library(tidyr)
library(dplyr)
library(lme4)
library(tidyverse)
library(tidyquant)
library(effects)
library(lmerTest)
library(caret)
library(BayesFactor)
library(psych)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(cowplot)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(multcomp)
library(ggdist)
library(RColorBrewer)
library(colorspace)
library(cv)

#==============================================================================#
# Title: Evidence for a choice-history Bias in Pain Intensity Judgments: A modelling Approach.

#-----
# Analysis: Initial behavioural assessment / inferential statistics
# Author: Samuel Mertens (a) + Fabrice Hubschmid (a,b)
# Affiliations: (a) Department of Experimental Psychology, Faculty of Mathematics and Natural Sciences, Heinrich-Heine University, Düsseldorf, Germany
#               (b) Institute of Clinical Neuroscience and Medical Psychology, Medical Faculty, Heinrich-Heine University, Düsseldorf, Germany
# Contact info: samuel.mertens@hhu.de

#-----
#==============================================================================#

#Variables: subj_idx, session, bloc, reference, trial_id, trial_type, delta, stim_choice, vas, rt

#session: 1 or 2

#bloc: 12 blocs

#reference: reference temperature was 54°C in all trials

#trial_id: 1-25; current trial in bloc

#trial_type: 1 = testtrial; 2 = VAS trial

#stim_type: 1 = stronger; 2 = weaker

#delta: -0.5; -0.4; -0.3; -0.2; -0.1; 0.1; 0.2; 0.3; 0.4; 0.5

#stim_choice: 1 = weaker; 2 = stronger; 3 = no choice 

#vas rating: unused

#rt: measured from choice onset

#==============================================================================#
################################## create DF ###################################
#==============================================================================#

setwd("C:/Users/student/Desktop/MasterthesisData")
setwd("C:/Users/samue/Desktop/MasterthesisData")
setwd("C:/Users/Samuel/Desktop/MasterthesisData")

mainDF <- read.csv("CHBPain_R.csv")

#==============================================================================#
############################ data processing ###################################
#==============================================================================#

mainDF$stim_choice <- ifelse(mainDF$stim_choice %in% c(3, 4), NA, mainDF$stim_choice)

mainDF <- mainDF %>%
  arrange(subj_idx, session, bloc, trial_id) %>%
  group_by(subj_idx, session) %>%
  mutate(
    lag1 = lag(stim_choice, 1),
    lag2 = lag(stim_choice, 2),
    lag3 = lag(stim_choice, 3),
    lag4 = lag(stim_choice, 4),
    lag5 = lag(stim_choice, 5),
    lag6 = lag(stim_choice, 6)
  )  

mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag1 = ifelse(session == 2 & bloc == 1 & trial_id == 1, NA, lag1))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag2 = ifelse(session == 2 & bloc == 1 & trial_id == 1, NA, lag2))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag3 = ifelse(session == 2 & bloc == 1 & trial_id == 1, NA, lag3))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag4 = ifelse(session == 2 & bloc == 1 & trial_id == 1, NA, lag4))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag5 = ifelse(session == 2 & bloc == 1 & trial_id == 1, NA, lag5))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag6 = ifelse(session == 2 & bloc == 1 & trial_id == 1, NA, lag6))

mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag2 = ifelse(session == 2 & bloc == 1 & trial_id == 2, NA, lag2))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag3 = ifelse(session == 2 & bloc == 1 & trial_id == 2, NA, lag3))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag4 = ifelse(session == 2 & bloc == 1 & trial_id == 2, NA, lag4))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag5 = ifelse(session == 2 & bloc == 1 & trial_id == 2, NA, lag5))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag6 = ifelse(session == 2 & bloc == 1 & trial_id == 2, NA, lag6))

mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag3 = ifelse(session == 2 & bloc == 1 & trial_id == 3, NA, lag3))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag4 = ifelse(session == 2 & bloc == 1 & trial_id == 3, NA, lag4))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag5 = ifelse(session == 2 & bloc == 1 & trial_id == 3, NA, lag5))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag6 = ifelse(session == 2 & bloc == 1 & trial_id == 3, NA, lag6))

mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag4 = ifelse(session == 2 & bloc == 1 & trial_id == 4, NA, lag4))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag5 = ifelse(session == 2 & bloc == 1 & trial_id == 4, NA, lag5))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag6 = ifelse(session == 2 & bloc == 1 & trial_id == 4, NA, lag6))

mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag5 = ifelse(session == 2 & bloc == 1 & trial_id == 5, NA, lag5))
mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag6 = ifelse(session == 2 & bloc == 1 & trial_id == 5, NA, lag6))

mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(lag6 = ifelse(session == 2 & bloc == 1 & trial_id == 6, NA, lag6))

mainDF <- mainDF %>%
  arrange(subj_idx, session, bloc, trial_id) %>%
  group_by(subj_idx, session) %>%
  mutate(trial_within_participant_and_session = row_number())

mainDF <- mainDF[complete.cases(mainDF$stim_choice),]

#==============================================================================#
########################### accuracy analysis prep #############################
#==============================================================================#

mainDF$accuracy <- ifelse(mainDF$stim_type == 1 & mainDF$stim_choice == 2 | mainDF$stim_type == 2 & mainDF$stim_choice == 1, 1, 0)
mean_all <- mean(mainDF$accuracy)

stepup <- subset(mainDF, delta == 0.1 | delta == 0.2 | delta == 0.3 | delta == 0.4 | delta == 0.5)
stepup$accuracy <- ifelse(stepup$stim_type == 1 & stepup$stim_choice == 2 | stepup$stim_type == 2 & stepup$stim_choice == 1, 1, 0)
mean_up <- mean(stepup$accuracy)

stepdown <- subset(mainDF, delta == -0.1 | delta == -0.2 | delta == -0.3 | delta == -0.4 | delta == -0.5)
stepdown$accuracy <- ifelse(stepdown$stim_type == 1 & stepdown$stim_choice == 2 | stepdown$stim_type == 2 & stepdown$stim_choice == 1, 1, 0)
mean_down <- mean(stepdown$accuracy)

#==============================================================================#
######################### factored accuracy analysis ###########################
#==============================================================================#

mainDF$stim_type <- as.factor(mainDF$stim_type)
mainDF$stim_choice <- as.factor(mainDF$stim_choice)
levels(mainDF$stim_type) <- c("+Δ", "-Δ")

glmm2 <- glmer(accuracy ~ stim_type + (1|subj_idx), data = mainDF, family = "binomial")
summary(glmm2)
car::Anova(glmm2, type = 3)
confint(glmm2)
plot(allEffects(glmm2))
pairs(emmeans(glmm2, ~ stim_type))

glmm2_df <-
  mainDF %>%
  group_by(subj_idx, stim_type) %>%
  summarize(accuracy = mean(accuracy,na.rm=TRUE))

glmm2_plot <- ggplot(glmm2_df, aes(x = stim_type, y = accuracy)) + 
                ggdist::stat_halfeye(
                  aes(color = stim_type,
                      fill = after_scale(lighten(color, .5))),
                  adjust = .5, 
                  width = .6,
                  .width = 0,
                  justification = -.3, 
                  point_color = NA,
                  inherit.aes = T) + 
                geom_boxplot(
                  aes(color = stim_type,
                      color = after_scale(darken(color, .1, space = "HLS")),
                      fill = after_scale(desaturate(lighten(color, .8), .4))),
                  width = .15, 
                  outlier.shape = NA
                ) +
                geom_point(
                  aes(color = stim_type,
                      color = after_scale(darken(color, .1, space = "HLS"))),
                  fill = "white",
                  shape = 21,
                  stroke = .4,
                  size = 2,
                  position = position_jitter(seed = 1, width = .12)
                ) + 
                geom_point(
                  aes(fill = stim_type),
                  color = "transparent",
                  shape = 21,
                  stroke = .4,
                  size = 2,
                  alpha = .3,
                  position = position_jitter(seed = 1, width = .12)
                ) +
                geom_bracket(
                  xmin = c("+Δ"), xmax = c("-Δ"), y.position = c(1),
                  label = c("***"),
                  tip.length = .01
                ) +
                coord_cartesian(ylim = c(0, 1.05)) +
                xlab("stimulation type") +
                scale_y_continuous("accuracy (in %)", c(0.25, 0.5, 0.75, 1), 1) + 
                ggtitle("All participants") +
                theme_classic() +
                theme(legend.position = "none") +
                scale_color_brewer(palette = "Dark2") +
                scale_fill_brewer(palette = "Dark2")


#==============================================================================#
########################## delta-accuracy analysis #############################
#==============================================================================#

stepup$delta <- as.factor(stepup$delta)

glmm3 <- glmer(accuracy ~ delta + (1|subj_idx), data = stepup, family = "binomial")
summary(glmm3)
car::Anova(glmm3, type = 3)
confint(glmm3)
plot(allEffects(glmm3))
pairs(emmeans(glmm3, ~ delta))

glmm3_df <-
  stepup %>%
  group_by(subj_idx, delta) %>%
  summarize(accuracy = mean(accuracy,na.rm=TRUE))

glmm3_plot <- ggplot(glmm3_df, aes(x = delta, y = accuracy)) + 
                ggdist::stat_halfeye(
                  aes(color = delta,
                      fill = after_scale(lighten(color, .5))),
                  adjust = .5, 
                  width = .6,
                  .width = 0,
                  justification = -.3, 
                  point_color = NA,
                  inherit.aes = T) + 
                geom_boxplot(
                  aes(color = delta,
                      color = after_scale(darken(color, .1, space = "HLS")),
                      fill = after_scale(desaturate(lighten(color, .8), .4))),
                  width = .15, 
                  outlier.shape = NA
                ) +
                geom_point(
                  aes(color = delta,
                      color = after_scale(darken(color, .1, space = "HLS"))),
                  fill = "white",
                  shape = 21,
                  stroke = .4,
                  size = 2,
                  position = position_jitter(seed = 1, width = .12)
                ) + 
                geom_point(
                  aes(fill = delta),
                  color = "transparent",
                  shape = 21,
                  stroke = .4,
                  size = 2,
                  alpha = .3,
                  position = position_jitter(seed = 1, width = .12)
                ) +
                geom_bracket(
                  xmin = c("0.1", "0.1", "0.1", "0.2"), xmax = c("0.3", "0.4", "0.5", "0.5"), y.position = c(1, 1.07, 1.14, 1.2),
                  label = c("***", "***", "***", "***"),
                  tip.length = .01
                ) +
                coord_cartesian(ylim = c(0, 1.25)) +
                xlab("Δ (in °C)") +
                ggtitle("All participants; only +Δ") +
                scale_y_continuous("accuracy (in %)", c(0.25, 0.5, 0.75, 1), 1) +
                theme_classic() +
                theme(legend.position = "none") +
                scale_color_brewer(palette = "Dark2") +
                scale_fill_brewer(palette = "Dark2")

stepdown$delta <- as.factor(stepdown$delta)

glmm4 <- glmer(accuracy ~ delta + (1|subj_idx), data = stepdown, family = "binomial")
summary(glmm4)
car::Anova(glmm4, type = 3)
confint(glmm4)
plot(allEffects(glmm4))
pairs(emmeans(glmm4, ~ delta))

glmm4_df <-
  stepdown %>%
  group_by(subj_idx, delta) %>%
  summarize(accuracy = mean(accuracy,na.rm=TRUE))

glmm4_plot <- ggplot(glmm4_df, aes(x = delta, y = accuracy)) + 
                ggdist::stat_halfeye(
                  aes(color = delta,
                      fill = after_scale(lighten(color, .5))),
                  adjust = .5, 
                  width = .6,
                  .width = 0,
                  justification = -.3, 
                  point_color = NA,
                  inherit.aes = T) + 
                geom_boxplot(
                  aes(color = delta,
                      color = after_scale(darken(color, .1, space = "HLS")),
                      fill = after_scale(desaturate(lighten(color, .8), .4))),
                  width = .15, 
                  outlier.shape = NA
                ) +
                geom_point(
                  aes(color = delta,
                      color = after_scale(darken(color, .1, space = "HLS"))),
                  fill = "white",
                  shape = 21,
                  stroke = .4,
                  size = 2,
                  position = position_jitter(seed = 1, width = .12)
                ) + 
                geom_point(
                  aes(fill = delta),
                  color = "transparent",
                  shape = 21,
                  stroke = .4,
                  size = 2,
                  alpha = .3,
                  position = position_jitter(seed = 1, width = .12)
                ) +
                geom_bracket(
                  xmin = c("-0.5", "-0.5", "-0.5", "-0.5", "-0.4", "-0.4"), xmax = c("-0.4", "-0.3", "-0.2", "-0.1", "-0.2", "-0.1"), y.position = c(1, 1.04, 1.08, 1.12, 1.16, 1.2),
                  label = c("**", "***", "***", "***", "**", "***"),
                  tip.length = .01
                ) +
                coord_cartesian(ylim = c(0, 1.25)) + 
                theme_classic() +
                xlab("Δ (in °C)") +
                ggtitle("All participants; only -Δ") +
                scale_y_continuous("accuracy (in %)", c(0.25, 0.5, 0.75, 1), 1) + 
                theme(legend.position = "none") +
                scale_color_brewer(palette = "Dark2") +
                scale_fill_brewer(palette = "Dark2")


#==============================================================================#
############################ classify rep/alt ##################################
#==============================================================================#

mainDF$repetition <- ifelse(!is.na(mainDF$lag1) & mainDF$stim_choice == mainDF$lag1, 1, ifelse(is.na(mainDF$lag1), NA, 0))

aggregate(repetition ~ subj_idx, data = mainDF, FUN = mean)

repeatorsDF <- mainDF %>%
  group_by(subj_idx) %>%
  filter(mean(repetition, na.rm = TRUE) > 0.5)

alternatorsDF <- mainDF %>%
  group_by(subj_idx) %>%
  filter(mean(repetition, na.rm = TRUE) < 0.5)

#==============================================================================#
############## factored accuracy analysis pooled by rep/alt ####################
#==============================================================================#

glmm5 <- glmer(accuracy ~ stim_type + (1|subj_idx), data = repeatorsDF, family = "binomial")
summary(glmm5)
car::Anova(glmm5, type = 3)
confint(glmm5)
plot(allEffects(glmm5))
pairs(emmeans(glmm5, ~ stim_type))

glmm5_df <-
  repeatorsDF %>%
  group_by(subj_idx, stim_type) %>%
  summarize(accuracy = mean(accuracy,na.rm=TRUE))

glmm5_plot <- ggplot(glmm5_df, aes(x = stim_type, y = accuracy)) + 
                ggdist::stat_halfeye(
                  aes(color = stim_type,
                      fill = after_scale(lighten(color, .5))),
                  adjust = .5, 
                  width = .6,
                  .width = 0,
                  justification = -.3, 
                  point_color = NA,
                  inherit.aes = T) + 
                geom_boxplot(
                  aes(color = stim_type,
                      color = after_scale(darken(color, .1, space = "HLS")),
                      fill = after_scale(desaturate(lighten(color, .8), .4))),
                  width = .15, 
                  outlier.shape = NA
                ) +
                geom_point(
                  aes(color = stim_type,
                      color = after_scale(darken(color, .1, space = "HLS"))),
                  fill = "white",
                  shape = 21,
                  stroke = .4,
                  size = 2,
                  position = position_jitter(seed = 1, width = .12)
                ) + 
                geom_point(
                  aes(fill = stim_type),
                  color = "transparent",
                  shape = 21,
                  stroke = .4,
                  size = 2,
                  alpha = .3,
                  position = position_jitter(seed = 1, width = .12)
                ) +
                geom_bracket(
                  xmin = c("+Δ"), xmax = c("-Δ"), y.position = c(1),
                  label = c("***"),
                  tip.length = .01
                ) +
                coord_cartesian(ylim = c(0, 1.05)) + 
                theme_classic() +
                xlab("stimulation type") +
                scale_y_continuous("accuracy (in %)", c(0.25, 0.5, 0.75, 1), 1) + 
                ggtitle("Repeaters") +
                theme(legend.position = "none") +
                scale_color_brewer(palette = "Dark2") +
                scale_fill_brewer(palette = "Dark2")

glmm6 <- glmer(accuracy ~ stim_type + (1|subj_idx), data = alternatorsDF, family = "binomial")
summary(glmm6)
car::Anova(glmm6, type = 3)
confint(glmm6)
plot(allEffects(glmm6))
pairs(emmeans(glmm6, ~ stim_type))

glmm6_df <-
  alternatorsDF %>%
  group_by(subj_idx, stim_type) %>%
  summarize(accuracy = mean(accuracy,na.rm=TRUE))

glmm6_plot <- ggplot(glmm6_df, aes(x = stim_type, y = accuracy)) + 
                ggdist::stat_halfeye(
                  aes(color = stim_type,
                      fill = after_scale(lighten(color, .5))),
                  adjust = .5, 
                  width = .6,
                  .width = 0,
                  justification = -.3, 
                  point_color = NA,
                  inherit.aes = T) + 
                geom_boxplot(
                  aes(color = stim_type,
                      color = after_scale(darken(color, .1, space = "HLS")),
                      fill = after_scale(desaturate(lighten(color, .8), .4))),
                  width = .15, 
                  outlier.shape = NA
                ) +
                geom_point(
                  aes(color = stim_type,
                      color = after_scale(darken(color, .1, space = "HLS"))),
                  fill = "white",
                  shape = 21,
                  stroke = .4,
                  size = 2,
                  position = position_jitter(seed = 1, width = .12)
                ) + 
                geom_point(
                  aes(fill = stim_type),
                  color = "transparent",
                  shape = 21,
                  stroke = .4,
                  size = 2,
                  alpha = .3,
                  position = position_jitter(seed = 1, width = .12)
                ) +
                geom_bracket(
                  xmin = c("+Δ"), xmax = c("-Δ"), y.position = c(1),
                  label = c("***"),
                  tip.length = .01
                ) +
                coord_cartesian(ylim = c(0, 1.05)) + 
                theme_classic() +
                xlab("stimulation type") +
                scale_y_continuous("accuracy (in %)", c(0.25, 0.5, 0.75, 1), 1) + 
                ggtitle("Alternators") +
                theme(legend.position = "none") +
                scale_color_brewer(palette = "Dark2") +
                scale_fill_brewer(palette = "Dark2")

tiff("combined_plot1.tiff", units="in", width=10, height=4, res=300)
cowplot::plot_grid(
  glmm2_plot, glmm5_plot, glmm6_plot,
  labels = c("A", "B", "C"),
  ncol = 3
)
dev.off()

#==============================================================================#
############## delta-accuracy analysis pooled by rep/alt ####################
#==============================================================================#

rep_stepup <- subset(repeatorsDF, delta == 0.1 | delta == 0.2 | delta == 0.3 | delta == 0.4 | delta == 0.5)
rep_stepup$accuracy <- ifelse(rep_stepup$stim_type == "+Δ" & rep_stepup$stim_choice == 2 | rep_stepup$stim_type == "-Δ" & rep_stepup$stim_choice == 1, 1, 0)

rep_stepdown <- subset(repeatorsDF, delta == -0.1 | delta == -0.2 | delta == -0.3 | delta == -0.4 | delta == -0.5)
rep_stepdown$accuracy <- ifelse(rep_stepdown$stim_type == "+Δ" & rep_stepdown$stim_choice == 2 | rep_stepdown$stim_type == "-Δ" & rep_stepdown$stim_choice == 1, 1, 0)

rep_stepup$delta <- as.factor(rep_stepup$delta)

glmm7 <- glmer(accuracy ~ delta + (1|subj_idx), data = rep_stepup, family = "binomial")
summary(glmm7)
car::Anova(glmm7, type = 3)
confint(glmm7)
plot(allEffects(glmm7))
pairs(emmeans(glmm7, ~ delta))

glmm7_df <-
  rep_stepup %>%
  group_by(subj_idx, delta) %>%
  summarize(accuracy = mean(accuracy,na.rm=TRUE))

glmm7_plot <- ggplot(glmm7_df, aes(x = delta, y = accuracy)) + 
  ggdist::stat_halfeye(
    aes(color = delta,
        fill = after_scale(lighten(color, .5))),
    adjust = .5, 
    width = .6,
    .width = 0,
    justification = -.3, 
    point_color = NA,
    inherit.aes = T) + 
  geom_boxplot(
    aes(color = delta,
        color = after_scale(darken(color, .1, space = "HLS")),
        fill = after_scale(desaturate(lighten(color, .8), .4))),
    width = .15, 
    outlier.shape = NA
  ) +
  geom_point(
    aes(color = delta,
        color = after_scale(darken(color, .1, space = "HLS"))),
    fill = "white",
    shape = 21,
    stroke = .4,
    size = 2,
    position = position_jitter(seed = 1, width = .12)
  ) + 
  geom_point(
    aes(fill = delta),
    color = "transparent",
    shape = 21,
    stroke = .4,
    size = 2,
    alpha = .3,
    position = position_jitter(seed = 1, width = .12)
  ) +
  geom_bracket(
    xmin = c("0.1", "0.1", "0.1", "0.1", "0.2"), xmax = c("0.2", "0.3", "0.4", "0.5", "0.5"), y.position = c(1, 1.05, 1.10, 1.15, 1.2),
    label = c("*", "**", "***", "***", "*"),
    tip.length = .01
  ) +
  coord_cartesian(ylim = c(0, 1.25)) +
  xlab("Δ (in °C)") +
  ggtitle("Repeaters; only +Δ") +
  scale_y_continuous("accuracy (in %)", c(0.25, 0.5, 0.75, 1), 1) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

rep_stepdown$delta <- as.factor(rep_stepdown$delta)

glmm8 <- glmer(accuracy ~ delta + (1|subj_idx), data = rep_stepdown, family = "binomial")
summary(glmm8)
car::Anova(glmm8, type = 3)
confint(glmm8)
plot(allEffects(glmm8))
pairs(emmeans(glmm8, ~ delta))

glmm8_df <-
  rep_stepdown %>%
  group_by(subj_idx, delta) %>%
  summarize(accuracy = mean(accuracy,na.rm=TRUE))

glmm8_plot <- ggplot(glmm8_df, aes(x = delta, y = accuracy)) + 
  ggdist::stat_halfeye(
    aes(color = delta,
        fill = after_scale(lighten(color, .5))),
    adjust = .5, 
    width = .6,
    .width = 0,
    justification = -.3, 
    point_color = NA,
    inherit.aes = T) + 
  geom_boxplot(
    aes(color = delta,
        color = after_scale(darken(color, .1, space = "HLS")),
        fill = after_scale(desaturate(lighten(color, .8), .4))),
    width = .15, 
    outlier.shape = NA
  ) +
  geom_point(
    aes(color = delta,
        color = after_scale(darken(color, .1, space = "HLS"))),
    fill = "white",
    shape = 21,
    stroke = .4,
    size = 2,
    position = position_jitter(seed = 1, width = .12)
  ) + 
  geom_point(
    aes(fill = delta),
    color = "transparent",
    shape = 21,
    stroke = .4,
    size = 2,
    alpha = .3,
    position = position_jitter(seed = 1, width = .12)
  ) +
  geom_bracket(
    xmin = c("-0.5", "-0.5", "-0.5", "-0.5", "-0.4", "-0.4"), xmax = c("-0.4", "-0.3", "-0.2", "-0.1", "-0.2", "-0.1"), y.position = c(1, 1.04, 1.08, 1.12, 1.16, 1.2),
    label = c("**", "***", "***", "***", "*", "**"),
    tip.length = .01
  ) +
  coord_cartesian(ylim = c(0, 1.25)) +
  xlab("Δ (in °C)") +
  ggtitle("Repeaters; only -Δ") +
  scale_y_continuous("accuracy (in %)", c(0.25, 0.5, 0.75, 1), 1) +  
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

alt_stepup <- subset(alternatorsDF, delta == 0.1 | delta == 0.2 | delta == 0.3 | delta == 0.4 | delta == 0.5)
alt_stepup$accuracy <- ifelse(alt_stepup$stim_type == "+Δ" & alt_stepup$stim_choice == 2 | alt_stepup$stim_type == "-Δ" & alt_stepup$stim_choice == 1, 1, 0)

alt_stepdown <- subset(alternatorsDF, delta == -0.1 | delta == -0.2 | delta == -0.3 | delta == -0.4 | delta == -0.5)
alt_stepdown$accuracy <- ifelse(alt_stepdown$stim_type == "+Δ" & alt_stepdown$stim_choice == 2 | alt_stepdown$stim_type == "-Δ" & alt_stepdown$stim_choice == 1, 1, 0)

alt_stepup$delta <- as.factor(alt_stepup$delta)

glmm9 <- glmer(accuracy ~ delta + (1|subj_idx), data = alt_stepup, family = "binomial")
summary(glmm9)
car::Anova(glmm9, type = 3)
confint(glmm9)
plot(allEffects(glmm9))
pairs(emmeans(glmm9, ~ delta))

glmm9_df <-
  alt_stepup %>%
  group_by(subj_idx, delta) %>%
  summarize(accuracy = mean(accuracy,na.rm=TRUE))

glmm9_plot <- ggplot(glmm9_df, aes(x = delta, y = accuracy)) + 
  ggdist::stat_halfeye(
    aes(color = delta,
        fill = after_scale(lighten(color, .5))),
    adjust = .5, 
    width = .6,
    .width = 0,
    justification = -.3, 
    point_color = NA,
    inherit.aes = T) + 
  geom_boxplot(
    aes(color = delta,
        color = after_scale(darken(color, .1, space = "HLS")),
        fill = after_scale(desaturate(lighten(color, .8), .4))),
    width = .15, 
    outlier.shape = NA
  ) +
  geom_point(
    aes(color = delta,
        color = after_scale(darken(color, .1, space = "HLS"))),
    fill = "white",
    shape = 21,
    stroke = .4,
    size = 2,
    position = position_jitter(seed = 1, width = .12)
  ) + 
  geom_point(
    aes(fill = delta),
    color = "transparent",
    shape = 21,
    stroke = .4,
    size = 2,
    alpha = .3,
    position = position_jitter(seed = 1, width = .12)
  ) +
  geom_bracket(
    xmin = c("0.1"), xmax = c("0.5"), y.position = c(1),
    label = c("*"),
    tip.length = .01
  ) +
  coord_cartesian(ylim = c(0, 1.25)) +
  xlab("Δ (in °C)") +
  ggtitle("Alternators; only +Δ") +
  scale_y_continuous("accuracy (in %)", c(0.25, 0.5, 0.75, 1), 1) +  
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

alt_stepdown$delta <- as.factor(alt_stepdown$delta)

glmm10 <- glmer(accuracy ~ delta + (1|subj_idx), data = alt_stepdown, family = "binomial")
summary(glmm10)
car::Anova(glmm10, type = 3)
confint(glmm10)
plot(allEffects(glmm10))
pairs(emmeans(glmm10, ~ delta))

glmm10_df <-
  alt_stepdown %>%
  group_by(subj_idx, delta) %>%
  summarize(accuracy = mean(accuracy,na.rm=TRUE))

glmm10_plot <- ggplot(glmm10_df, aes(x = delta, y = accuracy)) + 
  ggdist::stat_halfeye(
    aes(color = delta,
        fill = after_scale(lighten(color, .5))),
    adjust = .5, 
    width = .6,
    .width = 0,
    justification = -.3, 
    point_color = NA,
    inherit.aes = T) + 
  geom_boxplot(
    aes(color = delta,
        color = after_scale(darken(color, .1, space = "HLS")),
        fill = after_scale(desaturate(lighten(color, .8), .4))),
    width = .15, 
    outlier.shape = NA
  ) +
  geom_point(
    aes(color = delta,
        color = after_scale(darken(color, .1, space = "HLS"))),
    fill = "white",
    shape = 21,
    stroke = .4,
    size = 2,
    position = position_jitter(seed = 1, width = .12)
  ) + 
  geom_point(
    aes(fill = delta),
    color = "transparent",
    shape = 21,
    stroke = .4,
    size = 2,
    alpha = .3,
    position = position_jitter(seed = 1, width = .12)
  ) +
  geom_bracket(
    xmin = c("-0.5"), xmax = c("-0.1"), y.position = c(1),
    label = c("**"),
    tip.length = .01
  ) +
  coord_cartesian(ylim = c(0, 1.25)) +
  xlab("Δ (in °C)") +
  ggtitle("Alternators; only -Δ") +
  scale_y_continuous("accuracy (in %)", c(0.25, 0.5, 0.75, 1), 1) +  
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

tiff("combined_plot2_part1.tiff", units="in", width=9, height=7.15, res=400)
cowplot::plot_grid(
  glmm3_plot, glmm4_plot,
  labels = c("A", "B"),
  ncol = 2,
  nrow = 1
)
dev.off()

tiff("combined_plot2_part2.tiff", units="in", width=9, height=14.3, res=400)
cowplot::plot_grid(
  glmm7_plot, glmm8_plot, glmm9_plot, glmm10_plot,
  labels = c("C", "D", "E", "F"),
  ncol = 2,
  nrow = 2
)
dev.off()

#==============================================================================#
###################### mixed effects logistic regression #######################
#==============================================================================#

mainDF$stim_type <- ifelse(mainDF$stim_type == "-Δ", 0, 1)
mainDF$stim_choice <- ifelse(mainDF$stim_choice == 1, 0, 1)
mainDF$lag1 <- ifelse(mainDF$lag1 == 1, 0, 1)
mainDF$lag2 <- ifelse(mainDF$lag2 == 1, 0, 1)
mainDF$lag3 <- ifelse(mainDF$lag3 == 1, 0, 1)
mainDF$lag4 <- ifelse(mainDF$lag4 == 1, 0, 1)
mainDF$lag5 <- ifelse(mainDF$lag5 == 1, 0, 1)
mainDF$lag6 <- ifelse(mainDF$lag6 == 1, 0, 1)

mainDF$stim_type <- as.factor(mainDF$stim_type)
mainDF$stim_choice <- as.factor(mainDF$stim_choice)
mainDF$lag1 <- as.factor(mainDF$lag1)
mainDF$lag2 <- as.factor(mainDF$lag2)
mainDF$lag3 <- as.factor(mainDF$lag3)
mainDF$lag4 <- as.factor(mainDF$lag4)
mainDF$lag5 <- as.factor(mainDF$lag5)
mainDF$lag6 <- as.factor(mainDF$lag6)

mainDF <- mainDF %>%
  filter(!(trial_within_participant_and_session %in% c(1, 2, 3, 4, 5, 6)))                  

llmm6 <- glmer(stim_choice ~ stim_type + lag1 + lag2 + lag3 + lag4 + lag5 + lag6 + (1|subj_idx), data = mainDF, family = "binomial")
summary(llmm6)
car::Anova(llmm6, type = 3)
BIC(llmm6)
confint(llmm6)
pairs(emmeans(llmm6, ~ stim_type + lag1 + lag2 + lag3 + lag4 + lag5 + lag6))

#==============================================================================#
####### mixed effects logistic regression clustered analysis by rep/alt ########
#==============================================================================#

repeatorsDF$stim_type <- ifelse(repeatorsDF$stim_type == "-Δ", 0, 1)
repeatorsDF$stim_choice <- ifelse(repeatorsDF$stim_choice == 1, 0, 1)
repeatorsDF$lag1 <- ifelse(repeatorsDF$lag1 == 1, 0, 1)
repeatorsDF$lag2 <- ifelse(repeatorsDF$lag2 == 1, 0, 1)
repeatorsDF$lag3 <- ifelse(repeatorsDF$lag3 == 1, 0, 1)
repeatorsDF$lag4 <- ifelse(repeatorsDF$lag4 == 1, 0, 1)
repeatorsDF$lag5 <- ifelse(repeatorsDF$lag5 == 1, 0, 1)
repeatorsDF$lag6 <- ifelse(repeatorsDF$lag6 == 1, 0, 1)

repeatorsDF$stim_type <- as.factor(repeatorsDF$stim_type)
repeatorsDF$stim_choice <- as.factor(repeatorsDF$stim_choice)
repeatorsDF$lag1 <- as.factor(repeatorsDF$lag1)
repeatorsDF$lag2 <- as.factor(repeatorsDF$lag2)
repeatorsDF$lag3 <- as.factor(repeatorsDF$lag3)
repeatorsDF$lag4 <- as.factor(repeatorsDF$lag4)
repeatorsDF$lag5 <- as.factor(repeatorsDF$lag5)
repeatorsDF$lag6 <- as.factor(repeatorsDF$lag6)

llmmr6 <- glmer(stim_choice ~ stim_type + lag1 + lag2 + lag3 +lag4 +lag5 + lag6 + (1|subj_idx), data = repeatorsDF, family = "binomial")
summary(llmmr6)
car::Anova(llmmr6, type = 3)
BIC(llmmr6)
confint(llmmr6)

alternatorsDF$stim_type <- ifelse(alternatorsDF$stim_type == "-Δ", 0, 1)
alternatorsDF$stim_choice <- ifelse(alternatorsDF$stim_choice == 1, 0, 1)
alternatorsDF$lag1 <- ifelse(alternatorsDF$lag1 == 1, 0, 1)
alternatorsDF$lag2 <- ifelse(alternatorsDF$lag2 == 1, 0, 1)
alternatorsDF$lag3 <- ifelse(alternatorsDF$lag3 == 1, 0, 1)
alternatorsDF$lag4 <- ifelse(alternatorsDF$lag4 == 1, 0, 1)
alternatorsDF$lag5 <- ifelse(alternatorsDF$lag5 == 1, 0, 1)
alternatorsDF$lag6 <- ifelse(alternatorsDF$lag6 == 1, 0, 1)

alternatorsDF$stim_type <- as.factor(alternatorsDF$stim_type)
alternatorsDF$stim_choice <- as.factor(alternatorsDF$stim_choice)
alternatorsDF$lag1 <- as.factor(alternatorsDF$lag1)
alternatorsDF$lag2 <- as.factor(alternatorsDF$lag2)
alternatorsDF$lag3 <- as.factor(alternatorsDF$lag3)
alternatorsDF$lag4 <- as.factor(alternatorsDF$lag4)
alternatorsDF$lag5 <- as.factor(alternatorsDF$lag5)
alternatorsDF$lag6 <- as.factor(alternatorsDF$lag6)

llmma6 <- glmer(stim_choice ~ stim_type + lag1 + lag2 + lag3 +lag4 +lag5 + lag6 + (1|subj_idx), data = alternatorsDF, family = "binomial")
summary(llmma6)
car::Anova(llmma6, type = 3)
BIC(llmma6)
confint(llmma6)

#==============================================================================#
#################### assigning individual bias parameter dc ####################
#==============================================================================#

CHB_HDDM1 <- read.csv("CHB_HDDM1.csv", sep = ";")
CHB_HDDM1_dc <- CHB_HDDM1[ ,grepl("dc_subj.", names(CHB_HDDM1))]

mainDF_together <- data.frame()

for (i in 1:(ncol(CHB_HDDM1_dc)/2)) {
  mean_value <- mean(CHB_HDDM1_dc[, (i + (ncol(CHB_HDDM1_dc)/2))], na.rm = TRUE) - mean(CHB_HDDM1_dc[, i], na.rm = TRUE)
  subject <- unique(mainDF$subj_idx)
  mainDF_subset <- mainDF[mainDF$subj_idx == subject[i],]
  mainDF_subset$IBP_dc <- mean_value
  mainDF_together <- rbind(mainDF_together, mainDF_subset)
}

mainDF$IBP_dc <- mainDF_together$IBP_dc

#==============================================================================#
##### bayesian correlation of individual bias parameter dc and Pr(rep/alt) #####
#==============================================================================#

unique_IBP_dc <- aggregate(IBP_dc ~ subj_idx, data = mainDF, FUN = unique, na.rm = TRUE)
unique_rep_mean <- aggregate(repetition ~ subj_idx, data = mainDF, FUN = mean, na.rm = TRUE)
corr_df <- cbind(unique_rep_mean, unique_IBP_dc)

bf = correlationBF(corr_df$repetition, corr_df$IBP_dc)
samples = posterior(bf, iterations = 10000)
summary(samples)
correlationBF(corr_df$repetition, corr_df$IBP_dc)

#==============================================================================#
################ plots for model fit and corr Pr(rep) and dc ###################
#==============================================================================#

DIC <- data.frame(Model = c("v bias", "z bias", "hybrid"), DIC = c(-577.88, -407.43, -569.33))

plot1 <- ggplot(DIC, aes(x = Model, y = DIC, fill = Model)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = DIC), vjust = -1.6, size = 3.5) +
  scale_fill_brewer(palette = "Greens") +
  theme(panel.background = element_blank()) +
  theme_classic()

corr_df <- corr_df[-3]

plot2 <- ggplot(corr_df, aes(x = repetition, y = IBP_dc)) +
  geom_point() +
  labs(x = "Probability of Repetition", y = "History Shift") +
  theme(panel.background = element_blank()) +
  geom_smooth(method = "lm", col = "black") +
  annotate("text", x = 0.53, y = 1.1, label = "ρ = .72, BF10 = 4768.86", size = 3.5) +
  theme_classic()

plot_grid(plot1, plot2, labels = c("A", "B"), ncol = 2, nrow = 1)

tiff("test.tiff", units="in", width=10, height=4, res=300)
plot_grid(plot1, plot2, labels = c("A", "B"), ncol = 2, nrow = 1)
dev.off()

#==============================================================================#
###### bayesian correlation of individual bias parameter dc and accuracy #######
#==============================================================================#

unique_IBP_dc <- aggregate(IBP_dc ~ subj_idx, data = mainDF, FUN = unique, na.rm = TRUE)
unique_acc_mean <- aggregate(accuracy ~ subj_idx, data = mainDF, FUN = mean, na.rm = TRUE)

corr_df <- cbind(unique_acc_mean, unique_IBP_dc)
corr_df$IBP_dc <- abs(corr_df$IBP_dc)

bf2 = correlationBF(corr_df$accuracy, corr_df$IBP_dc)
samples = posterior(bf2, iterations = 10000)
summary(samples)
correlationBF(corr_df$accuracy, corr_df$IBP_dc)

cor(corr_df$IBP_dc, corr_df$accuracy, method = "spearman")

#==============================================================================#
############################### reading in HPT #################################
#==============================================================================#

CHB_HPT <- readxl::read_xlsx("CHB_HPT.xlsx")

CHB_HPT <- CHB_HPT %>%
  arrange(subj_idx) %>%
  mutate_at(vars(-1), as.numeric) %>%
  mutate(meanHPT = rowMeans(select(., -1), na.rm = TRUE))

mainDF_together2 <- data.frame()

for (i in 1:(nrow(CHB_HPT))) {
  mean_value <- CHB_HPT$meanHPT[i]
  subject <- unique(mainDF$subj_idx)
  mainDF_subset <- mainDF[mainDF$subj_idx == subject[i],]
  mainDF_subset$HPT <- mean_value
  mainDF_together2 <- rbind(mainDF_together2, mainDF_subset)
}

mainDF$meanHPT <- mainDF_together2$HPT

#==============================================================================#
##### bayesian correlation of individual bias parameters and pain threshold ####
#==============================================================================#

unique_meanHPT <- aggregate(meanHPT ~ subj_idx, data = mainDF, FUN = unique, na.rm = TRUE)
corr_df <- cbind(unique_meanHPT, corr_df)

bf4 = correlationBF(corr_df$meanHPT, corr_df$IBP_dc)
samples = posterior(bf4, iterations = 10000)
summary(samples)
correlationBF(corr_df$meanHPT, corr_df$IBP_dc)

#==============================================================================#
############# bayesian correlation of accuracy and pain threshold ##############
#==============================================================================#

unique_acc_mean <- aggregate(accuracy ~ subj_idx, data = mainDF, FUN = mean, na.rm = TRUE)
corr_df <- cbind(unique_acc_mean, corr_df)

bf5 = correlationBF(corr_df$accuracy, corr_df$meanHPT)
samples = posterior(bf5, iterations = 10000)
summary(samples)
correlationBF(corr_df$accuracy, corr_df$meanHPT)

#==============================================================================#
################### exclusion of subjects w/o quest-data #######################
#==============================================================================#

mainDF <- mainDF[mainDF$subj_idx != 1207,]
corr_df <- corr_df[corr_df$subj_idx != 1207,]

#==============================================================================#
############################## reading in NISS #################################
#==============================================================================#

CHB_NISS <- readxl::read_xlsx("CHB_NISS.xlsx")

func_niss <- function(data_frame_in) {
  #Need Inventory of Sensation Seeking
  #5-stufige Skala: 1 = "fast nie"; 5 = "fast immer"
  #Gesamtskala mit 17 Items sowie zwei Subskalen „Bedürfnis nach Stimulation“ (BS) und Subskala „Vermeidung von Ruhe“ (VR)
  #Kennwert ist der Mittelwert aller Items einer Skala; 
  #Bei fehlenden Werten zwei Optionen:  1. durch gerundeten Itemmittelwert der Normierungsstichprobe oder 2. fehlenden Wert durch den entsprechenden Skalenmittelwert der Untersuchungsperson schätzen -> diese Methode jedoch deutlich aufwändiger, Schätzung fällt weniger konservativ aus (in diesem Skript integriert); es soll aber nur ein missing value so ersetzt werden
  
  niss_names = c("subj_idx", c(paste0(rep("niss_0"), 1:9), paste0(rep("niss_"), 10:17))) 
  FB = data_frame_in
  names(FB) = niss_names
  
  if(length(names(FB)) != length(niss_names)){
    stop("ERROR THIS IS NOT THE CORRECT NUMBER OF INPUT ITEMS!!! WRONG RESULTS!!! Analysis cancelled...")
  }
  
  if(sum(sapply(data_frame_in, class)[2:18] == "numeric") + sum(sapply(data_frame_in, class)[2:18] == "logical") != 17){
    stop("ERROR THE ITEMS OF THE NISS NEED TO BE NUMERIC!!! Analysis cancelled...")
  }
  
  if(max(data_frame_in[,c(2:18)], na.rm = T) > 5 |  min(data_frame_in[,2:18], na.rm = T) < 1){
    stop("ERROR THE ITMES OF THE NISS NEED TO BE IN A RANGE FROM 1 TO 5!!! Analysis cancelled...")
  }
  
  #### NISS #################################################################
  
  niss_BS <- c(paste0(rep("niss_0"), c(1:7)), paste0(rep("niss_"), c(13:16)))     #Subscale "Bedürfnis nach Stimulation"
  niss_VR <- c(paste0(rep("niss_0"), c(8:9)), paste0(rep("niss_"), c(10:12, 17))) #Subscale „Vermeidung von Ruhe"
  
  inverse_with_na <- function(x) {                                                #inversion formula
    ifelse(is.na(x), NA, 6 - x)
  }
  
  FB$niss_08 <- inverse_with_na(FB$niss_08)                                       #inversing items
  FB$niss_09 <- inverse_with_na(FB$niss_09)
  FB$niss_10 <- inverse_with_na(FB$niss_10)
  FB$niss_11 <- inverse_with_na(FB$niss_11)
  FB$niss_12 <- inverse_with_na(FB$niss_12)
  FB$niss_17 <- inverse_with_na(FB$niss_17)
  
  if(all(rowSums(is.na(FB)) == 0)) {                                              #calculating overall-score, given that no values are missing
    FB$NISS <- rowMeans(FB[,-1], na.rm = TRUE)
    FB$missings <- "none"
  } else if(all(rowSums(is.na(FB)) == 1)) {                                       #calculating overall-score, given that one value is missing
    FB$NISS <- rowMeans(FB[,-1], na.rm = TRUE)
    FB$missings <- "one"
  } else{                                                                         #NA if more than two missing values
    FB$NISS <- NA
    FB$missings <- "multiple"
  }
  
  if(all(rowSums(is.na(FB[niss_BS])) == 0)) {                                     #calculating BS-score, given that no values is missing
    FB$BS <- rowMeans(FB[niss_BS], na.rm = TRUE)
    FB$BS_missings <- "none"
  } else if(all(rowSums(is.na(FB[niss_BS])) == 1)) {                              #calculating BS-score, given that one value is missing
    FB$BS <- rowMeans(FB[niss_BS], na.rm = TRUE)
    FB$BS_missings <- "one"
  } else{                                                                         #NA if more than two missing values
    FB$BS <- NA
    FB$BS_missings <- "multiple"
  }
  
  if(all(rowSums(is.na(FB[niss_VR])) == 0)) {                                     #calculating VR-score, given that no values is missing
    FB$VR <- rowMeans(FB[niss_VR], na.rm = TRUE)
    FB$VR_missings <- "none"
  } else if(all(rowSums(is.na(FB[niss_VR])) == 1)) {                              #calculating VR-score, given that one value is missing
    FB$VR <- rowMeans(FB[niss_VR], na.rm = TRUE)
    FB$VR_missings <- "one"
  } else{                                                                         #NA if more than two missing values
    FB$VR <- NA
    FB$VR_missings <- "multiple"
  }
  
  FB = subset(FB, select = c(subj_idx, NISS, missings, BS, BS_missings, VR, VR_missings))
  return(FB)
}

CHB_NISS <- func_niss(CHB_NISS)

mainDF_together3 <- data.frame()

for (i in 1:(nrow(CHB_NISS))) {
  mean_value <- CHB_NISS$NISS[i]
  subject <- unique(mainDF$subj_idx)
  mainDF_subset <- mainDF[mainDF$subj_idx == subject[i],]
  mainDF_subset$NISS <- mean_value
  mainDF_together3 <- rbind(mainDF_together3, mainDF_subset)
}

mainDF$NISS <- mainDF_together3$NISS

#==============================================================================#
########## bayesian correlation of individual bias parameters and NISS #########
#==============================================================================#

unique_NISS <- aggregate(NISS ~ subj_idx, data = mainDF, FUN = unique, na.rm = TRUE)
corr_df <- cbind(unique_NISS, corr_df)

bf6 = correlationBF(corr_df$NISS, corr_df$IBP_dc)
samples = posterior(bf6, iterations = 10000)
summary(samples)
correlationBF(corr_df$NISS, corr_df$IBP_dc)

#==============================================================================#
############################### reading in PCS #################################
#==============================================================================#

CHB_PCS <- readxl::read_xlsx("CHB_PCS.xlsx")

func_pcs <- function(data_frame_in) {
  #Pain Catastrophizing Scale
  #5-stufige Skala: 0 = "trifft überhaupt nicht zu"; 4 = "trifft immer zu"
  #Gesamtskala mit 13 Items sowie drei Subskalen „Grübeln“ (G), " Vergrößerung (V) und „Hilfslosigkeit“ (H)
  #Kennwert ist der Summerwert aller Items einer Skala; 
  #Bei fehlenden Werten: fehlenden Wert durch den entsprechenden Skalenmittelwert der Untersuchungsperson schätzen 
  
  pcs_names = c("ID", c(paste0(rep("pcs_0"), 1:9), paste0(rep("pcs_"), 10:13))) 
  FB = data_frame_in
  names(FB) = pcs_names
  
  if(length(names(FB)) != length(pcs_names)){
    stop("ERROR THIS IS NOT THE CORRECT NUMBER OF INPUT ITEMS!!! WRONG RESULTS!!! Analysis cancelled...")
  }
  
  if(sum(sapply(data_frame_in, class)[2:14] == "numeric") + sum(sapply(data_frame_in, class)[2:14] == "logical") != 13){
    stop("ERROR THE ITEMS OF THE PCS NEED TO BE NUMERIC!!! Analysis cancelled...")
  }
  
  if(max(data_frame_in[,c(2:14)], na.rm = T) > 4 |  min(data_frame_in[,2:14], na.rm = T) < 0){
    stop("ERROR THE ITMES OF THE PCS NEED TO BE IN A RANGE FROM 0 TO 4!!! Analysis cancelled...")
  }
  
  #### PCS #################################################################
  
  pcs_G <- c(paste0(rep("pcs_0"), c(8:9)), paste0(rep("pcs_"), c(10:11)))       #Subscale "Grübeln"
  pcs_V <- c(paste0(rep("pcs_0"), c(6:7)), "pcs_13")                            #Subscale „Vergrößerung"
  pcs_H <- c(paste0(rep("pcs_0"), c(8:9)), "pcs_12")                            #Subscale „Hilfslosigkeit"
  
  FB$PCS <- rowMeans(FB[,-1], na.rm = TRUE)
  FB$Gruebeln <- rowMeans(FB[pcs_G], na.rm = TRUE)
  FB$vergroeßerung <- rowMeans(FB[pcs_V], na.rm = TRUE)
  FB$Hilfslosigkeit <- rowMeans(FB[pcs_H], na.rm = TRUE)
  
  FB = subset(FB, select = c(ID, PCS, Gruebeln, vergroeßerung, Hilfslosigkeit))
  return(FB)
}

CHB_PCS <- func_pcs(CHB_PCS)

mainDF_together4 <- data.frame()

for (i in 1:(nrow(CHB_PCS))) {
  mean_value <- CHB_PCS$PCS[i]
  subject <- unique(mainDF$subj_idx)
  mainDF_subset <- mainDF[mainDF$subj_idx == subject[i],]
  mainDF_subset$PCS <- mean_value
  mainDF_together4 <- rbind(mainDF_together4, mainDF_subset)
}

mainDF$PCS <- mainDF_together4$PCS

#==============================================================================#
######### bayesian correlation of individual bias parameters and PCS ###########
#==============================================================================#

unique_PCS <- aggregate(PCS ~ subj_idx, data = mainDF, FUN = unique, na.rm = TRUE)
corr_df <- cbind(unique_PCS, corr_df)

bf7 = correlationBF(corr_df$PCS, corr_df$IBP_dc)
samples = posterior(bf7, iterations = 10000)
summary(samples)
correlationBF(corr_df$PCS, corr_df$IBP_dc)

