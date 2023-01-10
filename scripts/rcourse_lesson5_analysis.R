############
# Stefan Pophristic
# STATS ANOVA Course — Lesson 5
# Jan. 9, 2023
# Script for analysis
############

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## READ IN DATA ####
source("rcourse_lesson5_cleaning.R")

## LOAD PACKAGES ####
library(tidyr)
library(ez)


## ORAGANIZE DATA ####
# Make data for statistics
data_stats = data_clean %>%
  mutate(civil_war = factor(civil_war, levels = c("union", "confederacy"))) %>%
  group_by(state, incumbent_party, civil_war) %>%
  summarise(perc_incumbent_mean = mean(perc_votes_incumbent, na.rm = T)) %>%
  ungroup()

# Check if incumbent party is within-state
xtabs(~state+incumbent_party, data_stats)
# yes


# Check if civil war is within-state
xtabs(~state+civil_war, data_stats)
# no

## BUILD MODELS ####
# ANOVA (base R)
incumbent.aov = aov(perc_incumbent_mean ~ incumbent_party * civil_war +
                      Error(state/incumbent_party), data = data_stats)

incumbent.aov_sum = summary(incumbent.aov)
incumbent.aov_sum

# ezANOVA
incumbent.ezanova = ezANOVA(data.frame(data_stats),
                            dv = perc_incumbent_mean,
                            wid = state,
                            within = incumbent_party,
                            between = civil_war,
                            type = 3)

incumbent.ezanova

# Prepare data for t-test
data_union_stats = data_stats %>%
  filter(civil_war == "union") %>%
  spread(incumbent_party, perc_incumbent_mean)

data_confederacy_stats = data_stats %>%
  filter(civil_war == "confederacy") %>%
  spread(incumbent_party, perc_incumbent_mean)

data_democrat_stats = data_stats %>%
  filter(incumbent_party == "democrat")

data_republican_stats = data_stats %>%
  filter(incumbent_party == "republican")

## FOLLOW-UP T-TESTS ####
# Effect of incumbent party, separated by civil war
incumbent_union.ttest = t.test(data_union_stats$democrat,
                               data_union_stats$republican,
                               paired = T)
incumbent_union.ttest
# t = 2.3477, df = 10, p-value = 0.0408

incumbent_confederacy.ttest = t.test(data_confederacy_stats$democrat,
                                     data_confederacy_stats$republican,
                                     paired = T)
incumbent_confederacy.ttest
# t = -7.5325, df = 10, p-value = 1.987e-05


# Effect of incumbent party, separated by civil war
incumbent_democrat.ttest = t.test(perc_incumbent_mean ~ civil_war,
                                  paired = F,
                                  data = data_democrat_stats)
incumbent_democrat.ttest
# t = 5.4386, df = 18.774, p-value = 3.14e-05

incumbent_republican.ttest = t.test(perc_incumbent_mean ~ civil_war,
                                    paired = F,
                                    data = data_republican_stats)
incumbent_republican.ttest
# t = -6.0086, df = 14.529, p-value = 2.741e-05

