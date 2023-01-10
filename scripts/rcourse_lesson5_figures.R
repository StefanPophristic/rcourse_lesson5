############
# Stefan Pophristic
# STATS ANOVA Course — Lesson 5
# Jan. 9, 2023
# Script for figures
############

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## READ IN DATA ####
source("rcourse_lesson5_cleaning.R")

## LOAD PACKAGES ####
library(ggplot2)


## ORGANIZE DATA ####
data_figs = data_clean %>%
  mutate(civil_war = factor(civil_war,
                            levels = c("union", "confederacy"),
                            labels = c("Union", "Confederacy"))) %>%
  mutate(incumbent_party = factor(incumbent_party,
                                  levels = c("democrat", "republican"),
                                  labels = c("Democrat", "Republican")))

# Average data over years but not states
data_figs_state_sum = data_figs %>%
  group_by(state, incumbent_party, civil_war) %>%
  summarise(perc_incumbent_mean =
              mean(perc_votes_incumbent, na.rm = T)) %>%
  ungroup()

# Data averaged over year and states for barplot
data_figs_sum = data_figs_state_sum %>%
  group_by(incumbent_party, civil_war) %>%
  summarise(mean = mean(perc_incumbent_mean, na.rm = T),
            sd = sd(perc_incumbent_mean, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)


## MAKE FIGURES ####

# Histogram of full data set
incumbent_histogram_full.plot = ggplot(data_figs, aes(x = perc_votes_incumbent,
                                                      fill = incumbent_party)) +
  geom_histogram(bins = 10) +
  facet_grid(incumbent_party ~ civil_war) +
  scale_fill_manual(values = c("blue", "red"))

pdf("../figures/incumbent_histogram_full.pdf")
incumbent_histogram_full.plot
dev.off()


# Histogram of data averaged over years
incumbent_histogram_sum.plot = ggplot(data_figs_state_sum, aes(x = perc_incumbent_mean,
                                                               fill = incumbent_party)) +
  geom_histogram(bins = 10) +
  facet_grid(incumbent_party ~ civil_war) +
  scale_fill_manual(values = c("blue", "red"))

pdf("../figures/incumbent_histogram_sum.pdf")
incumbent_histogram_sum.plot
dev.off()

# Boxplot
incumbent_boxplot.plot = ggplot(data_figs_state_sum, aes(x = civil_war,
                                                         y = perc_incumbent_mean,
                                                         fill = incumbent_party)) +
  geom_boxplot() +
  ylim(0, 100) +
  geom_hline(yintercept = 50) +
  scale_fill_manual(values = c("blue", "red"))

pdf("../figures/incumbent_boxplot.pdf")
incumbent_boxplot.plot
dev.off()

# Barplot
incumbent_barplot.plot = ggplot(data_figs_sum, aes(x = civil_war,
                                                   y = mean,
                                                   fill = incumbent_party)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = se_low, ymax = se_high),
                width = 0.2,
                position = position_dodge(0.9)) +
  ylim(0, 100) +
  geom_hline(yintercept = 50) +
  scale_fill_manual(values = c("blue", "red"))

pdf("../figures/incumbent_barplot_sub.pdf")
incumbent_barplot.plot
dev.off()

