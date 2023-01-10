############
# Stefan Pophristic
# STATS ANOVA Course — Lesson 5
# Dec. 16, 2022
# Script for cleaning data
############

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## LOAD PACKAGES ####
library(dplyr)
library(purrr)

## READ IN DATA ####
# Full data on election results
data_election_results = list.files(path = "../data/elections", full.names = T) %>%
  map(read.table, header = T, sep = "\t") %>%
  reduce(rbind)

# Read in extra data about specific elections
data_elections = read.table("../data/rcourse_lesson5_data_elections.txt", header=T, sep="\t")

# Read in extra data about specific states
data_states = read.table("../data/rcourse_lesson5_data_states.txt", header=T, sep="\t")

# See how many states in union versus confederacy
xtabs(~civil_war, data_states)


## CLEAN DATA ####

# Make data set balanced for Union and Confederacy states
data_states_clean = data_states %>%
  filter(!is.na(civil_war)) %>%
  group_by(civil_war) %>%
  arrange(order_enter) %>%
  filter(row_number() <= 11) %>%
  ungroup()

# The way the Piccini chose which union states to include may introduce bias
# first states to join may have different political/cultural/historical trends
# than states that joined later

# Double check balanced for 'civil_war' variable
xtabs(~civil_war, data_states_clean)

# Combine three data frames
data_clean = data_election_results %>%
  inner_join(data_elections) %>%
  inner_join(data_states_clean) %>%
  mutate(state = factor(state))

# Double check all of numbers are balanced
xtabs(~incumbent_party+civil_war, data_clean)

