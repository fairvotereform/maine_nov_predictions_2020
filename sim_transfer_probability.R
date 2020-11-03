library(tidyverse)

source("predictions.R")

# read in poll summaries
senate_poll <- read_csv("poll_summary/senate.csv")
president_d1_poll <- read_csv("poll_summary/president_d1.csv")
president_d2_poll <- read_csv("poll_summary/president_d2.csv")
president_d12_poll <- read_csv("poll_summary/president_d12.csv")

senate_transfer(senate_poll) %>% 
  write_csv("transfer_probabilities/senate.csv")

president_transfer(president_d1_poll) %>% 
  write_csv("transfer_probabilities/president_d1.csv")

president_transfer(president_d2_poll) %>% 
  write_csv("transfer_probabilities/president_d2.csv")

president_transfer(president_d12_poll) %>% 
  write_csv("transfer_probabilities/president_d12.csv")