###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 4
## Author: Patrick Cunha Silva
## Tasks: 1 - XXXXXX.

# Clean session before starting
rm(list = ls())
library(rvest)

# Define the URL
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

# Read the URL
temp <- wikiURL %>% 
   read_html %>%
   html_nodes("table") %>%
   html_table()

# Select the table of interest
table1 <- temp[[2]]

# Add collunm names:
colnames(table1)[1] <- "id"
colnames(table1)[2] <- "election_year"
colnames(table1)[3] <- "w_name"
colnames(table1)[4] <- "w_party"
colnames(table1)[5] <- "w_votes_elec_col"
colnames(table1)[6] <- "w_votes_elec_col_perc"
colnames(table1)[7] <- "w_pop_votes_perc"
colnames(table1)[8] <- "w_pop_votes_margin_perc"
colnames(table1)[9] <- "w_pop_votes"
colnames(table1)[10] <- "w_pop_votes_margin"
colnames(table1)[11] <- "l_name"
colnames(table1)[12] <- "l_party"
colnames(table1)[13] <- "turnout"

# Clean and recode the data
## id
table1[,1] <- as.numeric(table1[,1])
## w_name
table1[,3] <- gsub(",", ", ", table1[,3])
## w_

#####
table1 <- table1[-c(1,2),]
table1[, 7] <- gsub("\\[|a\\]", "", table1[,7])
table1[, 7] <- gsub("%", "", table1[,7])

