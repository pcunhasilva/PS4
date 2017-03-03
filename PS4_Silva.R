###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 4
## Author: Patrick Cunha Silva
## Tasks: 1 - Scrap Wiki and Clean the Data.

# Clean session before starting
rm(list = ls())
library(rvest)
library(dplyr)

# Define the URL
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

# Read the URL
temp <- wikiURL %>% 
   read_html %>%
   html_nodes("table") %>%
   html_table()

# Select the table of interest
pop_vote_margin <- temp[[2]]
pop_vote_margin <- pop_vote_margin[-c(1,2),]

# Add collunm names:
colnames(pop_vote_margin) <- c("id",  "election_year", "w_name", "w_party",
                      "w_votes_elec_col", "w_votes_elec_col_perc",
                      "w_pop_votes_perc", "w_pop_votes_margin_perc",
                      "w_pop_votes", "w_pop_votes_margin", "l_name",
                      "l_party",  "turnout")
    
# For numeric variables:
cleanNum <- function(x){
   x <- gsub("\\[|a\\]", "", x)
   x <- gsub("%", "", x)
   x <- gsub("−", "-", x)
   x <- gsub(",", "", x)
   x <- as.numeric(x)
}

# For names (winner and loser) variables
cleanNames <- function(x){
   x <- gsub("\\[|a\\]", "", x)
   x <- gsub(".*,", "", x)
}

# Clean numeric variables
pop_vote_margin[, c(1, 2, 6, 7, 8,9, 10, 13)] <- apply(pop_vote_margin[, c(1, 2, 6, 7, 8,9, 10, 13)], 2, 
                                              cleanNum) 
# Clean name variables
pop_vote_margin[, c(3, 11)] <- apply(pop_vote_margin[, c(3, 11)], 2, cleanNames)

