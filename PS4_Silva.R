###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 4
## Author: Patrick Cunha Silva
## Tasks: 1 - Scrap First Wiki and Clean the Data.
## Tasks: 2 - Scrap Second Wiki, Clean the Data, Merge and Save dataset.

# Clean session before starting
rm(list = ls())
library(rvest)
library(htmltab)

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
                      "votes_elec_col", "w_votes_elec_col_perc",
                      "w_pop_votes_perc", "w_pop_votes_margin_perc",
                      "w_pop_votes", "w_pop_votes_margin", "rup_name",
                      "rup_party",  "turnout")
    
# For numeric variables:
cleanNum <- function(x){
   x <- gsub("\\[|a\\]", "", x)
   x <- gsub("%", "", x)
   x <- gsub("−", "-", x)
   x <- gsub(",", "", x)
   x <- as.numeric(x)
}

# For names (winner and runner-up) variables
cleanNames <- function(x){
   x <- gsub("\\[.*", "", x)
   x <- gsub(".*,", "", x)
   x <- gsub("\n.*", "", x)
   x <- gsub("N/A", "", x)
   x <- gsub("[A-Z]. ", "", x)
   x
}

# Clean numeric variables
pop_vote_margin[, c(1, 2, 6, 7, 8,9, 10, 13)] <- apply(pop_vote_margin[, c(1, 2, 6, 7, 8,9, 10, 13)], 
                                                       2, cleanNum) 
# Clean name variables
pop_vote_margin[, c(3, 11)] <- apply(pop_vote_margin[, c(3, 11)], 
                                     2, cleanNames)

#############################################

# Define the second URL
wikiURL2 <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

# Read the URL
votesElecCollege  <- htmltab(wikiURL2, which = 3)

# Convert year to numeric and change name
names(votesElecCollege)[1] <- "election_year"

# Drop observations before 1824
votesElecCollege <- votesElecCollege[which(votesElecCollege[,1]==1824)[1]:nrow(votesElecCollege),]

# Generate variable with the number of electoral votes
votesElecCollege$elec_col_votes_cand <- as.numeric(gsub(" .*", "", votesElecCollege[, 7]))

# Generate variable with the total number of electoral votes 
votesElecCollege$elec_col_votes <- as.numeric(gsub(".* ", "", votesElecCollege[, 7]))

# Keep only election year, presidential candidate, total number of electoral votes,
# and electoral votes.
votesElecCollege <- votesElecCollege[, c(1, 3, 9, 10)]

# Generate electoral votes per candidate
votesElecCollege <- aggregate(votesElecCollege[, 3], 
                     by = list(candidate = votesElecCollege[, 2], 
                                election_year = votesElecCollege[, 1]), FUN=sum)

# Remove middle name abbreviations
votesElecCollege[,1] <- cleanNames(votesElecCollege[,1])

# Recode candidates names. All the names were recoded to match with the names 
# in the pop_vote_margin data.frame:
votesElecCollege[,1][votesElecCollege[,1]=="William Howard Taft"] <- "William Taft"
votesElecCollege[,1][votesElecCollege[,1]=="Alton Parker"] <- "Alton Brooks Parker"
votesElecCollege[,1][votesElecCollege[,1]=="Adlai Stevenson II"] <- "Adlai Stevenson"
votesElecCollege[,1][votesElecCollege[,1]=="Adlai Stevenson II"] <- "Adlai Stevenson"

# Merge data:
# Winner's
temp <- votesElecCollege
names(temp)[1] <- "w_name"
names(temp)[3] <- "w_elec_col"
complete_data <- merge(pop_vote_margin, temp, by = c("w_name", "election_year"))
# Runner up's 
temp <- votesElecCollege
names(temp)[1] <- "rup_name"
names(temp)[3] <- "rup_elec_col"
complete_data <- merge(complete_data, temp, by = c("rup_name", "election_year"))

# Reorder variables
names_var <- names(pop_vote_margin)
complete_data <- complete_data[,c(names_var, "w_elec_col", "rup_elec_col")]

# Save data.frame in a .R file
save(complete_data, file = "complete_data.RData")


