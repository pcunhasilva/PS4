###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 4
## Author: Patrick Cunha Silva
## Tasks: 1 - Scrap First Wiki and clean the data.
##        2 - Scrap Second Wiki, clean the data, merge and save dataset in a .RData file.
##        3 - Generate the plots and save them in a .pdf file.
##        4 - Describe the plots.

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
colnames(pop_vote_margin) <- c("election_number",  "election_year", "w_name", "w_party",
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

# Merge data and rename variables:
# Winner's
complete_data <- merge(pop_vote_margin, votesElecCollege, 
                       by.x = c("w_name", "election_year"),
                       by.y = c("candidate", "election_year"))
colnames(complete_data)[ncol(complete_data)] <- "w_elec_col"


# Runner up's 
complete_data <- merge(complete_data, votesElecCollege, 
                       by.x = c("rup_name", "election_year"),
                       by.y = c("candidate", "election_year"))
colnames(complete_data)[ncol(complete_data)] <- "rup_elec_col"

# Reorder variables
names_var <- names(pop_vote_margin)
complete_data <- complete_data[,c(names_var, "w_elec_col", "rup_elec_col")]

# Sort data.frame
order_vec <- order(complete_data[,1])
complete_data <- complete_data[order_vec,]

# Save data.frame in a .R file
setwd("~/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5625 - Applied Statistical Programming/Homeworks/Problem Sets/PS4")
save(complete_data, file = "complete_data.RData")

######################################################
# Plots
# Attach the dataset.
attach(complete_data)
# Define plot's name
pdf("Trends_on_Vote_and_turnout.pdf")
# Define plot's parameters and layout
par(mar=c(4, 4, 3, 4))
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))

# Tendence of winner % Popular Vote over time 
plot(x = election_year, y = w_pop_votes_perc,
     type = "p", pch = 19, cex = 0.4,
     ylim = c(0, 100), axes = FALSE,
     xlab = "Election Year",
     ylab = "% of Votes",
     main = "Percentage of Popular and \n Electoral College Votes")
lines(x = election_year, y = w_pop_votes_perc, lty = 1)
# Tendence of winner % Electoral College Vote 
points(x = election_year, y = w_votes_elec_col_perc,
      pch = 17, cex = 0.4)
lines(x = election_year, y = w_votes_elec_col_perc, lty = 2)
# Add axis
axis(side = 1, at = c(1824, 1856, 1888, 1920, 1952, 1984, 2016))
axis(side = 2, at = c(0, 25, 50, 75, 100))
# Add a legend
legend("bottomright",
       legend = c("Popular Vote (Winner)", "Electoral College (Winner)"),
       lty = c(1,3),
       pch = c(19, 17),
       cex = .8)

# Popular Vote Margin and Turnout over time
# Plot Popular vote margin over time
plot(x = election_year, y = w_pop_votes_margin_perc,
     type = "p", pch = 19, cex = 0.2, axes = FALSE,
     ylim = c(-10, 30),
     xlab = "Election Year",
     ylab = "% of Votes",
     main = "Popular Vote Margin \n and Turnout over time",
     col = "blue")
# Add lines for Popular vote margin over time
lines(x = election_year, y = w_pop_votes_margin_perc,
      lty = 1,
      col = "blue")
# Add the left axis
axis(side = 2, at = seq(-10, 30, length.out = 3))
# Plot turnout over time
par(new = TRUE)
plot(x = election_year, turnout, type = "p",
     pch = 19, cex = 0.2, axes = FALSE,
     xlab = NA,
     ylab = NA,
     col = "red")
# Add lines for turnout over time.
lines(x = election_year, y = turnout,
      lty = 1,
      col = "red")
# Add the right axis
axis(side = 4, at = seq(0, 100, length.out = 5))
mtext(side = 4, line = 3, 
      text = "Turnout")
# Add bottom axis
axis(side = 1, at = c(1824, 1856, 1888, 1920, 1952, 1984, 2016))
# Add legend
legend("bottomright",
       legend = c("Popular Vote Margin (Winner)", "Turnout"),
       lty = c(1,1),
       col = c("blue", "red"),
       cex = .7)
# Close the file
dev.off()
# Detach dataset.
detach(complete_data)

# Explanation about the plots:




# Define a new layout
layout(matrix(c(1,1,1,1), 2, 2, byrow = TRUE))
# % Electoral College votes per party
plot(x = election_year, y = w_votes_elec_col_perc,
     type = "l", axes = FALSE,
     ylim = c(0, 100),
     xlim = c(1824, 2016),
     xlab = "Election Year",
     ylab = "% Electoral College votes",
     main = "Electoral College votes \n per party over time")
# Add left axis
axis(side = 2, at = seq(0, 100, length.out = 5))
# Add bottom axis
axis(side = 1, at = c(1824, 1856, 1888, 1920, 1952, 1984, 2016))
# Add points for Democrats
points(x = election_year[w_party=="Dem."], 
       y = w_votes_elec_col_perc[w_party=="Dem."],
       pch = 19, cex = 0.5, col = "blue")
# Add Republicans
points(x = election_year[w_party=="Rep."], 
       y = w_votes_elec_col_perc[w_party=="Rep."],
       pch = 19, cex = 0.5, col = "red")
# Democratic-Republican
points(x = election_year[w_party=="D.-R."], 
       y = w_votes_elec_col_perc[w_party=="D.-R."],
       pch = 19, cex = 0.5, col = "green")
# Whig
points(x = election_year[w_party=="Whig"], 
       y = w_votes_elec_col_perc[w_party=="Whig"],
       pch = 19, cex = 0.2, col = "yellow")
# Add legend
legend("bottomright",
       legend = c("Democracts", "Republican", 
                  "Democratic-Republican", "Whig"),
       col = c("blue", "red", "green", "yellow"),
       pch = c(19, 19, 19, 19),
       cex = .6)

