library(data.table)
library(ggplot2)
library(dplyr)

batting <- read.csv('Batting.csv')
sal <- read.csv('Salaries.csv')

#Create four new columns in the Batting data frame; Batting Average (BA), On Base % (OBP), Singles (X1B) and Slugging % (SLG)
batting$BA <- batting$H / batting$AB
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B <- (batting$H-batting$X2B-batting$X3B-batting$HR)
batting$SLG <- (((batting$X1B)+(2 * batting$X2B)+(3 * batting$X3B)+(4 * batting$HR))/batting$AB)

# Merge the Salaries df with Batting df. However, since Salaries df starts from 1985, we will need to trim Batting df to start from 1985
batting_1985_onwards <- subset(batting,yearID >= 1985)
# Use two common IDs (i.e. PlayerID and YearID) to combine two df.
combo <- merge(batting_1985_onwards,sal,by=c('playerID','yearID'))

# Extract out the details of the lost players, Giambi, Damon and Isringhausen from the 'combo' df
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))

# Since they left in 2001, we can further narrow them down to 2001.
lost_players <- subset(lost_players,yearID==2001)

# We extract the data required for the analysis. I.e. PlayerID, H, X2B, X3B, HR, OBP, SLG, BA, AB
select(lost_players,playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB)

# So the challenge is to find 3 players to replace the lost players. Requirements are:
# Combined salary cannot be > $15m
# Combined AB needs to be equal or more than the lost players (i.e. 1,469)
# Mean OBP needs to be equal or more than the mean OBP of lost players (0.364)

# So we grab the list of players from combo with the YearID being 2001.

combo <- subset(combo,yearID == 2001)

# plot out the OBP vs Salary
OBP_Sal <- ggplot(combo, aes(x=OBP,y=salary)) + geom_point(size=2)
print(OBP_Sal)

# plot out the OBP vs AB
OBP_AB <- ggplot(combo, aes(x=OBP,y=AB)) + geom_point(size=2)
print(OBP_AB)
# Based on the graph, we can see that there is a group of players with AB > 600.

# So we extract out the group of players with OBP > 0 and AB > 600.
OBP0_AB600 <- filter(combo,AB >= 600,OBP >=0.364)
print(OBP0_AB600 %>% select(playerID,AB,OBP,salary) %>% arrange(desc(AB),desc(OBP)))

# Based on the table, we could go for suzukic01, stewash01 and aurilri01 with:
# Combined salary of $11.1m (< $15m)
# Combined AB of 1,968 (>= 1,469)
# Mean OBP of 0.374 (>= 0.364)

