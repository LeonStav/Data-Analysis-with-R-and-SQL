#install.packages("RSQLite")
#install.packages("dplyr")
library(DBI)
library(tidyverse)

################################################################################
#Ερώτημα 1ο
#Φορτώνω το dataset σε μια μεταβλητή
football <- dbConnect(RSQLite::SQLite(), "C:/Users/User/Desktop/leon/SQL/Εργασία/database.sqlite")
football

################################################################################
#Ερώτημα 2ο
#Ελέγχο τί περιέχει το dataset 
dbListTables(football)


#Διαβάζω τους όλους τους πίνακες ξεχωριστά
dbReadTable(football, "Country")
str(dbReadTable(football, "Country"))

dbReadTable(football, "League")
str(dbReadTable(football, "League"))

dbReadTable(football, "Match")
str(dbReadTable(football, "Match"))

dbReadTable(football, "Player")
str(dbReadTable(football, "Player"))

dbReadTable(football, "Player_Attributes")
str(dbReadTable(football, "Player_Attributes"))

dbReadTable(football, "Team")
str(dbReadTable(football, "Team"))

dbReadTable(football, "Team_Attributes")
str(dbReadTable(football, "Team_Attributes"))

dbReadTable(football, "sqlite_sequence")
str(dbReadTable(football, "sqlite_sequence"))

################################################################################
#Ερώτημα 3ο 
#Χρησιμοποιώ pointers μεσω της tbl για να εισάγω τους πίνακες σε μεταβλητές

country <- tbl(football, "Country")
head(country)
dim(country)

league <- tbl(football, "League")
head(league)
dim(league)

match <- tbl(football, "Match")
head(match)
dim(match)

player <- tbl(football, "Player")
head(player)
dim(player)

player_attributes <- tbl(football, "Player_Attributes")
head(player_attributes)
dim(player_attributes)

team <- tbl(football, "Team")
head(team)
dim(team)
glimpse(team)

team_attributes <- tbl(football, "Team_Attributes")
head(team_attributes)
dim(team_attributes)

################################################################################
#Ερώτημα 4ο
library(dplyr)

#Ερώτημα πρώτο SQL
firstq <- player%>% 
  filter( height > 160, height < 180)%>%
  arrange(desc(weight))%>%
  select(player_name, height, weight)

glimpse(firstq)
firstq

#Ερώτημα δεύτερο SQL
secondq <- match%>%
  left_join(team, by = c("home_team_api_id" = "team_api_id"))%>%
  filter(match_api_id == 492473)%>%
  select(date, team_long_name, home_team_goal)%>%
  rename(Home_team = team_long_name)

glimpse(secondq)
secondq

#Ερώτημα τρίτο SQL
thirdq <- match%>%
  left_join(team, by = c("home_team_api_id" = "team_api_id"))%>%
  left_join(country, by = c("country_id" = "id"))%>%
  select(team_long_name, name)%>%
  rename(Country_name = name)%>%
  rename(Team_name = team_long_name)%>%
  distinct(Team_name, Country_name)

glimpse(thirdq)
thirdq
count(thirdq)



#Ερώτημα τέταρτο SQL
fourthq1 <- match%>%
  filter(match_api_id == 492473)%>%
  left_join(team, by = c("home_team_api_id" = "team_api_id"))%>%
  rename(Home_team = team_long_name)%>%
  select(date, Home_team, home_team_goal)

glimpse(fourthq1)
fourthq1

fourthq2 <- match%>%
  filter(match_api_id == 492473)%>%
  left_join(team, by = c("away_team_api_id" = "team_api_id"))%>%
  rename(Away_team = team_long_name)%>%
  select(Away_team, away_team_goal)

glimpse(fourthq2)
fourthq2

fourthq <- merge(fourthq1, fourthq2)

glimpse(fourthq)
fourthq

################################################################################
#Ερώτημα 5ο
#Υποερώτημα α
#Βρείτε πόσοι παίκτες είναι αριστεροπόδαροι και πόσοι δεξιοπόδαροι (Θα
#χρησιμοποιήσετε την στήλη preferred_foot του πίνακα player_attributes καθώς
#και τις συναρτήσεις group_by και summarize). Να αφαιρεθούν τα NA εφόσον
#υπάρχουν.

glimpse(player_attributes)

pref_foot <- player_attributes%>%
  group_by(preferred_foot)%>%
  summarize(count = count(preferred_foot))%>%
  filter(!is.na(preferred_foot))

pref_foot

#Υποερώτημα β
#Χρησιμοποιήστε τη βιβλιοθήκη ggplot2 και κατασκευάστε ένα απλό
#ραβδόγραμμα για τη μεταβλητή preferred_foot του πίνακα player_attributes
library(ggplot2)

player_attributes.new<-data.frame(player_attributes)
player_attributes.new<-player_attributes.new[!is.na(player_attributes.new$preferred_foot), ]
player_attributes.new

glimpse(player_attributes.new$preferred_foot)
str(player_attributes.new)

ggplot(player_attributes.new, aes(x = preferred_foot))+
  geom_bar()+
  labs(x="Preferred foot", y="Count")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")



################################################################################
#Ερώτημα 6ο
#a. Με χρήση της ggplot2 κάντε ένα διάγραμμα διασποράς για τις μεταβλητές ύψος
#(height) και βάρος (weight) των παικτών (πίνακας player)

ggplot(player, aes(x = weight, y = height)) +
  geom_point()

#b.Βελτιώσετε το παραπάνω γράφημα έτσι ώστε να αποφύγετε το overplotting

#Αλλάζουμε το alpha και το σχήμα για να αποφύγουμε το overplotting μικραίνοντας
#το σχήμα των παρατηρήσεων και κάνοντας το κενούς κύκλους++
ggplot(player, aes(x = weight, y = height)) +
  geom_point(alpha=0.6, shape = 1)
  
#Κλείνουμε την σύνδεση με την βάση δεδομένων
dbDisconnect(football)
