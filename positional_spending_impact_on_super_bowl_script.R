#Title: Analysis of Positional Spending by Super Bowl Teams 2014-2023
#Author: Joshua Sipple
#Date: 23 MAY 2024

#Description: I have gathered data from multiple websites (see below) on 
#the strategic use of cap space by teams in the NFL that made it to 
#the Super Bowl from 2014 to 2023. I focused my analysis on the amount of cap 
#allocated to the Quarterback (QB), Wide Receiver (WR), Offensive Line (OLINE),
#and Defensive End/Edge Pass Rusher (EDGE) positions, as these are widely 
#considered to be premium positions. I also included the Running Back (RB) 
#position, as the salaries of this position have been the source of much 
#controversy over the last several years. Additionally, I have averaged 
#the amounts of cap allocated to each position over the last 10 years, 
#so any interested party can easily check to see if their team has followed a
#similar strategy in 2024.

#Data: The data is sourced from reputable sports websites (see below). In this 
#script, I eliminate unnecessary data before cleaning and formatting the 
#remaining data to be used in my analyses.

#Websites Used:
# https://www.topendsports.com/events/super-bowl/winners-list.htm
# https://overthecap.com/positional-spending
# https://www.spotrac.com/nfl/cba

#Install tidyverse package
install.packages("tidyverse")
install.packages("scales")
install.packages("writexl")

#Load tidyverse package
library(tidyverse)
library(scales)
library(writexl)



##Data Source1
##Scrape and select Positional Spending data from Over the Cap website 
#Read html code for Over the Cap website
content <- read_html("https://overthecap.com/positional-spending")

#Pull all Positional Spending (ps) tables from Over the Cap website
ps_tables <- content %>% html_table()

#Eliminate positions we are not interested in
for(i in 2:12){
  ps_tables[[i]] <- ps_tables[[i]][,c("Team", "QB", "RB", "WR", "OL", "EDGE")]
}

#Create one dataframe for 2024 teams to use for comparison later
current_year_ps <- ps_tables[[12]]

#NOTE: ps_tables[[2]] corresponds to 2014 data 
#ps_tables[[12]] corresponds to 2024 data



##Data Source2
##Scrape and select Salary Cap data from Spotrac website 
#Read html code for Spotrac website
content2 <- read_html("https://www.spotrac.com/nfl/cba")

#Pull all Salary cap tables from Over the Cap website
cap_tables <- content2 %>% html_table()

#Create dataframe with Salary Cap from 1994-2024
assign(paste0("cap_94to24"), cap_tables[[2]])

#Eliminate unnecessary data. Only need Cap data from 2014-2023
cap_24to14 <- cap_94to24[1:11,1:2]

# Calculating reverse, so years will list from 2014 to 2023
rev_data_frame1 <- apply(cap_24to14, 2, rev)

# Converting the result to dataframe
cap_14to24 <- as.data.frame(rev_data_frame1)
names(cap_14to24) <- str_replace_all(names(cap_14to24), c(" " = "_"))



##Data Source3
##Scrape and select Super Bowl winners and losers data from Top End Sports website 
#Read html code for Top End Sports website
content3 <- read_html("https://www.topendsports.com/events/super-bowl/winners-list.htm")

#Pull all Super Bowl History tables from Top End Sports website
sb_tables <- content3 %>% html_table()

#Create dataframe with Super Bowl History from 1967-2023
assign(paste0("sb_67to23"), sb_tables[[1]])

#Eliminate unnecessary data. Only need Super Bowl history from 2014-2023
sb_23to14 <- sb_67to23[1:10,c(1,3,4)]

#Super Bowls are played in Feb the year after the season.
#Subtracting 1 from year of each Super Bowl victory to standardize all data
sb_23to14[, 1] <- sb_23to14[, 1] - 1

# Calculating reverse, so years will list from 2014 to 2023
rev_data_frame2 <- apply(sb_23to14, 2, rev)

# Converting the result to dataframe
sb_14to23 <- as.data.frame(rev_data_frame2)

#Need function to extract team name only to match other dataframes
get_last_word <- function(text) {
  words <- strsplit(text, " ")[[1]]
  return(tail(words, 1))
}

#Convert team city-name to team name for Winners and Opponents
for (i in 1:nrow(sb_14to23)){
  sb_14to23[i,2] <- get_last_word(sb_14to23[i,2])
  sb_14to23[i,3] <- get_last_word(sb_14to23[i,3])
}

#Add a row for current year's Super Bowl
sb24 <- c(2024, "TBD", "TBD")
sb14to24 <- rbind(sb_14to23, sb24)



#Combine Salary Cap data with Super Bowl History Data
sb_sc <- merge(cap_14to24, sb14to24, on='year', how='outer')



#Create a data frame with Winners info

#Create an array of winning team premium positions
wcolumn_names <- c("WQB", "WQB_PCT", "WRB", "WRB_PCT", "WWR", "WWR_PCT", "WOLINE", "WOLINE_PCT", "WEDGE", "WEDGE_PCT")

# Create a temporary dataframe with 10 rows and 10 columns
winfo <- data.frame(matrix(nrow = 10, ncol = length(wcolumn_names)))
colnames(winfo) <- wcolumn_names

#Function to calculate percentage of cap spent on given position
#Returns rounded percentage
calc_pct <- function(cap_spent_str, cap_max_str){
  
  #Clean data by removing $ and commas
  clean_str1 <- gsub("[\\$,]", "", cap_spent_str)
  clean_str2 <- gsub("[\\$,]", "", cap_max_str)
  
  # Convert to numeric
  cap_spent_num <- as.numeric(clean_str1)
  cap_max_num <- as.numeric(clean_str2)
  
  #Calculate %
  result_as_dec <- (cap_spent_num/cap_max_num) * 100
  result_as_dec_rounded <- round(result_as_dec, 2)
  result_as_pct <- paste0(result_as_dec_rounded, "%")
  return(result_as_pct)
  
  }



#Fill the dataframe with info on premium positions and the percentage of the Salary Cap they take

#Step through the rows of winfo, filling them one at a time with info from 
#    the corresponding year in ps_tables - ps_tables[[2]] = 2014 & ps_tables[[12]] = 2024
for (i in 1:nrow(winfo)){
  
  #Step through the columns of ps_tables once for every 2 steps through 
    #the columns of winfo
  k <- 1
  
  #Step through the columns of winfo
  for (j in 1:ncol(winfo)){
      
      #if j is odd
      if (j %% 2 == 1) {
        
        wteam <- sb_sc[i,3] #identify team that won the SB this year
        row_number <- which(ps_tables[[i+1]]$Team == wteam) #identify the row of this team
        winfo[i,j] <- ps_tables[[i+1]][row_number,k+1] #store winning team data in winfo
        
        #else if j is even
      } else {
        
        wteam <- sb_sc[i,3] #identify team that won the SB this year
        row_number <- which(ps_tables[[i+1]]$Team == wteam) #identify the row of this team
        cap_spent_str <- ps_tables[[i + 1]][row_number, k + 1] #cap spent on this position by this team in this year
        cap_max_str <- sb_sc[i, 2] #Salary Cap this year
        winfo[i, j] <- calc_pct(cap_spent_str, cap_max_str) #calculate percent of cap taken and store
        k <- k + 1
        
        }
      
      }
  
}

#Add a row to the bottom of winfo with averages of all data previously collected
# Function to convert character class elements with $ and commas to numeric values
convert_to_numeric <- function(x) {
  as.numeric(gsub("[\\$,%]", "", x))
}

# Convert elements to numeric
winfo_num <- apply(winfo, 2, convert_to_numeric)

# Calculate column-wise averages
column_averages <- colMeans(winfo_num, na.rm = TRUE)

# Create a new row with the averages
avg_row <- as.data.frame(t(column_averages))

#Convert to $ format for better visual
avg_row$WQB<-dollar(avg_row$WQB)
avg_row$WRB<-dollar(avg_row$WRB)
avg_row$WWR<-dollar(avg_row$WWR)
avg_row$WOLINE<-dollar(avg_row$WOLINE)
avg_row$WEDGE<-dollar(avg_row$WEDGE)

#Add % for better visual
avg_row$WQB_PCT<- paste0(avg_row$WQB_PCT, "%")
avg_row$WRB_PCT<-paste0(avg_row$WRB_PCT, "%")
avg_row$WWR_PCT<-paste0(avg_row$WWR_PCT, "%")
avg_row$WOLINE_PCT<-paste0(avg_row$WOLINE_PCT, "%")
avg_row$WEDGE_PCT<-paste0(avg_row$WEDGE_PCT, "%")

# Add the new row to the dataframe
winfo <- rbind(winfo, avg_row)



#Repeat process for Opposing team info
#Create a data frame with Opponent info
#Create an array of Opponent team premium positions
ocolumn_names <- c("OQB", "OQB_PCT", "ORB", "ORB_PCT", "OWR", "OWR_PCT", "OOLINE", "OOLINE_PCT", "OEDGE", "OEDGE_PCT")

# Create a temporary dataframe with 10 rows and 10 columns
oinfo <- data.frame(matrix(nrow = 10, ncol = length(ocolumn_names)))
colnames(oinfo) <- ocolumn_names

#Utilize calc_pct function from earlier

#Fill the dataframe with info on premium positions and the percentage of the Salary Cap they take

#Step through the rows of oinfo, filling them one at a time with info from
#   the corresponding year in ps_tables - ps_tables[[2]] = 2014 & ps_tables[[12]] = 2024
for (i in 1:nrow(oinfo)){
  
  #Step through th ecolumns of ps_tables once for every 2 steps through
    #the columns of oinfo
  k <- 1
  
  #Step through the columns of oinfo
  for (j in 1:ncol(oinfo)){
    
    #if j is odd
    if (j %% 2 == 1) {
      
      oteam <- sb_sc[i,4] #identify team that lost the SB this year
      row_number <- which(ps_tables[[i+1]]$Team == oteam) #identify the row of this team
      oinfo[i,j] <- ps_tables[[i+1]][row_number,k+1] #store losing team data in oinfo
      
      #else if j is even
    } else {
      
      oteam <- sb_sc[i,4] #identify team that lost the SB this year
      row_number <- which(ps_tables[[i+1]]$Team == oteam)#identify the row of this team
      cap_spent_str <- ps_tables[[i + 1]][row_number, k + 1] #cap spent on this position by this team in this year
      cap_max_str <- sb_sc[i, 2] #Salary Cap this year
      oinfo[i, j] <- calc_pct(cap_spent_str, cap_max_str) #calculate percent of cap taken and store
      k <- k + 1
      
    }
    
  }
  
}

#Add a row to the bottom of oinfo with averages of all data previously collected
# Function to convert character class elements with $, %, and commas to numeric values
convert_to_numeric <- function(x) {
  as.numeric(gsub("[\\$,%]", "", x))
}

# Convert elements to numeric
oinfo_num <- apply(oinfo, 2, convert_to_numeric)

# Calculate column-wise averages
column_averages2 <- colMeans(oinfo_num, na.rm = TRUE)

# Create a new row with the averages
avg_row2 <- as.data.frame(t(column_averages2))

#Convert to $ format for better visual
avg_row2$OQB<-dollar(avg_row2$OQB)
avg_row2$ORB<-dollar(avg_row2$ORB)
avg_row2$OWR<-dollar(avg_row2$OWR)
avg_row2$OOLINE<-dollar(avg_row2$OOLINE)
avg_row2$OEDGE<-dollar(avg_row2$OEDGE)

#Add % for better visual
avg_row2$OQB_PCT<- paste0(avg_row2$OQB_PCT, "%")
avg_row2$ORB_PCT<-paste0(avg_row2$ORB_PCT, "%")
avg_row2$OWR_PCT<-paste0(avg_row2$OWR_PCT, "%")
avg_row2$OOLINE_PCT<-paste0(avg_row2$OOLINE_PCT, "%")
avg_row2$OEDGE_PCT<-paste0(avg_row2$OEDGE_PCT, "%")

# Add the new row to the dataframe
oinfo <- rbind(oinfo, avg_row2)



#Combine the data together
#Separate Opposing teams from winning teams
opponent <- sb_sc[,4]
sb_sc_w <- sb_sc[,1:3]

#Combine the 4 data frames in order of year, salary cap, winning team and info, then opposing team and info
final_sb_sc_info <- cbind(sb_sc_w, winfo, opponent, oinfo)
names(final_sb_sc_info)[names(final_sb_sc_info) == "opponent"] <- "Opponent"


#take a list of current NFL teams and run each of their key position groups 
#spending against the average from the last 10 years
#create a new column as a boolean saying yes they meet all the averages or no. 

#salary cap for 2024
sc24 <- final_sb_sc_info[11,2]
sc24_compare <- convert_to_numeric(sc24)

#Create a data frame with current year positional spending in dollar amounts
#to be converted to percentages
current_year_ps_pct <- current_year_ps

#Use calc_pct function to calculate what percentage of the salary cap is 
#being used.
for (i in 1:nrow(current_year_ps_pct)) {
  
  for (j in 2:length(current_year_ps_pct)) {
    
    current_year_ps_pct[i,j] <- calc_pct(current_year_ps_pct[i,j], sc24)
    
    }
}



#Analysis 1: How many teams are allocating the same percentage or less 
#to all 5 premium positions? 

#Create a new column for boolean values to record results
current_year_ps_pct$under_sb_pct <- NA

#Create a vector of the values we wish to compare against. 
#i.e. the average percentage allocated by each team that won a super bowl 
#to each of the 5 premium positions 
sbwin_pct <- avg_row[,c(2,4,6,8,10)]

#Utilize a function to check if each NFL team's 2024 cap allocations to each of 
#these premium positions is equal to or less than the average allocation of 
#the last 10 Super Bowl winners to the corresponding position. 
#Returns TRUE if the team is spending the same or less than for all 5 positions.
#Returns FALSE if the team is spending more for at least 1 position.
compare_pct <- function(row_current_year){
    
    ps_only <- row_current_year[2:6]
      
    # Convert to numeric
    for (i in length(ps_only)){
      
      ps_only[i] <- convert_to_numeric(ps_only[i])

    }
    
    #Use boolean to check if spending is equal to or less than for 5/5 positions
    less_than_sbwin_pct <- all(ps_only <= sbwin_pct)
    
    #Return boolean
    return(less_than_sbwin_pct)
    
  }

#Use for loop to apply function to each team
for (i in 1:nrow(current_year_ps_pct)){
  
  current_year_ps_pct[i,7] <- compare_pct(current_year_ps_pct[i,])
  
}



#Analysis 2: How many teams are allocating the same percentage or less 
#to 4 out of 5 premium positions?

#Create a new column for boolean values to record results
current_year_ps_pct$under_sb_pct_4_of_5 <- NA

#Utilize a function to check if each NFL team's 2024 cap allocations to each of 
#these premium positions is equal to or less than the average allocation of 
#the last 10 Super Bowl winners to the corresponding position. 
#Returns TRUE if the team is spending the same or less than for 4/5 positions.
#Returns FALSE if the team is spending more for at least 2 positions.
compare_pct_4_of_5 <- function(row_current_year){
  
  ps_only <- row_current_year[2:6]
  
  # Convert to numeric
  for (i in length(ps_only)){
    
    ps_only[i] <- convert_to_numeric(ps_only[i])
    
  }
  
  #Count how many positions have spending equal to or less than
  count <- sum(ps_only <= sbwin_pct)
  
  #Use boolean to check if spending is equal to or less than for at least 4/5 positions
  less_than_sbwin_pct <- (count >= 4)
  
  #Return boolean
  return(less_than_sbwin_pct)
  
}

#Use for loop to apply function to each team
for (i in 1:nrow(current_year_ps_pct)){
  
  current_year_ps_pct[i,8] <- compare_pct_4_of_5(current_year_ps_pct[i,])
  
}



#Analysis 3: How many teams are allocating the same percentage or less than to the RB 
#position and the same percentage or more to the other 4 premium positions? 

#Create a new column for boolean values to record results
current_year_ps_pct$RBunder_elseover <- NA

#Utilize a function to check for 2 conditions:
#1) If each NFL team's 2024 cap allocations to the RB position is equal to or 
#greater than the average allocation of the last 10 Super Bowl winners.
#2) If each NFL team's 2024 cap allocations to the other 4 premium positions is 
#equal to or greater than the average allocation of the last 10 Super Bowl winners.  
#Returns TRUE if the team is spending the same or less than for RB and the same or 
#greater than for all of the other 4 premium positions.
#Returns FALSE if the team is spending more than for RB or the same or less than
#for any of the other 4 premium positions.
compare_rbunder <- function(row_current_year){
  
  ps_only <- row_current_year[2:6]
  
  # Convert to numeric
  for (i in length(ps_only)){
    
    ps_only[i] <- convert_to_numeric(ps_only[i])
    
  }
  
  #Compare and store in boolean
  if(ps_only[1] >= sbwin_pct[1] &&
     ps_only[2] <= sbwin_pct[2] &&
     ps_only[3] >= sbwin_pct[3] &&
     ps_only[4] >= sbwin_pct[4] &&
     ps_only[5] >= sbwin_pct[5]) {
    
    less_than_sbwin_pct <- TRUE
    
  } else {
    
    less_than_sbwin_pct <- FALSE
    
  }
    
    #Return boolean
    return(less_than_sbwin_pct)
  
}

#Use for loop to apply function to each team
for (i in 1:nrow(current_year_ps_pct)){
  
  current_year_ps_pct[i,9] <- compare_rbunder(current_year_ps_pct[i,])
  
}

#Export to Excel positional spending data for Super Bowl teams from the last 10 years
write_xlsx(final_sb_sc_info, "Super_Bowl_Teams_Spending_2014_to_2023.xlsx")

#Export to Excel positional spending data for every NFL team this year
write_xlsx(current_year_ps_pct, "Positional_Spending_2024.xlsx")

#Export to Excel average positional spending data Super Bowl winning teams the last 10 years
write_xlsx(avg_row, "Average_Positional_Spending_by_SBWinners_2014_to_2023.xlsx")
