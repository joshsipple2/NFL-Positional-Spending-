Background: In the NFL, the presence of a hard salary cap (a fixed and inflexible limit on how much a given NFL team can spend in a given season on its roster) strongly impacts a franchise's ability to compete for championships. Specifically, it makes it difficult for franchises to keep their best players at the most impactful positions, as these players become highly sought after and command ever larger salaries. Paying more for one player directly lessens that team's ability to bid on the services of a different player. Given this, I decided to compare historical positional spending to current positional spending. I used R to scrape selected sports websites for the desired data. I then cleaned and reorganized said data and exported the results to Excel. I then used Tableau Public to create a visualization of the results. I have included the R Script as a zip file and a link to the dashboard on Tableau Public. 

Title: Analysis of Positional Spending by Super Bowl Teams 2014-2023
Author: Joshua Sipple
Date: 23 MAY 2024

Description: I have gathered data from multiple websites (see below) on the strategic use of cap space by teams in the NFL that made it to  the Super Bowl from 2014 to 2023. I focused my analysis on the amount of cap allocated to the Quarterback (QB), Wide Receiver (WR), Offensive Line (OLINE), and Defensive End/Edge Pass Rusher (EDGE) positions, as these are widely considered to be premium positions. I also included the Running Back (RB) position, as the salaries of this position have been the source of much controversy over the last several years. Additionally, I have averaged the amounts of cap allocated to each position over the last 10 years, so any interested party can easily check to see if their team has followed a similar strategy in 2024.

Data: The data is sourced from reputable sports websites (see below). In this script, I eliminate unnecessary data before cleaning and formatting the 
remaining data to be used in my analyses.

Websites Used:
 https://www.topendsports.com/events/super-bowl/winners-list.htm
 https://overthecap.com/positional-spending
 https://www.spotrac.com/nfl/cba
