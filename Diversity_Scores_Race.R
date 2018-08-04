#Install and set up the "dplyr" package
install.packages("dplyr")
library(dplyr)

#Load data from sample_sam for convenience
sample_sam <- readRDS("C:/Users/adity/OneDrive/Desktop/Data Science/Summer Project/The Diversity Project/sample_sam.RDS")


#Calculate a "Diversity Score" for Racial Diversity in Harris County
#Iceland's Formula used
Racial_Diversity_For_Harris_County <- sample_sam %>%
  group_by(sample_sam$tract) %>%
  summarise(Total_Population_Harris_County = (sum(!is.na(ACCOUNT))), Diversity_Score_Whites = (((sum(race == "White"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "White"))), Diversity_Score_Blacks = (((sum(race == "Black or African American"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "Black or African American"))), Diversity_Score_Asians = (((sum(race == "Asian"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "Asian"))), Diversity_Score_Hispanics = (((sum(race == "Hispanic or Latino"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "Hispanic or Latino"))), Diversity_Score_Two_Or_More_Races = (((sum(race == "Two or More Races"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "Two or More Races"))), Diversity_Score_Some_Other_Race = (((sum(race == "Some Other Race"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "Some Other Race"))))

#Make all nan values equal to zero
Racial_Diversity_For_Harris_County[is.nan(Racial_Diversity_For_Harris_County)] <- 0

#Calculate the summation of the Diversity Scores
Racial_Diversity_For_Harris_County$Summation_Of_Diversity_Scores <- Racial_Diversity_For_Harris_County$Diversity_Score_Whites + Racial_Diversity_For_Harris_County$Diversity_Score_Blacks + Racial_Diversity_For_Harris_County$Diversity_Score_Asians + Racial_Diversity_For_Harris_County$Diversity_Score_Hispanics + Racial_Diversity_For_Harris_County$Diversity_Score_Two_Or_More_Races + Racial_Diversity_For_Harris_County$Diversity_Score_Some_Other_Race

#View the Racial Diversity Scores for Harris County
View(Racial_Diversity_For_Harris_County)

###

#Diversity Scores for Race According to Census Tracts

#Calculate a "Diversity Score" for Racial Diversity in Harris County according to Census Tracts
#Iceland's Formula used
Racial_Diversity_According_To_Census_Tracts <- sampleRacial_Diversity_According_To_Census_Tracts <- sample_sam %>%
  group_by(Census_Tract = sample_sam$tract) %>%
  summarise(Total_Population_In_Census_Tract = (sum(!is.na(ACCOUNT))), Diversity_Score_According_To_Census_Tracts_Whites = (((sum(race == "White"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "White"))), Diversity_Score_According_To_Census_Tracts_Blacks = (((sum(race == "Black or African American"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "Black or African American"))), Diversity_Score_According_To_Census_Tracts_Asians = (((sum(race == "Asian"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "Asian"))), Diversity_Score_According_To_Census_Tracts_Hispanics = (((sum(race == "Hispanic or Latino"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "Hispanic or Latino"))), Diversity_Score_According_To_Census_Tracts_Two_Or_More_Races = (((sum(race == "Two or More Races"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "Two or More Races"))), Diversity_Score_According_To_Census_Tracts_Some_Other_Race = (((sum(race == "Some Other Race"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(race == "Some Other Race"))))

#Make the Nan values zero
Racial_Diversity_According_To_Census_Tracts[is.nan(Racial_Diversity_According_To_Census_Tracts)] <- 0

#Calculate summation of Diversity Scores of all Races
Racial_Diversity_According_To_Census_Tracts$Summation_Of_Diversity_Scores_According_To_Census_Tracts <- Racial_Diversity_According_To_Census_Tracts$Diversity_Score_According_To_Census_Tracts_Whites + Racial_Diversity_According_To_Census_Tracts$Diversity_Score_According_To_Census_Tracts_Blacks + Racial_Diversity_According_To_Census_Tracts$Diversity_Score_According_To_Census_Tracts_Asians + Racial_Diversity_According_To_Census_Tracts$Diversity_Score_According_To_Census_Tracts_Hispanics + Racial_Diversity_According_To_Census_Tracts$Diversity_Score_According_To_Census_Tracts_Two_Or_More_Races + Racial_Diversity_According_To_Census_Tracts$Diversity_Score_According_To_Census_Tracts_Some_Other_Race

# View Racial Diversity according to Census Tracts
View(Racial_Diversity_According_To_Census_Tracts)
