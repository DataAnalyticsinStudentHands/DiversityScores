#Install and set up the "dplyr" package
install.packages("dplyr")
library(dplyr)

#Load data from sample_sam for convenience

sample_sam <- readRDS("C:/Users/adity/OneDrive/Desktop/Data Science/Summer Project/The Diversity Project/sample_sam.RDS")

#Types of Education Attainments
table(sample_sam$education.attainment)

#Calculate a "Diversity Score" for Educational Diversity in Harris County
#Iceland's Formula used 
Educational_Attainment_For_Harris_County <- sample_sam %>%
  group_by(sample_sam$tract) %>%
  summarise(Total_Population_Harris_County = (sum(!is.na(ACCOUNT))), Diversity_Score_9to12 = (((sum(education.attainment == "9th to 12th grade, no diploma"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "9th to 12th grade, no diploma"))), Diversity_Score_Associate = (((sum(education.attainment == "Associate's degree"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "Associate's degree"))), Diversity_Score_Bachelor = (((sum(education.attainment == "Bachelor's Degree"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "Bachelor's Degree"))), Diversity_Score_Graduate = (((sum(education.attainment == "Graduate or Professional Degree"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "Graduate or Professional Degree"))), Diversity_Score_HighSchoolGrad = (((sum(education.attainment == "High School Graduate"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "High School Graduate"))), Diversity_Score_LessThan9 = (((sum(education.attainment == "Less than 9th grade"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "Less than 9th grade"))), Diversity_Score_SomeCollegeNoDegree = (((sum(education.attainment == "Some College, no degree"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "Some College, no degree"))))

#Make all nan values equal to zero
Educational_Attainment_For_Harris_County[is.na(Educational_Attainment_For_Harris_County)] <- 0

#Calculate the summation of the Diversity Scores
Educational_Attainment_For_Harris_County$Summation_Of_Diversity_Scores <- Educational_Attainment_For_Harris_County$Diversity_Score_9to12 + Educational_Attainment_For_Harris_County$Diversity_Score_Associate + Educational_Attainment_For_Harris_County$Diversity_Score_Bachelor + Educational_Attainment_For_Harris_County$Diversity_Score_Graduate + Educational_Attainment_For_Harris_County$Diversity_Score_HighSchoolGrad +
  Educational_Attainment_For_Harris_County$Diversity_Score_SomeCollegeNoDegree +
  Educational_Attainment_For_Harris_County$Diversity_Score_LessThan9

#View the Racial Diversity Scores for Harris County
View(Educational_Attainment_For_Harris_County)

###
#Diversity Scores for Race According to Census Tracts

#Calculate a "Diversity Score" for Educational Diversity in Harris County according to Census Tracts
#Iceland's Formula used
Educational_Attainment_According_To_Census_Tracts <- sampleEducational_Attainment_According_To_Census_Tracts <- sample_sam %>%
  group_by(Census_Tract = sample_sam$tract) %>%
  summarise(Total_Population_In_Census_Tract = (sum(!is.na(ACCOUNT))), Educational_Attainment_According_To_Census_Tracts_9to12 = (((sum(education.attainment == "9th to 12th grade, no diploma"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "9th to 12th grade, no diploma"))), Educational_Attainment_According_To_Census_Tracts_Associate = (((sum(education.attainment == "Associate's degree"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "Associate's degree"))), Educational_Attainment_According_To_Census_Tracts_Bachelor = (((sum(education.attainment == "Bachelor's Degree"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "Bachelor's Degree"))), Educational_Attainment_According_To_Census_Tracts_Graduate = (((sum(education.attainment == "Graduate or Professional Degree"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "Graduate or Professional Degree"))), Educational_Attainment_According_To_Census_Tracts_HighSchoolGrad = (((sum(education.attainment == "High School Graduate"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "High School Graduate"))), Educational_Attainment_According_To_Census_Tracts_LessThan9 = (((sum(education.attainment == "Less than 9th grade"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "Less than 9th grade"))), Educational_Attainment_According_To_Census_Tracts_SomeCollegeNoDegree = (((sum(education.attainment == "Some College, no degree"))/((sum(!is.na(ACCOUNT))))))*log(((sum(!is.na(ACCOUNT))))/(sum(education.attainment == "Some College, no degree"))))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
Educational_Attainment_According_To_Census_Tracts[is.na(Educational_Attainment_According_To_Census_Tracts)] <- 0

#Make the Nan values zero
Educational_Attainment_According_To_Census_Tracts[is.nan(Educational_Attainment_According_To_Census_Tracts)] <- 0

#Calculate summation of Diversity Scores of all types of education

Educational_Attainment_According_To_Census_Tracts$Summation_Of_Diversity_Scores_According_To_Census_Tracts <- Educational_Attainment_According_To_Census_Tracts$Educational_Attainment_According_To_Census_Tracts_9to12 + Educational_Attainment_According_To_Census_Tracts$Educational_Attainment_According_To_Census_Tracts_Associate + Educational_Attainment_According_To_Census_Tracts$Educational_Attainment_According_To_Census_Tracts_Bachelor + Educational_Attainment_According_To_Census_Tracts$Educational_Attainment_According_To_Census_Tracts_Graduate + Educational_Attainment_According_To_Census_Tracts$Educational_Attainment_According_To_Census_Tracts_HighSchoolGrad + Educational_Attainment_According_To_Census_Tracts$Educational_Attainment_According_To_Census_Tracts_LessThan9 +
  Educational_Attainment_According_To_Census_Tracts$Educational_Attainment_According_To_Census_Tracts_SomeCollegeNoDegree

# View Racial Diversity according to Census Tracts
View(Educational_Attainment_According_To_Census_Tracts)



