
#The original dataset is denoted as "RawData" to make descriptions and
#the later creation of the new dataset less bulky.
RawData <- COVID.19_Case_Surveillance_Public_Use_Data_with_Geography

####Initial description####
dim(RawData)
#[1] 10291750       19

####Creating the new dataset####
#Only the necessary variables are included:
Data1 <- data.frame(RawData[, 1], RawData[, 6], RawData[, 7],
                   RawData[, 8], RawData[, 16], RawData[, 18])
colnames(Data1) <- c("case_month", "age_group", "sex", "race", "hosp_yn", "death_yn")
head(Data1)
# case_month      age_group    sex    race hosp_yn death_yn
# 1    2021-08 18 to 49 years   <NA>    <NA> Missing       No
# 2    2021-04   0 - 17 years Female    <NA>      No       No
# 3    2021-09   0 - 17 years Female Missing Missing  Missing
# 4    2021-04   0 - 17 years Female    <NA> Missing  Unknown
# 5    2021-08 18 to 49 years Female Unknown Missing  Missing
# 6    2021-08 18 to 49 years Female    <NA>      No       No

#remove all records with missing values in the predictors for imputation:
Data <- subset(Data1, age_group!= "Missing" & !is.na(age_group) & 
                    sex!= "Missing" & sex!= "Unknown" & !is.na(sex) & 
                    race!= "Missing" & race!= "Unknown" & !is.na(race) & 
                    death_yn!= "Missing" & death_yn!= "Unknown" & !is.na(death_yn))
dim(Data)
#[1] 2659442       6
#The new dataset contains 2,659,442 records and 6 variables.

#code the missing data in hosp_yn as <NA> for imputation:
Data[Data$hosp_yn == "Missing"|Data$hosp_yn == "Unknown", 5] <- NA

####Unique values in dataset####
unique(Data$case_month)
#[1] "2021-04" "2021-09" "2021-07" "2021-08" "2021-06" "2021-05"
unique(Data$age_group)
#[1] "18 to 49 years" "0 - 17 years"   "50 to 64 years" "65+ years" 
unique(Data$sex)
#[1] "Female" "Male"
unique(Data$race)
# [1] "White"                                  "Black"                                 
# [3] "Multiple/Other"                         "Asian"                                 
# [5] "American Indian/Alaska Native"          "Native Hawaiian/Other Pacific Islander"
unique(Data$hosp_yn)
#[1] "No"  "Yes" NA
unique(Data$death_yn)
#[1] "No"  "Yes"

####Redefining variables as factors####
str(Data)
# 'data.frame':	2659442 obs. of  6 variables:
# $ case_month: chr  "2021-04" "2021-09" "2021-04" "2021-07" ...
# $ age_group : chr  "18 to 49 years" "18 to 49 years" "18 to 49 years" "18 to 49 years" ...
# $ sex       : chr  "Female" "Female" "Female" "Female" ...
# $ race      : chr  "White" "Black" "White" "Multiple/Other" ...
# $ hosp_yn   : chr  "No" "No" "No" "Yes" ...
# $ death_yn  : chr  "No" "No" "No" "No" ...
Data$case_month <- as.factor(Data$case_month)
Data$age_group <- as.factor(Data$age_group)
Data$sex <- as.factor(Data$sex)
Data$race <- as.factor(Data$race)
Data$hosp_yn <- as.factor(Data$hosp_yn)
Data$death_yn <- as.factor(Data$death_yn)
str(Data)
# 'data.frame':	2659442 obs. of  6 variables:
# $ case_month: Factor w/ 6 levels "2021-04","2021-05",..: 1 6 1 4 5 6 6 1 1 6 ...
# $ age_group : Factor w/ 4 levels "0 - 17 years",..: 2 2 2 2 2 2 2 2 1 2 ...
# $ sex       : Factor w/ 2 levels "Female","Male": 1 1 1 1 1 2 1 2 1 2 ...
# $ race      : Factor w/ 6 levels "American Indian/Alaska Native",..: 6 3 6 4 3 3 6 2 6 6 ...
# $ hosp_yn   : Factor w/ 2 levels "No","Yes": 1 1 1 2 1 NA 1 1 1 NA ...
# $ death_yn  : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
