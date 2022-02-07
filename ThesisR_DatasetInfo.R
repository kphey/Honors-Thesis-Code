

#The original dataset is denoted as "RawData" to make descriptions and
#the later creation of the new dataset less bulky.
RawData <- COVID.19_Case_Surveillance_Public_Use_Data_with_Geography

####Initial description####
dim(RawData)
#[1] 10291750       19

####Creating the new dataset####
#Only the necessary variables are included:
Data1 <- data.frame(RawData[, 1], RawData[, 6], RawData[, 7],
                   RawData[, 8], RawData[, 16])
colnames(Data1) <- c("case_month", "age_group", "sex", "race", "hosp_yn")
head(Data1)
#   case_month      age_group    sex    race hosp_yn
# 1    2021-08 18 to 49 years   <NA>    <NA> Missing
# 2    2021-04   0 - 17 years Female    <NA>      No
# 3    2021-09   0 - 17 years Female Missing Missing
# 4    2021-04   0 - 17 years Female    <NA> Missing
# 5    2021-08 18 to 49 years Female Unknown Missing
# 6    2021-08 18 to 49 years Female    <NA>      No

#remove all records with missing values in the predictors for imputation:
Data <- subset(Data1, age_group!= "Missing" & !is.na(age_group) & 
                    sex!= "Missing" & sex!= "Unknown" & !is.na(sex) & 
                    race!= "Missing" & race!= "Unknown" & !is.na(race))
dim(Data)
#[1] 6770358       5
#The new dataset contains 6,770,358 records and 5 variables.

#code the missing data in hosp_yn as <NA> for imputation:
Data[Data$hosp_yn == "Missing"|Data$hosp_yn == "Unknown", 5] <- NA

####Unique values in dataset####
unique(Data$case_month)
#[1] "2021-04" "2021-09" "2021-05" "2021-08" "2021-07" "2021-06"
unique(Data$age_group)
#[1] "18 to 49 years" "0 - 17 years"   "50 to 64 years" "65+ years" 
unique(Data$sex)
#[1] "Female" "Male"
unique(Data$race)
# [1] "Black"                                  "Multiple/Other"                        
# [3] "White"                                  "American Indian/Alaska Native"         
# [5] "Asian"                                  "Native Hawaiian/Other Pacific Islander"
unique(Data$hosp_yn)
#[1] NA    "No"  "Yes"

####Redefining variables as factors####
str(Data)
# 'data.frame':	6770358 obs. of  5 variables:
# $ case_month: chr  "2021-04" "2021-09" "2021-05" "2021-04" ...
# $ age_group : chr  "18 to 49 years" "18 to 49 years" "18 to 49 years" "18 to 49 years" ...
# $ sex       : chr  "Female" "Female" "Male" "Female" ...
# $ race      : chr  "Black" "Black" "Multiple/Other" "White" ...
# $ hosp_yn   : chr  NA NA "No" "No" ...
Data$case_month <- as.factor(Data$case_month)
Data$age_group <- as.factor(Data$age_group)
Data$sex <- as.factor(Data$sex)
Data$race <- as.factor(Data$race)
Data$hosp_yn <- as.factor(Data$hosp_yn)
str(Data)
# 'data.frame':	6770358 obs. of  5 variables:
# $ case_month: Factor w/ 6 levels "2021-04","2021-05",..: 1 6 2 1 2 1 5 6 1 5 ...
# $ age_group : Factor w/ 4 levels "0 - 17 years",..: 2 2 2 2 2 2 1 2 2 2 ...
# $ sex       : Factor w/ 2 levels "Female","Male": 1 1 2 1 1 1 1 1 1 2 ...
# $ race      : Factor w/ 6 levels "American Indian/Alaska Native",..: 3 3 4 6 4 4 6 3 6 1 ...
# $ hosp_yn   : Factor w/ 2 levels "No","Yes": NA NA 1 1 NA NA NA 1 1 NA ...
