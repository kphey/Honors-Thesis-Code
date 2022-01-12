
####IMPUTATION####
#This program assumes that the "Data" data frame developed in "ThesisR_DatasetInfo.R"
#is saved in the global environment.
library(mice)
(LRimp1_hosp <- mice(Data, method = "logreg", m = 5))
# iter imp variable
# 1   1  hosp_yn
# 1   2  hosp_yn
# 1   3  hosp_yn
# 1   4  hosp_yn
# 1   5  hosp_yn
# 2   1  hosp_yn
# 2   2  hosp_yn
# 2   3  hosp_yn
# 2   4  hosp_yn
# 2   5  hosp_yn
# 3   1  hosp_yn
# 3   2  hosp_yn
# 3   3  hosp_yn
# 3   4  hosp_yn
# 3   5  hosp_yn
# 4   1  hosp_yn
# 4   2  hosp_yn
# 4   3  hosp_yn
# 4   4  hosp_yn
# 4   5  hosp_yn
# 5   1  hosp_yn
# 5   2  hosp_yn
# 5   3  hosp_yn
# 5   4  hosp_yn
# 5   5  hosp_yn
# Class: mids
# Number of multiple imputations:  5 
# Imputation methods:
#   case_month  age_group        sex       race    hosp_yn   death_yn 
#           ""         ""         ""         ""   "logreg"         "" 
# PredictorMatrix:
#   case_month age_group sex race hosp_yn death_yn
# case_month          0         1   1    1       1        1
# age_group           1         0   1    1       1        1
# sex                 1         1   0    1       1        1
# race                1         1   1    0       1        1
# hosp_yn             1         1   1    1       0        1
# death_yn            1         1   1    1       1        0

#Accessing the imputed datasets:
DataLRComplete <- complete(LRimp1_hosp, "all")
head(DataLRComplete)
#Here are the first 5 rows of the first imputed dataset:
# $`1`
# case_month      age_group    sex                          race hosp_yn death_yn
# 15      2021-04 18 to 49 years Female                         White      No       No
# 33      2021-09 18 to 49 years Female                         Black      No       No
# 34      2021-04 18 to 49 years Female                         White      No       No
# 45      2021-07 18 to 49 years Female                Multiple/Other     Yes       No
# 53      2021-08 18 to 49 years Female                         Black      No       No
#Here are the first 5 rows of the fifth imputed dataset:
# $`5`
# case_month      age_group    sex                          race hosp_yn death_yn
# 15      2021-04 18 to 49 years Female                         White      No       No
# 33      2021-09 18 to 49 years Female                         Black      No       No
# 34      2021-04 18 to 49 years Female                         White      No       No
# 45      2021-07 18 to 49 years Female                Multiple/Other     Yes       No
# 53      2021-08 18 to 49 years Female                         Black      No       No

#Checking the values of hosp_yn in all datasets with
#unique(DataLRComplete$`number here`$hosp_yn)
#shows that the missing values in hosp_yn in all datasets have been imputed.
#For example:
unique(DataLRComplete$`3`$hosp_yn) 
# [1] No  Yes
# Levels: No Yes

#####IMPUTATION MODEL COEFFICIENTS####
#A regular LR model, with the observed hosp_yn data:
DataLR_RegObs <- subset(Data, age_group!= "Missing" & !is.na(age_group) & 
                    sex!= "Missing" & sex!= "Unknown" & !is.na(sex) & 
                    race!= "Missing" & race!= "Unknown" & !is.na(race) & 
                    death_yn!= "Missing" & death_yn!= "Unknown" & !is.na(death_yn) &
                      hosp_yn!= "Missing" & hosp_yn!= "Unknown")
dim(DataLR_RegObs) #1,886,066 records

#Set all variables to factors:
Obscase_month <- as.factor(DataLR_RegObs$case_month)
Obsage_group <- as.factor(DataLR_RegObs$age_group)
Obssex <- as.factor(DataLR_RegObs$sex)
Obsrace <- as.factor(DataLR_RegObs$race)
Obshosp_yn <- as.factor(DataLR_RegObs$hosp_yn)
Obsdeath_yn <- as.factor(DataLR_RegObs$death_yn)
#Check:
str(DataLR_RegObs)
#Model:
LRObsFit <- glm(Obshosp_yn ~ Obscase_month + Obsage_group + Obssex + Obsrace + Obsdeath_yn, family = binomial)
summary(LRObsFit)
# Call:
#   glm(formula = Obshosp_yn ~ Obscase_month + Obsage_group + Obssex + 
#         Obsrace + Obsdeath_yn, family = binomial)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.4283  -0.3578  -0.2566  -0.1704   3.0879  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                   -4.105173   0.043207 -95.011  < 2e-16 ***
#   Obscase_month2021-05                           0.185800   0.013091  14.193  < 2e-16 ***
#   Obscase_month2021-06                           0.252162   0.017571  14.351  < 2e-16 ***
#   Obscase_month2021-07                          -0.103953   0.011851  -8.771  < 2e-16 ***
#   Obscase_month2021-08                          -0.130406   0.009587 -13.602  < 2e-16 ***
#   Obscase_month2021-09                          -0.184292   0.009821 -18.766  < 2e-16 ***
#   Obsage_group18 to 49 years                     1.177355   0.014623  80.517  < 2e-16 ***
#   Obsage_group50 to 64 years                     2.284791   0.014994 152.381  < 2e-16 ***
#   Obsage_group65+ years                          3.100679   0.015073 205.708  < 2e-16 ***
#   ObssexMale                                     0.159435   0.006484  24.589  < 2e-16 ***
#   ObsraceAsian                                  -0.273359   0.052306  -5.226 1.73e-07 ***
#   ObsraceBlack                                   0.211414   0.041067   5.148 2.63e-07 ***
#   ObsraceMultiple/Other                          0.064791   0.045117   1.436    0.151    
# ObsraceNative Hawaiian/Other Pacific Islander  0.480386   0.107413   4.472 7.74e-06 ***
#   ObsraceWhite                                  -0.469421   0.040585 -11.566  < 2e-16 ***
#   Obsdeath_ynYes                                 3.342243   0.017483 191.171  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 925398  on 1886065  degrees of freedom
# Residual deviance: 738603  on 1886050  degrees of freedom
# AIC: 738635
# 
# Number of Fisher Scoring iterations: 7

####PREDICTOR SELECTION BY USEFULNESS IN IMPUTATION####

quickpred(DataLR1.1, mincor = 0.1, minpuc = 1)
#minpuc doesn't matter here, since all predictors have only obs values.
#The highest correlation with at least 3 predictors is r = 0.1. No predictors past r = 0.25 can be used.
#             case_month age_group sex race hosp_yn death_yn
# case_month          0         0   0    0       0        0
# age_group           0         0   0    0       0        0
# sex                 0         0   0    0       0        0
# race                0         0   0    0       0        0
# hosp_yn             1         1   0    0       0        1
# death_yn            0         0   0    0       0        0
#With these criteria, case_month, age_group, and death_yn are useful predictors.
#Further analysis with mincor > .1 show that age_group and death_yn
#have the stronger correlations, with age_group being the strongest (at mincor = 0.25).

#Below are the same procedures, but including unobserved values in the predictors
#(i.e. missing and unknown values in age_group, sex, race, and death_yn).
#Remove the NMAR values:
DataLR_MisUnkPred <- subset(Data1, !is.na(age_group) & !is.na(sex) & !is.na(race) & !is.na(death_yn))
DataLR_MisUnkPred[DataLR_MisUnkPred$case_month == "Missing"|DataLR_MisUnkPred$case_month == "Unknown", 1] <- NA
DataLR_MisUnkPred[DataLR_MisUnkPred$age_group == "Missing"|DataLR_MisUnkPred$age_group == "Unknown", 2] <- NA
DataLR_MisUnkPred[DataLR_MisUnkPred$sex == "Missing"|DataLR_MisUnkPred$sex == "Unknown", 3] <- NA
DataLR_MisUnkPred[DataLR_MisUnkPred$race == "Missing"|DataLR_MisUnkPred$race == "Unknown", 4] <- NA
DataLR_MisUnkPred[DataLR_MisUnkPred$hosp_yn == "Missing"|DataLR_MisUnkPred$hosp_yn == "Unknown", 5] <- NA
DataLR_MisUnkPred[DataLR_MisUnkPred$death_yn == "Missing"|DataLR_MisUnkPred$death_yn == "Unknown", 6] <- NA

#Run quickpred() again:
quickpred(DataLR_MisUnkPred, mincor = 0.27, minpuc = 0.19)
#these were the highest values in both arguments that still allowed more than one predictor.
#            case_month age_group sex race hosp_yn death_yn
# case_month          0         0   0    0       0        0
# age_group           0         0   0    0       0        0
# sex                 0         0   0    0       0        0
# race                0         0   0    0       0        0
# hosp_yn             0         1   0    0       0        1
# death_yn            0         0   0    0       0        0

#age_group and death_yn could be useful predictors.

####REDUCED MODEL####
#this model also uses only observed values for all variables, like
#the full model.
LRObsRedFit <- glm(Obshosp_yn ~ Obscase_month + Obsage_group + Obsdeath_yn, family = binomial)
summary(LRObsRedFit)
# Call:
#   glm(formula = Obshosp_yn ~ Obscase_month + Obsage_group + Obsdeath_yn, 
#       family = binomial)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1871  -0.3209  -0.2707  -0.1660   3.0068  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                -4.278294   0.015154 -282.32   <2e-16 ***
#   Obscase_month2021-05        0.185111   0.013036   14.20   <2e-16 ***
#   Obscase_month2021-06        0.236647   0.017510   13.52   <2e-16 ***
#   Obscase_month2021-07       -0.125792   0.011801  -10.66   <2e-16 ***
#   Obscase_month2021-08       -0.162752   0.009527  -17.08   <2e-16 ***
#   Obscase_month2021-09       -0.231123   0.009750  -23.70   <2e-16 ***
#   Obsage_group18 to 49 years  1.152412   0.014593   78.97   <2e-16 ***
#   Obsage_group50 to 64 years  2.191781   0.014899  147.11   <2e-16 ***
#   Obsage_group65+ years       2.956876   0.014891  198.56   <2e-16 ***
#   Obsdeath_ynYes              3.380615   0.017425  194.01   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 925398  on 1886065  degrees of freedom
# Residual deviance: 745173  on 1886056  degrees of freedom
# AIC: 745193
# 
# Number of Fisher Scoring iterations: 7

####PARTIAL F-TEST####
anova(LRObsRedFit, LRObsFit, test = "Chisq")
# Analysis of Deviance Table
# 
# Model 1: Obshosp_yn ~ Obscase_month + Obsage_group + Obsdeath_yn
# Model 2: Obshosp_yn ~ Obscase_month + Obsage_group + Obssex + Obsrace + 
#   Obsdeath_yn
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1   1886056     745173                          
# 2   1886050     738603  6   6570.3 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#The reduced model does not seem to fit better than the full model.
#The variables sex and race do contribute some utility to the model
#given case_month, age_group, and death_yn.

####Unedited as of 1/11/22####

####LOG-LINEAR MODEL FITTING####
#First, we need to make a contingency table of the imputed data.
#See main doc for an explanation as to why a table of DataLRComplete is not feasible,
#and why the best-fit model is determined this way.

(DataLRComplete_1_table <-
   ftable(hosp_yn ~ case_month + age_group + sex + race + death_yn, data = DataLRComplete$`1`))

#table not displayed here for brevity. Note that there are many zeros in the table.

for(i in 1:dim(DataLRComplete_1_table)[1]) {
  for(j in 1:dim(DataLRComplete_1_table)[2]) {
    DataLRComplete_1_table[i, j] <- (DataLRComplete_1_table[i, j] + .5)
  }
}
DataLRComplete_1_table #all cells (including those with nonzero values) have a
#value of at least .5 to prevent potential errors due to structural zeros

#I created this function to return the analog R^2 values
#to test the L^2 values of two log-linear models (based on Knoke and Burk, p. 41)
analogR <- function(Lbase, Lalt) {
  anlogRsq <- (Lbase - Lalt) / (Lbase)
  return(anlogRsq)
}
#Model selection

#[CASRDH]
(LR_CASRDH <- loglin(as.table(DataLRComplete_1_table),
       margin = list(c("case_month", "age_group", "sex", "race", "death_yn", "hosp_yn"))))
# $lrt
# [1] 0 - obviously; saturated model
# 
# $pearson
# [1] 0
# 
# $df
# [1] 0
#[CASRD][H]
(LR_CASRD_H <- loglin(as.table(DataLRComplete_1_table),
       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("hosp_yn"))))
# $lrt
# [1] 170175.5
# 
# $pearson
# [1] 252049.8
# 
# $df
# [1] 575

(LR_CASRD_CH <- loglin(as.table(DataLRComplete_1_table),
                  margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"))))
(LR_CASRD_AH <- loglin(as.table(DataLRComplete_1_table),
                   margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"))))
(LR_CASRD_SH <- loglin(as.table(DataLRComplete_1_table),
                   margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("sex","hosp_yn"))))
(LR_CASRD_RH <- loglin(as.table(DataLRComplete_1_table),
                   margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("race","hosp_yn"))))
(LR_CASRD_DH <- loglin(as.table(DataLRComplete_1_table),
                   margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("death_yn","hosp_yn"))))
analogR(LR_CASRD_H$lrt, LR_CASRD_CH$lrt) #0.00436275
analogR(LR_CASRD_H$lrt, LR_CASRD_AH$lrt) #0.8099917
analogR(LR_CASRD_H$lrt, LR_CASRD_SH$lrt) #0.004984067
analogR(LR_CASRD_H$lrt, LR_CASRD_RH$lrt) #0.01253915
analogR(LR_CASRD_H$lrt, LR_CASRD_DH$lrt) #0.2454301
#Conclusion: only age_group has a significant relationship with hosp_yn. If we
#really didn't care about making a Type I error, we could also add race and death_yn.

#From here, we should expect C, S, and (probably) R (and maybe D) to not add anything
#significant to the fit of any models.
(LR_CASRD_CH_AH <- loglin(as.table(DataLRComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("age_group","hosp_yn"))))
(LR_CASRD_CH_SH <- loglin(as.table(DataLRComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("sex","hosp_yn"))))
(LR_CASRD_CH_RH <- loglin(as.table(DataLRComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("race","hosp_yn"))))
(LR_CASRD_CH_DH <- loglin(as.table(DataLRComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("death_yn","hosp_yn"))))
(LR_CASRD_AH_SH <- loglin(as.table(DataLRComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"), c("sex","hosp_yn"))))
(LR_CASRD_AH_RH <- loglin(as.table(DataLRComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"), c("race","hosp_yn"))))

(LR_CASRD_AH_DH <- loglin(as.table(DataLRComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"), c("death_yn","hosp_yn"))))


(LR_CASRD_SH_RH <- loglin(as.table(DataLRComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("sex","hosp_yn"), c("race","hosp_yn"))))
(LR_CASRD_SH_DH <- loglin(as.table(DataLRComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("sex","hosp_yn"), c("death_yn","hosp_yn"))))
(LR_CASRD_RH_DH <- loglin(as.table(DataLRComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("race","hosp_yn"), c("death_yn","hosp_yn"))))
analogR(LR_CASRD_CH$lrt, LR_CASRD_CH_AH$lrt) #0.8136458
#Note: although LR_CASRD_CH_AH is a vast improvement from LR_CASRD_CH, the latter model was
#a poor fit in the first place, and we know that age_group is related to hosp_yn, so naturally
#the model is improved with the inclusion of AH. We still reject this model, as CH does
#not significantly improve the model.
analogR(LR_CASRD_CH$lrt, LR_CASRD_CH_SH$lrt) #0.005054556
analogR(LR_CASRD_CH$lrt, LR_CASRD_CH_RH$lrt) #0.01241717
analogR(LR_CASRD_CH$lrt, LR_CASRD_CH_DH$lrt) #0.2468067

analogR(LR_CASRD_AH$lrt, LR_CASRD_AH_SH$lrt) #0.03511717
analogR(LR_CASRD_AH$lrt, LR_CASRD_AH_RH$lrt) #0.2566237
analogR(LR_CASRD_AH$lrt, LR_CASRD_AH_DH$lrt) #0.6435683
#LR_CASRD_AH_DH is the best model out of this group, as the fit of LR_CASRD_AH was already
#pretty good, and the inclusion of DH improves the fit to a good extent compared
#to the other models in this group.

analogR(LR_CASRD_SH$lrt, LR_CASRD_SH_RH$lrt) #0.01310339
analogR(LR_CASRD_SH$lrt, LR_CASRD_SH_DH$lrt) #0.2450497
analogR(LR_CASRD_RH$lrt, LR_CASRD_RH_DH$lrt) #0.2483183
#Conclusion: Both age_group and death_yn affect hosp_yn (though we can guess
#that age_group has the stronger effect). The model {LR_CASRD}{AH}{DH} is the best
#out of this group.

#for comparison:
(LR_CASRD_CH_AH_SH_RH_DH <- loglin(as.table(DataLRComplete_1_table),
margin = list(c("case_month","age_group","sex","race","death_yn"),
c("case_month","hosp_yn"), c("age_group","hosp_yn"), c("sex","hosp_yn"), c("race","hosp_yn"), c("death_yn","hosp_yn"))))

(LR_CASRD_ADH_CSRH <- loglin(as.table(DataLRComplete_1_table),
margin = list(c("case_month","age_group","sex","race","death_yn"), c("age_group","death_yn","hosp_yn"), c("case_month","sex","race","hosp_yn"))))
(LR_CASRD_RDH_CSAH <- loglin(as.table(DataLRComplete_1_table),
margin = list(c("case_month","age_group","sex","race","death_yn"), c("race","death_yn","hosp_yn"), c("case_month","sex","age_group","hosp_yn"))))
(LR_CASRD_ARH_CSDH <- loglin(as.table(DataLRComplete_1_table),
margin = list(c("case_month","age_group","sex","race","death_yn"), c("age_group","race","hosp_yn"), c("case_month","sex","death_yn","hosp_yn"))))
(LR_CASRD_CSH_ARDH <- loglin(as.table(DataLRComplete_1_table),
margin = list(c("case_month","age_group","sex","race","death_yn"), c("case_month","sex","hosp_yn"), c("age_group","race","death_yn","hosp_yn"))))

analogR(LR_CASRD_CH_AH_SH_RH_DH$lrt, LR_CASRD_ADH_CSRH$lrt) #0.2929918
analogR(LR_CASRD_CH_AH_SH_RH_DH$lrt, LR_CASRD_RDH_CSAH$lrt) #0.3163102
analogR(LR_CASRD_CH_AH_SH_RH_DH$lrt, LR_CASRD_ARH_CSDH$lrt) #0.2080924

analogR(LR_CASRD_CH_AH_SH_RH_DH$lrt, LR_CASRD_CSH_ARDH$lrt) #0.4098991
#The model {LR_CASRD}{CSH}{ARDH} has an lrt value of 925.9724 with 517 df, making it
#arguably the best-fitting model out of all those tested.

#OVerall conclusion: Between {LR_CASRD}{CSH}{ARDH} and {LR_CASRD}{AH}{DH}, the former
#has a much lower lrt value for fewer df's, so {LR_CASRD}{CSH}{ARDH} will be used
#in the with() function (i.e. for the other imputed databases).

analogR(LR_CASRD_H$lrt, LR_CASRD_CSH_ARDH$lrt) #0.9945587

#just to make sure:
library(MASS)

DataLR_loglm <- loglm(hosp_yn ~
  case_month:age_group:sex:race:death_yn
  + case_month:age_group:sex + case_month:age_group:race + case_month:age_group:death_yn + case_month:sex:race + case_month:sex:death_yn + case_month:race:death_yn
  + age_group:sex:race + age_group:sex:death_yn + age_group:race:death_yn + sex:race:death_yn + case_month:age_group + case_month:sex + case_month:race
  + case_month:death_yn + age_group:sex + age_group:race + age_group:death_yn + sex:race + sex:death_yn + race:death_yn
  + case_month + age_group + sex + race + death_yn
  + case_month:sex:hosp_yn
  + case_month:hosp_yn + hosp_yn
  + age_group:race:death_yn:hosp_yn
  + age_group:race:hosp_yn + age_group:death_yn:hosp_yn + race:death_yn:hosp_yn + age_group:hosp_yn + race:hosp_yn + death_yn:hosp_yn, 
  data = DataLRComplete$`1`)
# Statistics:
#                       X^2      df P(> X^2)
# Likelihood Ratio 105771.5 2281052        1
# Pearson          126410.3 2281052        1
#I don't know how to get the loglin() results from the loglm() function,
#and I can't use loglin() in with().







