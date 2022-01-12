
####IMPUTATION####
#This program assumes that the "Data" data frame developed in "ThesisR_DatasetInfo.R"
#is saved in the global environment.
library(mice)
(PMMimp_hosp <- mice(Data, method = "pmm", m = 5, donors = 10))
#d = 10 values per group for each missing value.

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
#           ""         ""         ""         ""      "pmm"         "" 
# PredictorMatrix:
#            case_month age_group sex race hosp_yn death_yn
# case_month          0         1   1    1       1        1
# age_group           1         0   1    1       1        1
# sex                 1         1   0    1       1        1
# race                1         1   1    0       1        1
# hosp_yn             1         1   1    1       0        1
# death_yn            1         1   1    1       1        0

DataPMMComplete <- complete(PMMimp_hosp, "all")
#For example:
unique(DataPMMComplete$`1`$hosp_yn)
# [1] No  Yes
# Levels: No Yes

#####Unedited as of 1/11/2022#####

####LOG-LINEAR MODEL FITTING####
#First, we need to make a contingency table of the imputed data.
(DataPMMComplete_1_table <-
   ftable(hosp_yn ~ case_month + age_group + sex + race + death_yn, data = DataPMMComplete$`1`))
#table not displayed here for brevity. Note that there are many zeros in the table.
for(i in 1:dim(DataPMMComplete_1_table)[1]) {
  for(j in 1:dim(DataPMMComplete_1_table)[2]) {
    DataPMMComplete_1_table[i, j] <- (DataPMMComplete_1_table[i, j] + .5)
  }
}
DataPMMComplete_1_table #all cells (including those with nonzero values) have a
#value of at least .5 to prevent potential errors due to structural zeros

#Just for reference, below is the analogR function. This does not need to
#be run if analogR is already in the global environment (under "Functions").
analogR <- function(Lbase, Lalt) {
  anlogRsq <- (Lbase - Lalt) / (Lbase)
  return(anlogRsq)
}

#Model selection
#[CASRDH]
(PMM_CASRDH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month", "age_group", "sex", "race", "death_yn", "hosp_yn"))))
# $lrt
# [1] 0
# 
# $pearson
# [1] 0
# 
# $df
# [1] 0

(PMM_CASRD_H <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("hosp_yn"))))
# $lrt
# [1] 208972.5
# 
# $pearson
# [1] 293567.6
# 
# $df
# [1] 575

(PMM_CASRD_CH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"))))
(PMM_CASRD_AH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"))))
(PMM_CASRD_SH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("sex","hosp_yn"))))
(PMM_CASRD_RH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("race","hosp_yn"))))
(PMM_CASRD_DH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("death_yn","hosp_yn"))))
analogR(PMM_CASRD_H$lrt, PMM_CASRD_CH$lrt) #0.01701395
analogR(PMM_CASRD_H$lrt, PMM_CASRD_AH$lrt) #0.6347092
analogR(PMM_CASRD_H$lrt, PMM_CASRD_SH$lrt) #0.006177317
analogR(PMM_CASRD_H$lrt, PMM_CASRD_RH$lrt) #0.002789206
analogR(PMM_CASRD_H$lrt, PMM_CASRD_DH$lrt) #0.1916345
#Conclusion: Only age_group has a somewhat significant relationship with hosp_yn. If we
#really didn't care about making a Type I error, we could also add death_yn.

#From here, we expect C, S, and R (and maybe D) to not add anything
#significant to the fit of any models.
(PMM_CASRD_CH_AH <- loglin(as.table(DataPMMComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("age_group","hosp_yn"))))
(PMM_CASRD_CH_SH <- loglin(as.table(DataPMMComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("sex","hosp_yn"))))
(PMM_CASRD_CH_RH <- loglin(as.table(DataPMMComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("race","hosp_yn"))))
(PMM_CASRD_CH_DH <- loglin(as.table(DataPMMComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("death_yn","hosp_yn"))))

(PMM_CASRD_AH_SH <- loglin(as.table(DataPMMComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"), c("sex","hosp_yn"))))
(PMM_CASRD_AH_RH <- loglin(as.table(DataPMMComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"), c("race","hosp_yn"))))
(PMM_CASRD_AH_DH <- loglin(as.table(DataPMMComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"), c("death_yn","hosp_yn"))))

(PMM_CASRD_SH_RH <- loglin(as.table(DataPMMComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("sex","hosp_yn"), c("race","hosp_yn"))))
(PMM_CASRD_SH_DH <- loglin(as.table(DataPMMComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("sex","hosp_yn"), c("death_yn","hosp_yn"))))
(PMM_CASRD_RH_DH <- loglin(as.table(DataPMMComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("race","hosp_yn"), c("death_yn","hosp_yn"))))

analogR(PMM_CASRD_CH$lrt, PMM_CASRD_CH_AH$lrt) #0.6490324
#As in the log-linear model analysis in the LRI, this seems favorable,
#but is likely only due to the inclusion of AH and does not necessarily
#mean that the model itself is the best one. We still reject this model,
#as CH adds very little to the model fit.
analogR(PMM_CASRD_CH$lrt, PMM_CASRD_CH_SH$lrt) #0.006204727
analogR(PMM_CASRD_CH$lrt, PMM_CASRD_CH_RH$lrt) #0.002720474
analogR(PMM_CASRD_CH$lrt, PMM_CASRD_CH_DH$lrt) #0.1921255

analogR(PMM_CASRD_AH$lrt, PMM_CASRD_AH_SH$lrt) #0.01942812
analogR(PMM_CASRD_AH$lrt, PMM_CASRD_AH_RH$lrt) #0.06666903
analogR(PMM_CASRD_AH$lrt, PMM_CASRD_AH_DH$lrt) #0.2562949
#DH only adds a little to the model, which is already a good one
#due to AH. Compared to the previous models, however,
#this may be the best model.

analogR(PMM_CASRD_SH$lrt, PMM_CASRD_SH_RH$lrt) #0.002865211
analogR(PMM_CASRD_SH$lrt, PMM_CASRD_SH_DH$lrt) #0.1912555
analogR(PMM_CASRD_RH$lrt, PMM_CASRD_RH_DH$lrt) #0.1923311
#Conclusion: The best model here is likely {CASRD}{AH}{DH}, with
#age_group having a stronger relationship to hosp_yn than death_yn.
#We may also want to pay attention to death_yn and case_month in
#the following models.

#for comparison:
(PMM_CASRD_CH_AH_SH_RH_DH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month","age_group","sex","race","death_yn"),
c("case_month","hosp_yn"), c("age_group","hosp_yn"), c("sex","hosp_yn"), c("race","hosp_yn"), c("death_yn","hosp_yn"))))

(PMM_CASRD_ADH_CSRH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month","age_group","sex","race","death_yn"), c("age_group","death_yn","hosp_yn"), c("case_month","sex","race","hosp_yn"))))
(PMM_CASRD_RDH_CSAH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month","age_group","sex","race","death_yn"), c("race","death_yn","hosp_yn"), c("case_month","sex","age_group","hosp_yn"))))
(PMM_CASRD_ARH_CSDH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month","age_group","sex","race","death_yn"), c("age_group","race","hosp_yn"), c("case_month","sex","death_yn","hosp_yn"))))
(PMM_CASRD_CSH_ARDH <- loglin(as.table(DataPMMComplete_1_table),
margin = list(c("case_month","age_group","sex","race","death_yn"), c("case_month","sex","hosp_yn"), c("age_group","race","death_yn","hosp_yn"))))

analogR(PMM_CASRD_CH_AH_SH_RH_DH$lrt, PMM_CASRD_ADH_CSRH$lrt) #0.3164127
analogR(PMM_CASRD_CH_AH_SH_RH_DH$lrt, PMM_CASRD_RDH_CSAH$lrt) #0.5480027
analogR(PMM_CASRD_CH_AH_SH_RH_DH$lrt, PMM_CASRD_ARH_CSDH$lrt) #0.1957291
analogR(PMM_CASRD_CH_AH_SH_RH_DH$lrt, PMM_CASRD_CSH_ARDH$lrt) #0.2056887

#OVerall conclusion: The model {CASRD}{RDH}{CSAH} is the best-fitting model tested,
#with an lrt value of 21326.93 with df = 517. This is surprising, as we would expect
#from the previous models that age_group would work best with death_yn and maybe
#race, but {CASRD}{CSH}{ARDH} has an lrt value of 37478.59 with df = 517.
#(Note: I have also double-checked that the code is written correctly, and 
#to my knowledge, it is.)

analogR(PMM_CASRD_H$lrt, PMM_CASRD_RDH_CSAH$lrt)#0.8979439






