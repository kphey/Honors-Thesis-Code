
####IMPUTATION####
#This program assumes that the "Data" data frame developed in "ThesisR_DatasetInfo.R"
#is saved in the global environment.
library(Amelia)

#in the below amelia() function, m is the number of imputed
#datasets to be created, p2s is the level of detail desired
#in the output of the function, noms tells the function
#which variables are categorical (in our case, all of them),
#and boot.type tells the function whether or not to use
#bootstrapping in the EM Algorithm.

(EMAimp1_hosp <- amelia(Data, m = 1, p2s = 2,
noms = c("case_month", "age_group", "sex", "race", "hosp_yn"), boot.type = "none"))
#Abbreviated output:
# Amelia output with 5 imputed datasets.
# Return code:  1 
# Message:  Normal EM convergence. 
# 
# Chain Lengths:
#   --------------
# Imputation 1:  2
# Imputation 2:  2
# Imputation 3:  2
# Imputation 4:  2
# Imputation 5:  2

#Save the imputed datasets. These can be accessed via EMAimp1_hosp$imputations[[number]]
save(EMAimp1_hosp, file = "EMAimputations.RData")
unique(EMAimp1_hosp$imputations[[1]]$hosp_yn)
# [1] No  Yes
# Levels: No Yes

####Unedited as of 1/11/2022####

####LOG-LINEAR MODEL FITTING####
#First, we need to make a contingency table of the imputed data.
(DataEMAComplete_1_table <-
   ftable(hosp_yn ~ case_month + age_group + sex + race + death_yn, data = EMAimp1_hosp$imputations[[1]]))
#table not displayed here for brevity. Note that there are many zeros in the table.
for(i in 1:dim(DataEMAComplete_1_table)[1]) {
  for(j in 1:dim(DataEMAComplete_1_table)[2]) {
    DataEMAComplete_1_table[i, j] <- (DataEMAComplete_1_table[i, j] + .5)
  }
}
DataEMAComplete_1_table #all cells (including those with nonzero values) have a
#value of at least .5 to prevent potential errors due to structural zeros

#Just for reference, below is the analogR function. This does not need to
#be run if analogR is already in the global environment (under "Functions").
analogR <- function(Lbase, Lalt) {
  anlogRsq <- (Lbase - Lalt) / (Lbase)
  return(anlogRsq)
}

#Model selection




