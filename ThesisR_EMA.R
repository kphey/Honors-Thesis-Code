
####IMPUTATION####
#This program assumes that the "Data" data frame developed in "ThesisR_DatasetInfo.R"
#is saved in the global environment.
library(Amelia)
(EMAimp1_hosp <- amelia(Data, m = 5, p2s = 2,
noms = c("case_month", "age_group", "sex", "race", "hosp_yn", "death_yn"), boot.type = "none"))
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
#[CASRDH]
(EMA_CASRDH <- loglin(as.table(DataEMAComplete_1_table),
                      margin = list(c("case_month", "age_group", "sex", "race", "death_yn", "hosp_yn"))))
# $lrt
# [1] 0
# 
# $pearson
# [1] 0
# 
# $df
# [1] 0

(EMA_CASRD_H <- loglin(as.table(DataEMAComplete_1_table),
                       margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("hosp_yn"))))
# $lrt
# [1] 106393.3
# 
# $pearson
# [1] 147367.5
# 
# $df
# [1] 575

(EMA_CASRD_CH <- loglin(as.table(DataEMAComplete_1_table),
                        margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"))))
(EMA_CASRD_AH <- loglin(as.table(DataEMAComplete_1_table),
                        margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"))))
(EMA_CASRD_SH <- loglin(as.table(DataEMAComplete_1_table),
                        margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("sex","hosp_yn"))))
(EMA_CASRD_RH <- loglin(as.table(DataEMAComplete_1_table),
                        margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("race","hosp_yn"))))
(EMA_CASRD_DH <- loglin(as.table(DataEMAComplete_1_table),
                        margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("death_yn","hosp_yn"))))
analogR(EMA_CASRD_H$lrt, EMA_CASRD_CH$lrt) #0.008999553
analogR(EMA_CASRD_H$lrt, EMA_CASRD_AH$lrt) #0.7586583
analogR(EMA_CASRD_H$lrt, EMA_CASRD_SH$lrt) #0.005152421
analogR(EMA_CASRD_H$lrt, EMA_CASRD_RH$lrt) #0.007260104
analogR(EMA_CASRD_H$lrt, EMA_CASRD_DH$lrt) #0.3140109
#Conclusion: Only age_group has a somewhat significant relationship with hosp_yn. If we
#really didn't care about making a Type I error, we could also add death_yn.

#From here, we expect C, S, and R to not add anything
#significant to the fit of most models.
(EMA_CASRD_CH_AH <- loglin(as.table(DataEMAComplete_1_table),
                           margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("age_group","hosp_yn"))))
(EMA_CASRD_CH_SH <- loglin(as.table(DataEMAComplete_1_table),
                           margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("sex","hosp_yn"))))
(EMA_CASRD_CH_RH <- loglin(as.table(DataEMAComplete_1_table),
                           margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("race","hosp_yn"))))
(EMA_CASRD_CH_DH <- loglin(as.table(DataEMAComplete_1_table),
                           margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("case_month","hosp_yn"), c("death_yn","hosp_yn"))))

(EMA_CASRD_AH_SH <- loglin(as.table(DataEMAComplete_1_table),
                           margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"), c("sex","hosp_yn"))))
(EMA_CASRD_AH_RH <- loglin(as.table(DataEMAComplete_1_table),
                           margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"), c("race","hosp_yn"))))
(EMA_CASRD_AH_DH <- loglin(as.table(DataEMAComplete_1_table),
                           margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("age_group","hosp_yn"), c("death_yn","hosp_yn"))))

(EMA_CASRD_SH_RH <- loglin(as.table(DataEMAComplete_1_table),
                           margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("sex","hosp_yn"), c("race","hosp_yn"))))
(EMA_CASRD_SH_DH <- loglin(as.table(DataEMAComplete_1_table),
                           margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("sex","hosp_yn"), c("death_yn","hosp_yn"))))
(EMA_CASRD_RH_DH <- loglin(as.table(DataEMAComplete_1_table),
                           margin = list(c("case_month", "age_group", "sex", "race", "death_yn"), c("race","hosp_yn"), c("death_yn","hosp_yn"))))

analogR(EMA_CASRD_CH$lrt, EMA_CASRD_CH_AH$lrt) #0.7626294
#This seems favorable,
#but is likely only due to the inclusion of AH and does not necessarily
#mean that the model itself is the best one. We still reject this model,
#as CH adds very little to the model fit.
analogR(EMA_CASRD_CH$lrt, EMA_CASRD_CH_SH$lrt) #0.005242645
analogR(EMA_CASRD_CH$lrt, EMA_CASRD_CH_RH$lrt) #0.007834413
analogR(EMA_CASRD_CH$lrt, EMA_CASRD_CH_DH$lrt) #0.3179704

analogR(EMA_CASRD_AH$lrt, EMA_CASRD_AH_SH$lrt) #0.02768752
analogR(EMA_CASRD_AH$lrt, EMA_CASRD_AH_RH$lrt) #0.1562703
analogR(EMA_CASRD_AH$lrt, EMA_CASRD_AH_DH$lrt) #0.7171307
#DH does not add a lot to the model, which is already a good one
#due to AH. Compared to the previous models, however,
#this may be the best model.

analogR(EMA_CASRD_SH$lrt, EMA_CASRD_SH_RH$lrt) #0.007660046
analogR(EMA_CASRD_SH$lrt, EMA_CASRD_SH_DH$lrt) #0.3139205
analogR(EMA_CASRD_RH$lrt, EMA_CASRD_RH_DH$lrt) #0.3165294
#Conclusion: The best model here is likely {CASRD}{AH}{DH}, with
#age_group having a stronger relationship to hosp_yn than death_yn.
#As in the results from the PMMI dataset, combinations of A and D with
#less impactful variables may lead to better-fitting models.

#for comparison:
(EMA_CASRD_CH_AH_SH_RH_DH <- loglin(as.table(DataEMAComplete_1_table),
                                    margin = list(c("case_month","age_group","sex","race","death_yn"),
                                                  c("case_month","hosp_yn"), c("age_group","hosp_yn"), c("sex","hosp_yn"), c("race","hosp_yn"), c("death_yn","hosp_yn"))))

(EMA_CASRD_ADH_CSRH <- loglin(as.table(DataEMAComplete_1_table),
                              margin = list(c("case_month","age_group","sex","race","death_yn"), c("age_group","death_yn","hosp_yn"), c("case_month","sex","race","hosp_yn"))))
(EMA_CASRD_RDH_CSAH <- loglin(as.table(DataEMAComplete_1_table),
                              margin = list(c("case_month","age_group","sex","race","death_yn"), c("race","death_yn","hosp_yn"), c("case_month","sex","age_group","hosp_yn"))))
(EMA_CASRD_ARH_CSDH <- loglin(as.table(DataEMAComplete_1_table),
                              margin = list(c("case_month","age_group","sex","race","death_yn"), c("age_group","race","hosp_yn"), c("case_month","sex","death_yn","hosp_yn"))))
(EMA_CASRD_CSH_ARDH <- loglin(as.table(DataEMAComplete_1_table),
                              margin = list(c("case_month","age_group","sex","race","death_yn"), c("case_month","sex","hosp_yn"), c("age_group","race","death_yn","hosp_yn"))))

analogR(EMA_CASRD_CH_AH_SH_RH_DH$lrt, EMA_CASRD_ADH_CSRH$lrt) #0.2278811
analogR(EMA_CASRD_CH_AH_SH_RH_DH$lrt, EMA_CASRD_RDH_CSAH$lrt) #0.5022923
analogR(EMA_CASRD_CH_AH_SH_RH_DH$lrt, EMA_CASRD_ARH_CSDH$lrt) #0.1378025
analogR(EMA_CASRD_CH_AH_SH_RH_DH$lrt, EMA_CASRD_CSH_ARDH$lrt) #0.2396087

#Overall conclusion: The model{CASRD}{RDH}{CSAH} is the best-fitting model tested,
#with an lrt value of 889.7408 with df = 517. This is congruent with the results
#of the log-linear model fitting with the PMMI dataset.

analogR(EMA_CASRD_H$lrt, EMA_CASRD_RDH_CSAH$lrt)#0.9916372







