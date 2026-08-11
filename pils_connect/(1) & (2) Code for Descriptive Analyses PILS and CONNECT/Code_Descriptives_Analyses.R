############################################################################################
##                                                                                        ##
## This file contains the R code that we used to analyze data for:                        ##
## Geukes, K., Breil, S.M., Hutteman, R., Nestler, S., K?fner, C. P., & Back, M.B. (2018).##
## Explaining the Longitudinal Interplay of Personality and Social Relationships in the   ##
## Laboratory and in the Field: The PILS and CONNECT Study                                ##
## Katharina Geukes (katharina.geukes@uni-muenster.de)                                    ##
##                                                                                        ##
############################################################################################

## GENERAL COMMENTS:
##
## If not indicated otherwise,
## 1) we calculated Pearson Correlations
## 2) we calculated Cronbach Alpha values as scale reliabilities
## 3) we calculated ICC (2,k) as reliabilities of ratings or codings
## 4) we report unstandardized Perceiver-/Target-/Relationship-Variances.
##
## Furthermore: In Tables 11, 14, 17, & 18 we report mean value of other-perceptions.
## These means reflect how the average perceiver judged others on average.
## Thus, we computed multilevel models without the inclusion of target parameters.

## GETTING STARTED

### load necessary packages
#install.packages("lme4")
library(lme4) #for multilevel models
#install.packages("haven")
library(haven) #import of spss rawdata
#install.packages("dplyr")
library(dplyr) #data structuring
#install.packages("TripleR")
library(TripleR) #actor partner perceiver effects
#install.packages("psych")
library(psych) # psych functions e.g., alpha
#install.packages("effsize")
library(effsize) # effect size calulator

############################################################################################
## 1 Preparation for PILS                                                                 ##
############################################################################################

### set working directory
#setwd("B:/AEBack/02_Forschung/01_PaperinPreparation/Methodenpaper/Data_Pils/ohneoderrandomID") #please add your own working directory here

### load data

Pils_Traits <- read_sav("01_pils_onlinesurvey_self.sav",user_na = FALSE)
Pils_Traits_Acq_Agg <- read_sav("02_pils_onlinesurvey_acquaintance_aggregated.sav",user_na = FALSE)
Pils_Traits_Acq_Single <- read_sav("03_pils_onlinesurvey_acquaintance_single.sav",user_na = FALSE)
Pils_Session_Effects <- read_sav("05_pils_sessiondata_effects.sav",user_na = FALSE)
Pils_Session_Long <- read_sav("04_pils_sessiondata_long.sav",user_na = FALSE)
Pils_Cognitive <- read_sav("06_pils_directobservations_cognitiveabilities.sav",user_na = FALSE)
Pils_DirectObs_Agg <- read_sav("07_pils_directobservations_appearancebehavior_aggregated.sav",user_na = FALSE)
Pils_DirectObs_Single <- read_sav("08_pils_directobservations_appearancebehavior_single.sav",user_na = FALSE)
Pils_DirectObs_Group_Agg <- read_sav("09_pils_directobservations_groupratings_aggregated.sav",user_na = FALSE)
Pils_DirectObs_Group_Single <- read_sav("10_pils_directobservations_groupratings_single.sav",user_na = FALSE)

############################################################################################
## 2 Descriptive Results PILS                                                             ##
############################################################################################

############################################################################################
## A. TABLE 5                                                                             ##
############################################################################################

## SELF-REPORT (M & SD)

describe(Pils_Traits$age)
table(Pils_Traits$sex)
describe(Pils_Traits[,c("big5_n","big5_e","big5_o","big5_a","big5_c","rses","saq","npi","narq_adm","narq_riv","dido_mach","dido_psych","dido_narc")])
describe(Pils_Cognitive[,c("wmc_solved","wmc_span","mwtb_total","raven_total")]) #cognitive ability

## SELF-REPORT (alpha)
psych::alpha(as.data.frame(select(Pils_Traits, big5_5,big5_10,big5_15_r))) #N
psych::alpha(as.data.frame(select(Pils_Traits, big5_2,big5_8,big5_12_r))) #E
psych::alpha(as.data.frame(select(Pils_Traits, big5_4,big5_9,big5_14))) #O
psych::alpha(as.data.frame(select(Pils_Traits, big5_3_r,big5_6,big5_13,big5_16,big5_17_r))) #A
psych::alpha(as.data.frame(select(Pils_Traits, big5_1,big5_7_r,big5_11))) #C
psych::alpha(as.data.frame(select(Pils_Traits, rses_1, rses_2_r, rses_3, rses_4, rses_5_r, rses_6_r, rses_7, rses_8_r, rses_9_r, rses_10))) #RSES
psych::alpha(as.data.frame(select(Pils_Traits, saq_1, saq_2, saq_3, saq_4, saq_5, saq_6, saq_7, saq_8, saq_9, saq_attr))) #SAQ
psych::alpha(as.data.frame(select(Pils_Traits, npi_1, npi_2, npi_3, npi_4_r, npi_5_r, npi_6, npi_7_r, npi_8, npi_9_r, npi_10_r,
npi_11, npi_12, npi_13, npi_14, npi_15_r, npi_16, npi_17_r, npi_18_r, npi_19_r, npi_20_r, npi_21,npi_22_r, npi_23_r, npi_24, npi_25, npi_26_r, npi_27, npi_28_r, npi_29, npi_30, npi_31, npi_32_r, npi_33, npi_34, npi_35_r, npi_36, npi_37, npi_38, npi_39, npi_40_r))) #NPI
psych::alpha(as.data.frame(select(Pils_Traits, narq_1, narq_2, narq_3, narq_5, narq_7, narq_8, narq_15, narq_16, narq_18))) #NARQ-Adm
psych::alpha(as.data.frame(select(Pils_Traits, narq_4, narq_6, narq_9, narq_10, narq_11, narq_12, narq_13, narq_14, narq_17))) #NARQ-Riv
psych::alpha(as.data.frame(select(Pils_Traits, dido_1, dido_4, dido_7, dido_10))) #DIDO-Mach
psych::alpha(as.data.frame(select(Pils_Traits, dido_2, dido_5, dido_8, dido_11))) #DIDO-Psych
psych::alpha(as.data.frame(select(Pils_Traits, dido_3, dido_6, dido_9, dido_12))) #DIDO-Narc
####  ITEM wmc_5_8 excluded for analyses because of error message: "likely variables with missing values are  wmc_5_8  Error in principal(x, scores = FALSE) : I am sorry: missing values (NAs) in the correlation matrix do not allow me to continue."
psych::alpha(as.data.frame(select(Pils_Cognitive, wmc_4_11, wmc_4_12, wmc_4_13, wmc_4_14, wmc_4_16, wmc_4_17, wmc_4_18, wmc_4_19, wmc_5_1, wmc_5_2, wmc_5_3, wmc_5_4,wmc_5_5, wmc_5_7, wmc_5_9, wmc_5_10, wmc_5_11, wmc_6_1, wmc_6_2, wmc_6_3,wmc_6_4,wmc_6_5,wmc_6_6, wmc_6_8, wmc_6_9, wmc_6_10,wmc_6_11, wmc_6_12,wmc_6_13,wmc_7_1, wmc_7_2, wmc_7_3,wmc_7_4,wmc_7_5, wmc_7_6,wmc_7_7, wmc_7_9,wmc_7_10, wmc_7_11,wmc_7_12,wmc_7_13,wmc_7_14,wmc_7_15,wmc_8_1,wmc_8_2,wmc_8_3,wmc_8_4,wmc_8_5,wmc_8_6,wmc_8_7,wmc_8_8,wmc_8_10,wmc_8_11,wmc_8_12,wmc_8_13,wmc_8_14,wmc_8_15,wmc_8_16,wmc_8_17))) # WMC - solved equations
psych::alpha(as.data.frame(select(Pils_Cognitive, wmc_4_15, wmc_4_20, wmc_5_6, wmc_5_12, wmc_6_7, wmc_6_14, wmc_7_8, wmc_7_16, wmc_8_9, wmc_8_18))) #WMC - memory span
psych::alpha(as.data.frame(select(Pils_Cognitive, mwtb1, mwtb2, mwtb3, mwtb4, mwtb5,mwtb6,mwtb7,mwtb8,mwtb9,mwtb10,mwtb11,mwtb12,mwtb13,mwtb14,mwtb15,mwtb16,mwtb17,mwtb18,mwtb19,mwtb20,mwtb21,mwtb22,mwtb23,mwtb24,mwtb25,mwtb26,mwtb27,mwtb28,mwtb29,mwtb30,mwtb31,mwtb32,mwtb33,mwtb34,mwtb35,mwtb37))) #MWTB
psych::alpha(as.data.frame(select(Pils_Cognitive, raven1, raven2, raven3, raven4, raven5,raven6,raven7,raven8,raven9,raven10,raven11,raven12,raven13,raven14,raven15))) #raven

## INFORMANT-REPORT (M & SD)

## demographic information (Please note: information describing the informants, not the participants!)
describe(Pils_Traits_Acq_Single$age_acqu)
table(Pils_Traits_Acq_Single$sex_acqu)
describe(Pils_Traits_Acq_Agg[,c("big5_n_acqu","big5_e_acqu","big5_o_acqu","big5_a_acqu","big5_c_acqu","rses_acqu","saq_acqu","npi_acqu","narq_adm_acqu","narq_riv_acqu","dido_mach_acqu","dido_psych_acqu","dido_narc_acqu")])

## INFORMANT-REPORT (alpha)

psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, big5_5_acqu,big5_10_acqu,big5_15_acqu_r))) #N
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, big5_2_acqu,big5_8_acqu,big5_12_acqu_r))) #E
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, big5_4_acqu,big5_9_acqu,big5_14_acqu))) #O
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, big5_3_acqu_r,big5_6_acqu,big5_13_acqu,big5_16_acqu,big5_17_acqu_r))) #A
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, big5_1_acqu,big5_7_acqu_r,big5_11_acqu))) #C
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, rses_1_acqu, rses_2_acqu_r, rses_3_acqu, rses_4_acqu, rses_5_acqu_r, rses_6_acqu_r, rses_7_acqu, rses_8_acqu_r, rses_9_acqu_r, rses_10_acqu))) #RSES
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, saq_1_acqu, saq_2_acqu, saq_3_acqu, saq_4_acqu, saq_5_acqu, saq_6_acqu, saq_7_acqu, saq_8_acqu, saq_9_acqu, saq_attr_acqu))) #SAQ
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, npi_1_acqu,  npi_4_acqu_r,  npi_7_acqu_r,  npi_9_acqu_r, npi_10_acqu_r,
npi_12_acqu, npi_13_acqu,  npi_18_acqu_r, npi_27_acqu,  npi_30_acqu,  npi_32_acqu_r, npi_33_acqu, npi_34_acqu,  npi_36_acqu,  npi_40_acqu_r))) #NPI Short
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, narq_1_acqu, narq_2_acqu, narq_3_acqu, narq_5_acqu, narq_7_acqu, narq_8_acqu, narq_15_acqu, narq_16_acqu, narq_18_acqu))) #NARQ-Adm
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, narq_4_acqu, narq_6_acqu, narq_9_acqu, narq_10_acqu, narq_11_acqu, narq_12_acqu, narq_13_acqu, narq_14_acqu, narq_17_acqu))) #NARQ-Riv
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, dido_1_acqu, dido_4_acqu, dido_7_acqu, dido_10_acqu))) #DIDO-Mach
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, dido_2_acqu, dido_5_acqu, dido_8_acqu, dido_11_acqu))) #DIDO-Psych
psych::alpha(as.data.frame(select(Pils_Traits_Acq_Agg, dido_3_acqu, dido_6_acqu, dido_9_acqu, dido_12_acqu))) #DIDO-Narc


############################################################################################
## B. TABLE 6                                                                            ##
############################################################################################

## SELF-REPORT: Correlations (top-right half in the table)

## get and merge relevant variables
PilsCorrelationSelf <- merge (as.data.frame(select(Pils_Traits, id_a, big5_n, big5_e, big5_o, big5_a, big5_c,rses, saq, npi, narq_adm, narq_riv, dido_mach, dido_psych, dido_narc)),as.data.frame(select(Pils_Cognitive, id_a, wmc_solved, wmc_span, mwtb_total, raven_total)), by ="id_a" )

## compute self-report correlations
Table10Selfvalues <- round (corr.test(PilsCorrelationSelf[,c("big5_n", "big5_e", "big5_o", "big5_a", "big5_c","rses", "saq", "npi", "narq_adm", "narq_riv", "dido_mach", "dido_psych", "dido_narc","wmc_solved", "wmc_span","mwtb_total", "raven_total")],adjust ="none")[[1]], digits =2)
Table10Selfvalues

## get Ns for self-report correlations
round(corr.test(PilsCorrelationSelf[,c("big5_n", "big5_e", "big5_o", "big5_a", "big5_c","rses", "saq", "npi", "narq_adm", "narq_riv", "dido_mach", "dido_psych", "dido_narc","wmc_solved", "wmc_span","mwtb_total", "raven_total")],adjust ="none")[[2]])

## get significances (p-values) for self-report correlations
Table10SelfSignificance <- round (corr.test(PilsCorrelationSelf[,c("big5_n", "big5_e", "big5_o", "big5_a", "big5_c","rses", "saq", "npi", "narq_adm", "narq_riv", "dido_mach", "dido_psych", "dido_narc","wmc_solved", "wmc_span","mwtb_total", "raven_total")],adjust ="none")[[4]], digits =3)
Table10SelfSignificance

## Compute correlations of self-reports with informant-reports
## diagonal elements of the table

## get and merge relevant variables
PilsCorrelationSelfAcqu <- merge (as.data.frame(select(Pils_Traits, id_a, big5_n, big5_e, big5_o, big5_a, big5_c,rses, saq, npi, npi_short, narq_adm, narq_riv, dido_mach, dido_psych, dido_narc)),as.data.frame(select(Pils_Traits_Acq_Agg, id_a, big5_n_acqu, big5_e_acqu, big5_o_acqu, big5_a_acqu, big5_c_acqu,rses_acqu, saq_acqu, npi_acqu, narq_adm_acqu, narq_riv_acqu, dido_mach_acqu, dido_psych_acqu, dido_narc_acqu)), by ="id_a" )

## compute self-/informant-report correlations
Table10Acquvalues <- round(corr.test(PilsCorrelationSelfAcqu[,c("big5_n", "big5_e", "big5_o", "big5_a", "big5_c","rses", "saq", "npi", "npi_short", "narq_adm", "narq_riv", "dido_mach", "dido_psych", "dido_narc","big5_n_acqu", "big5_e_acqu", "big5_o_acqu", "big5_a_acqu", "big5_c_acqu","rses_acqu", "saq_acqu", "npi_acqu", "narq_adm_acqu", "narq_riv_acqu", "dido_mach_acqu", "dido_psych_acqu", "dido_narc_acqu")],adjust ="none")[[1]], digits =2)

## only the values in the diagonal of the following correlation matrix are in the
## diagonal of Table 10
Table10Acquvalues[c("big5_n", "big5_e", "big5_o", "big5_a", "big5_c","rses", "saq", "npi_short", "narq_adm", "narq_riv", "dido_mach", "dido_psych", "dido_narc"),c("big5_n_acqu", "big5_e_acqu", "big5_o_acqu", "big5_a_acqu", "big5_c_acqu","rses_acqu", "saq_acqu", "npi_acqu", "narq_adm_acqu", "narq_riv_acqu", "dido_mach_acqu", "dido_psych_acqu", "dido_narc_acqu")]

## get Ns for self-/informant-report correlations
Table10Acquvalues <- round (corr.test(PilsCorrelationSelfAcqu[,c("big5_n", "big5_e", "big5_o", "big5_a", "big5_c","rses", "saq", "npi", "npi_short", "narq_adm", "narq_riv", "dido_mach", "dido_psych", "dido_narc","big5_n_acqu", "big5_e_acqu", "big5_o_acqu", "big5_a_acqu", "big5_c_acqu","rses_acqu", "saq_acqu", "npi_acqu", "narq_adm_acqu", "narq_riv_acqu", "dido_mach_acqu", "dido_psych_acqu", "dido_narc_acqu")],adjust ="none")[[2]])
Table10Acquvalues[c("big5_n", "big5_e", "big5_o", "big5_a", "big5_c","rses", "saq", "npi", "npi_short", "narq_adm", "narq_riv", "dido_mach", "dido_psych", "dido_narc"),c("big5_n_acqu", "big5_e_acqu", "big5_o_acqu", "big5_a_acqu", "big5_c_acqu","rses_acqu", "saq_acqu", "npi_acqu", "narq_adm_acqu", "narq_riv_acqu", "dido_mach_acqu", "dido_psych_acqu", "dido_narc_acqu")]

#Significance self with acquaintance
Table2AcquSignificance <- round (corr.test(PilsCorrelationSelfAcqu[,c("big5_n", "big5_e", "big5_o", "big5_a", "big5_c","rses", "saq", "npi", "npi_short", "narq_adm", "narq_riv", "dido_mach", "dido_psych", "dido_narc","big5_n_acqu", "big5_e_acqu", "big5_o_acqu", "big5_a_acqu", "big5_c_acqu","rses_acqu", "saq_acqu", "npi_acqu", "narq_adm_acqu", "narq_riv_acqu", "dido_mach_acqu", "dido_psych_acqu", "dido_narc_acqu")],adjust ="none")[[4]], digits =3)
Table2AcquSignificance[c("big5_n", "big5_e", "big5_o", "big5_a", "big5_c","rses", "saq", "npi", "npi_short", "narq_adm", "narq_riv", "dido_mach", "dido_psych", "dido_narc"),c("big5_n_acqu", "big5_e_acqu", "big5_o_acqu", "big5_a_acqu", "big5_c_acqu","rses_acqu", "saq_acqu", "npi_acqu", "narq_adm_acqu", "narq_riv_acqu", "dido_mach_acqu", "dido_psych_acqu", "dido_narc_acqu")]


############################################################################################
## C. TABLE 7                                                                            ##
############################################################################################

## Selfperceptions: Subset of only self perceptions and only timepoints 1 to 10; timepoint 11 was retrospective
Pils_Session_Selfperceptions <- as.data.frame(subset(Pils_Session_Long, Pils_Session_Long$id_a == Pils_Session_Long$id_p & Pils_Session_Long$timepoint < 11))

## Self-perceptions liking to rivalry (M, SDwithin, SDbetween)
summary(lmer(liking ~ 1 + (1|id_a), data = Pils_Session_Selfperceptions))
summary(lmer(annoying ~ 1 + (1|id_a), data = Pils_Session_Selfperceptions))
summary(lmer(assertiveness ~ 1 + (1|id_a), data = Pils_Session_Selfperceptions))
summary(lmer(trustworthiness ~ 1 + (1|id_a), data = Pils_Session_Selfperceptions))
summary(lmer(intelligence ~ 1 + (1|id_a), data = Pils_Session_Selfperceptions))
summary(lmer(attractiveness ~ 1 + (1|id_a), data = Pils_Session_Selfperceptions))
summary(lmer(admiration ~ 1 + (1|id_a), data = Pils_Session_Selfperceptions))
summary(lmer(rivalry ~ 1 + (1|id_a), data = Pils_Session_Selfperceptions))

## Self-perceptions: Affect grid & State effect (M, SDwithin, SDbetween)
## data is in wide format
## 1) select relevant timepoints for each variable
## 2) reshape data to long format
## 3) multilevel model for

## Affect grid
Pils_Session_Selfperceptions_pleasure <- as.data.frame(Pils_Session_Effects[c("t1_stateaffect_pleasure","t2_stateaffect_pleasure","t3_stateaffect_pleasure","t4_stateaffect_pleasure","t5_stateaffect_pleasure","t6_stateaffect_pleasure","t7_stateaffect_pleasure","t8_stateaffect_pleasure","t9_stateaffect_pleasure","t10_stateaffect_pleasure")])
summary(lmer(pleasure ~ 1 + (1|id), data = reshape(Pils_Session_Selfperceptions_pleasure,varying = c(1:10),v.names = "pleasure",timevar = "t",direction = "long"))) #pleasure

Pils_Session_Selfperceptions_arousal <- as.data.frame(Pils_Session_Effects[c("t1_stateaffect_arousal","t2_stateaffect_arousal","t3_stateaffect_arousal","t4_stateaffect_arousal","t5_stateaffect_arousal","t6_stateaffect_arousal","t7_stateaffect_arousal","t8_stateaffect_arousal","t9_stateaffect_arousal","t10_stateaffect_arousal")])
summary(lmer(arousal ~ 1 + (1|id), data = reshape(Pils_Session_Selfperceptions_arousal,varying = c(1:10),v.names = "arousal",timevar = "t",direction = "long"))) #arousal

## state Affect / State Self-Esteem
Pils_Session_Selfperceptions_Active <- as.data.frame(Pils_Session_Effects[c("t1_stateaffect_active","t2_stateaffect_active","t3_stateaffect_active","t4_stateaffect_active","t5_stateaffect_active","t6_stateaffect_active","t7_stateaffect_active","t8_stateaffect_active","t9_stateaffect_active","t10_stateaffect_active")])
summary(lmer(active ~ 1 + (1|id), data = reshape(Pils_Session_Selfperceptions_Active,varying = c(1:10),v.names = "active",timevar = "t",direction = "long"))) #Active

Pils_Session_Selfperceptions_optimistic <- as.data.frame(Pils_Session_Effects[c("t1_stateaffect_optimistic","t2_stateaffect_optimistic","t3_stateaffect_optimistic","t4_stateaffect_optimistic","t5_stateaffect_optimistic","t6_stateaffect_optimistic","t7_stateaffect_optimistic","t8_stateaffect_optimistic","t9_stateaffect_optimistic","t10_stateaffect_optimistic")])
summary(lmer(optimistic ~ 1 + (1|id), data = reshape(Pils_Session_Selfperceptions_optimistic,varying = c(1:10),v.names = "optimistic",timevar = "t",direction = "long"))) #optimistic

Pils_Session_Selfperceptions_inhibited <- as.data.frame(Pils_Session_Effects[c("t1_stateaffect_inhibited","t2_stateaffect_inhibited","t3_stateaffect_inhibited","t4_stateaffect_inhibited","t5_stateaffect_inhibited","t6_stateaffect_inhibited","t7_stateaffect_inhibited","t8_stateaffect_inhibited","t9_stateaffect_inhibited","t10_stateaffect_inhibited")])
summary(lmer(inhibited ~ 1 + (1|id), data = reshape(Pils_Session_Selfperceptions_inhibited,varying = c(1:10),v.names = "inhibited",timevar = "t",direction = "long"))) #inhibited

Pils_Session_Selfperceptions_determined <- as.data.frame(Pils_Session_Effects[c("t1_stateaffect_determined","t2_stateaffect_determined","t3_stateaffect_determined","t4_stateaffect_determined","t5_stateaffect_determined","t6_stateaffect_determined","t7_stateaffect_determined","t8_stateaffect_determined","t9_stateaffect_determined","t10_stateaffect_determined")])
summary(lmer(determined ~ 1 + (1|id), data = reshape(Pils_Session_Selfperceptions_determined,varying = c(1:10),v.names = "determined",timevar = "t",direction = "long"))) #determined

Pils_Session_Selfperceptions_satisfaction <- as.data.frame(Pils_Session_Effects[c("t1_stateaffect_satisfaction","t2_stateaffect_satisfaction","t3_stateaffect_satisfaction","t4_stateaffect_satisfaction","t5_stateaffect_satisfaction","t6_stateaffect_satisfaction","t7_stateaffect_satisfaction","t8_stateaffect_satisfaction","t9_stateaffect_satisfaction","t10_stateaffect_satisfaction")])
summary(lmer(satisfaction ~ 1 + (1|id), data = reshape(Pils_Session_Selfperceptions_satisfaction,varying = c(1:10),v.names = "satisfaction",timevar = "t",direction = "long"))) #satisfaction

Pils_Session_Selfperceptions_trusting <- as.data.frame(Pils_Session_Effects[c("t1_stateaffect_trusting","t2_stateaffect_trusting","t3_stateaffect_trusting","t4_stateaffect_trusting","t5_stateaffect_trusting","t6_stateaffect_trusting","t7_stateaffect_trusting","t8_stateaffect_trusting","t9_stateaffect_trusting","t10_stateaffect_trusting")])
summary(lmer(trusting ~ 1 + (1|id), data = reshape(Pils_Session_Selfperceptions_trusting,varying = c(1:10),v.names = "trusting",timevar = "t",direction = "long"))) #trusting

Pils_Session_Selfperceptions_satisfied <- as.data.frame(Pils_Session_Effects[c("t1_stateaffect_satisfied","t2_stateaffect_satisfied","t3_stateaffect_satisfied","t4_stateaffect_satisfied","t5_stateaffect_satisfied","t6_stateaffect_satisfied","t7_stateaffect_satisfied","t8_stateaffect_satisfied","t9_stateaffect_satisfied","t10_stateaffect_satisfied")])
summary(lmer(satisfied ~ 1 + (1|id), data = reshape(Pils_Session_Selfperceptions_satisfied,varying = c(1:10),v.names = "satisfied",timevar = "t",direction = "long"))) #satisfied


## Self-Perceptions: Leader and Friend
summary(lmer(leader ~ 1 + (1|id_a), data = Pils_Session_Selfperceptions))
summary(lmer(friend ~ 1 + (1|id_a), data = Pils_Session_Selfperceptions))

## Self-perceptions: Dating and Mating Indicators (M, SDwithin, SDbetween)
Pils_Session_Selfperceptions_DatingMating <- as.data.frame(subset(Pils_Session_Long, Pils_Session_Long$id_a == Pils_Session_Long$id_p))
describe(Pils_Session_Selfperceptions_DatingMating[,c("physical_appeal","date","short_love_affair","romantic_relationship")])

## Other perceptions: Subset of only other-perceptions
Pils_OtherPerception <- as.data.frame(subset(Pils_Session_Long, Pils_Session_Long$id_a != Pils_Session_Long$id_p & Pils_Session_Long$timepoint < 11))

## Other-perceptions (only Means; based on perceiver)

summary(lmer(knowing ~ 1 + (1|id_a), data = Pils_OtherPerception))
summary(lmer(liking ~ 1 + (1|id_a), data = Pils_OtherPerception))
summary(lmer(metaliking ~ 1 + (1|id_a), data = Pils_OtherPerception))
summary(lmer(annoying ~ 1 + (1|id_a), data = Pils_OtherPerception))
summary(lmer(assertiveness ~ 1 + (1|id_a), data = Pils_OtherPerception))
summary(lmer(trustworthiness ~ 1 + (1|id_a), data = Pils_OtherPerception))
summary(lmer(intelligence ~ 1 + (1|id_a), data = Pils_OtherPerception))
summary(lmer(attractiveness ~ 1 + (1|id_a), data = Pils_OtherPerception))
summary(lmer(admiration ~ 1 + (1|id_a), data = Pils_OtherPerception))
summary(lmer(rivalry ~ 1 + (1|id_a), data = Pils_OtherPerception))

summary(lmer(leader ~ 1 + (1|id_a), data = Pils_OtherPerception))
summary(lmer(friend ~ 1 + (1|id_a), data = Pils_OtherPerception))


Pils_OtherPerception_DatingMating <- as.data.frame(subset(Pils_Session_Long, Pils_Session_Long$id_a != Pils_Session_Long$id_p & Pils_Session_Long$group_number <= 59 ))

summary(lmer(physical_appeal ~ 1 + (1|id_a), data = Pils_OtherPerception_DatingMating))
summary(lmer(date ~ 1 + (1|id_a), data = Pils_OtherPerception_DatingMating))
summary(lmer(short_love_affair ~ 1 + (1|id_a), data = Pils_OtherPerception_DatingMating))
summary(lmer(romantic_relationship ~ 1 + (1|id_a), data = Pils_OtherPerception_DatingMating))

library(TripleR)

## Other-perceptions: Variances -- Perceiver / Target / Relationship Variance
RR1knowing <- RR(knowing~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
round (RR1knowing$varComp$estimate,digits =2)

RR1liking <- RR(liking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2liking <- RR(liking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3liking <- RR(liking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4liking <- RR(liking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5liking <- RR(liking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6liking <- RR(liking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7liking <- RR(liking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8liking <- RR(liking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9liking <- RR(liking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10liking <- RR(liking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1liking$varComp$estimate+RR2liking$varComp$estimate+RR3liking$varComp$estimate+RR4liking$varComp$estimate+RR5liking$varComp$estimate+RR6liking$varComp$estimate+RR7liking$varComp$estimate+RR8liking$varComp$estimate+RR9liking$varComp$estimate+RR10liking$varComp$estimate) / 10), digits =2)

RR1metaliking <- RR(metaliking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2metaliking <- RR(metaliking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3metaliking <- RR(metaliking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4metaliking <- RR(metaliking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5metaliking <- RR(metaliking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6metaliking <- RR(metaliking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7metaliking <- RR(metaliking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8metaliking <- RR(metaliking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9metaliking <- RR(metaliking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10metaliking <- RR(metaliking~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1metaliking$varComp$estimate+RR2metaliking$varComp$estimate+RR3metaliking$varComp$estimate+RR4metaliking$varComp$estimate+RR5metaliking$varComp$estimate+RR6metaliking$varComp$estimate+RR7metaliking$varComp$estimate+RR8metaliking$varComp$estimate+RR9metaliking$varComp$estimate+RR10metaliking$varComp$estimate) / 10), digits =2)

RR1annoying <- RR(annoying~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2annoying <- RR(annoying~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3annoying <- RR(annoying~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4annoying <- RR(annoying~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5annoying <- RR(annoying~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6annoying <- RR(annoying~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7annoying <- RR(annoying~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8annoying <- RR(annoying~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9annoying <- RR(annoying~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10annoying <- RR(annoying~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1annoying$varComp$estimate+RR2annoying$varComp$estimate+RR3annoying$varComp$estimate+RR4annoying$varComp$estimate+RR5annoying$varComp$estimate+RR6annoying$varComp$estimate+RR7annoying$varComp$estimate+RR8annoying$varComp$estimate+RR9annoying$varComp$estimate+RR10annoying$varComp$estimate) / 10), digits =2)

RR1assertiveness <- RR(assertiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2assertiveness <- RR(assertiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3assertiveness <- RR(assertiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4assertiveness <- RR(assertiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5assertiveness <- RR(assertiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6assertiveness <- RR(assertiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7assertiveness <- RR(assertiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8assertiveness <- RR(assertiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9assertiveness <- RR(assertiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10assertiveness <- RR(assertiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1assertiveness$varComp$estimate+RR2assertiveness$varComp$estimate+RR3assertiveness$varComp$estimate+RR4assertiveness$varComp$estimate+RR5assertiveness$varComp$estimate+RR6assertiveness$varComp$estimate+RR7assertiveness$varComp$estimate+RR8assertiveness$varComp$estimate+RR9assertiveness$varComp$estimate+RR10assertiveness$varComp$estimate) / 10), digits =2)

RR1trustworthiness <- RR(trustworthiness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2trustworthiness <- RR(trustworthiness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3trustworthiness <- RR(trustworthiness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4trustworthiness <- RR(trustworthiness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5trustworthiness <- RR(trustworthiness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6trustworthiness <- RR(trustworthiness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7trustworthiness <- RR(trustworthiness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8trustworthiness <- RR(trustworthiness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9trustworthiness <- RR(trustworthiness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10trustworthiness <- RR(trustworthiness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1trustworthiness$varComp$estimate+RR2trustworthiness$varComp$estimate+RR3trustworthiness$varComp$estimate+RR4trustworthiness$varComp$estimate+RR5trustworthiness$varComp$estimate+RR6trustworthiness$varComp$estimate+RR7trustworthiness$varComp$estimate+RR8trustworthiness$varComp$estimate+RR9trustworthiness$varComp$estimate+RR10trustworthiness$varComp$estimate) / 10), digits =2)

RR1intelligence <- RR(intelligence~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2intelligence <- RR(intelligence~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3intelligence <- RR(intelligence~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4intelligence <- RR(intelligence~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5intelligence <- RR(intelligence~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6intelligence <- RR(intelligence~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7intelligence <- RR(intelligence~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8intelligence <- RR(intelligence~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9intelligence <- RR(intelligence~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10intelligence <- RR(intelligence~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1intelligence$varComp$estimate+RR2intelligence$varComp$estimate+RR3intelligence$varComp$estimate+RR4intelligence$varComp$estimate+RR5intelligence$varComp$estimate+RR6intelligence$varComp$estimate+RR7intelligence$varComp$estimate+RR8intelligence$varComp$estimate+RR9intelligence$varComp$estimate+RR10intelligence$varComp$estimate) / 10), digits =2)

RR1attractiveness <- RR(attractiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2attractiveness <- RR(attractiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3attractiveness <- RR(attractiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4attractiveness <- RR(attractiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5attractiveness <- RR(attractiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6attractiveness <- RR(attractiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7attractiveness <- RR(attractiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8attractiveness <- RR(attractiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9attractiveness <- RR(attractiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10attractiveness <- RR(attractiveness~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1attractiveness$varComp$estimate+RR2attractiveness$varComp$estimate+RR3attractiveness$varComp$estimate+RR4attractiveness$varComp$estimate+RR5attractiveness$varComp$estimate+RR6attractiveness$varComp$estimate+RR7attractiveness$varComp$estimate+RR8attractiveness$varComp$estimate+RR9attractiveness$varComp$estimate+RR10attractiveness$varComp$estimate) / 10), digits =2)

RR1admiration <- RR(admiration~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2admiration <- RR(admiration~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3admiration <- RR(admiration~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4admiration <- RR(admiration~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5admiration <- RR(admiration~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6admiration <- RR(admiration~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7admiration <- RR(admiration~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8admiration <- RR(admiration~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9admiration <- RR(admiration~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10admiration <- RR(admiration~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1admiration$varComp$estimate+RR2admiration$varComp$estimate+RR3admiration$varComp$estimate+RR4admiration$varComp$estimate+RR5admiration$varComp$estimate+RR6admiration$varComp$estimate+RR7admiration$varComp$estimate+RR8admiration$varComp$estimate+RR9admiration$varComp$estimate+RR10admiration$varComp$estimate) / 10), digits =2)

RR1rivalry <- RR(rivalry~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2rivalry <- RR(rivalry~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3rivalry <- RR(rivalry~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4rivalry <- RR(rivalry~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5rivalry <- RR(rivalry~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6rivalry <- RR(rivalry~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7rivalry <- RR(rivalry~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8rivalry <- RR(rivalry~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9rivalry <- RR(rivalry~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10rivalry <- RR(rivalry~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1rivalry$varComp$estimate+RR2rivalry$varComp$estimate+RR3rivalry$varComp$estimate+RR4rivalry$varComp$estimate+RR5rivalry$varComp$estimate+RR6rivalry$varComp$estimate+RR7rivalry$varComp$estimate+RR8rivalry$varComp$estimate+RR9rivalry$varComp$estimate+RR10rivalry$varComp$estimate) / 10), digits =2)

RR1leader <- RR(leader~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2leader <- RR(leader~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3leader <- RR(leader~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4leader <- RR(leader~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5leader <- RR(leader~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6leader <- RR(leader~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7leader <- RR(leader~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8leader <- RR(leader~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9leader <- RR(leader~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10leader <- RR(leader~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1leader$varComp$estimate+RR2leader$varComp$estimate+RR3leader$varComp$estimate+RR4leader$varComp$estimate+RR5leader$varComp$estimate+RR6leader$varComp$estimate+RR7leader$varComp$estimate+RR8leader$varComp$estimate+RR9leader$varComp$estimate+RR10leader$varComp$estimate) / 10), digits =2)

RR1friend <- RR(friend~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 1)), na.rm=TRUE)
RR2friend <- RR(friend~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 2)), na.rm=TRUE)
RR3friend <- RR(friend~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 3)), na.rm=TRUE)
RR4friend <- RR(friend~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 4)), na.rm=TRUE)
RR5friend <- RR(friend~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 5)), na.rm=TRUE)
RR6friend <- RR(friend~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 6)), na.rm=TRUE)
RR7friend <- RR(friend~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 7)), na.rm=TRUE)
RR8friend <- RR(friend~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 8)), na.rm=TRUE)
RR9friend <- RR(friend~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 9)), na.rm=TRUE)
RR10friend <- RR(friend~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception, Pils_OtherPerception$timepoint == 10)), na.rm=TRUE)
round(((RR1friend$varComp$estimate+RR2friend$varComp$estimate+RR3friend$varComp$estimate+RR4friend$varComp$estimate+RR5friend$varComp$estimate+RR6friend$varComp$estimate+RR7friend$varComp$estimate+RR8friend$varComp$estimate+RR9friend$varComp$estimate+RR10friend$varComp$estimate) / 10), digits =2)

## Subset only same sex groups for variance analyses regarding physical appeal, date, short love affai, romantic relationship (for mixed sex groups we had no full round-robin design) and timepoint 11
RR(physical_appeal~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception_DatingMating, Pils_OtherPerception_DatingMating$timepoint == 11 & Pils_OtherPerception_DatingMating$group_number <= 59 )), na.rm=TRUE)
RR(date~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception_DatingMating, Pils_OtherPerception_DatingMating$timepoint == 11 & Pils_OtherPerception_DatingMating$group_number <= 59 )), na.rm=TRUE)
RR(short_love_affair~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception_DatingMating, Pils_OtherPerception_DatingMating$timepoint == 11 & Pils_OtherPerception_DatingMating$group_number <= 59 )), na.rm=TRUE)
RR(romantic_relationship~id_a*id_p|group_number, data= as.data.frame(subset(Pils_OtherPerception_DatingMating, Pils_OtherPerception_DatingMating$timepoint == 11 & Pils_OtherPerception_DatingMating$group_number <= 59 )), na.rm=TRUE)


############################################################################################
## D. TABLE 8                                                                            ##
############################################################################################

## TASK A: Means and SDs: Attractiveness & Task A to Task G
describe(Pils_DirectObs_Agg[,c("taskA_cheerfulness_of_voice","taskA_nervousness","taskA_attention","taskA_verbal_fluency","taskA_intelligence")])

## TASK: ICCs
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskA_cheerfulness_of_voice_R1  ,	taskA_cheerfulness_of_voice_R2	,	taskA_cheerfulness_of_voice_R3	,	taskA_cheerfulness_of_voice_R4	,	taskA_cheerfulness_of_voice_R5))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskA_nervousness_R1	,	taskA_nervousness_R2	,	taskA_nervousness_R3	,	taskA_nervousness_R4	,	taskA_nervousness_R5	,	taskA_nervousness_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskA_attention_R1	,	taskA_attention_R2	,	taskA_attention_R3	,	taskA_attention_R4	,	taskA_attention_R5	,	taskA_attention_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskA_verbal_fluency_R1	,	taskA_verbal_fluency_R2	,	taskA_verbal_fluency_R3	,	taskA_verbal_fluency_R4	,	taskA_verbal_fluency_R5	,	taskA_verbal_fluency_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskA_intelligence_R1	,	taskA_intelligence_R2	,	taskA_intelligence_R3	,	taskA_intelligence_R4	,	taskA_intelligence_R5	,	taskA_intelligence_R6))))

## TASK B
describe(Pils_DirectObs_Agg[,c("taskB_expressive_behavior","taskB_dominant_behavior","taskB_arrogant_behavior","taskB_friendly_behavior","taskB_nervous_behavior")])

## TASK B: ICCs
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskB_expressive_behavior_R1  ,	taskB_expressive_behavior_R2	,	taskB_expressive_behavior_R3))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskB_dominant_behavior_R1	,	taskB_dominant_behavior_R2	,	taskB_dominant_behavior_R3))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskB_arrogant_behavior_R1	,	taskB_arrogant_behavior_R2	,	taskB_arrogant_behavior_R3))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskB_friendly_behavior_R1	,	taskB_friendly_behavior_R2	,	taskB_friendly_behavior_R3))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskB_nervous_behavior_R1	,	taskB_nervous_behavior_R2	,	taskB_nervous_behavior_R3))))

## TASK C
describe(Pils_DirectObs_Agg[,c("taskC_expressive_behavior","taskC_dominant_behavior","taskC_arrogant_behavior","taskC_friendly_behavior","taskC_nervous_behavior")])

## TASK C: ICCs
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskC_expressive_behavior_R1  ,	taskC_expressive_behavior_R2	,	taskC_expressive_behavior_R3))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskC_dominant_behavior_R1	,	taskC_dominant_behavior_R2	,	taskC_dominant_behavior_R3))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskC_arrogant_behavior_R1	,	taskC_arrogant_behavior_R2	,	taskC_arrogant_behavior_R3))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskC_friendly_behavior_R1	,	taskC_friendly_behavior_R2	,	taskC_friendly_behavior_R3))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskC_nervous_behavior_R1	,	taskC_nervous_behavior_R2	,	taskC_nervous_behavior_R3))))

## TASK D
describe(Pils_DirectObs_Agg[,c("taskD_expressive_behavior","taskD_dominant_behavior","taskD_arrogant_behavior","taskD_cooperative_behavior","taskD_aggressive_behavior")])

## TASK D: ICCs
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskD_expressive_behavior_R1	,	taskD_expressive_behavior_R2	,	taskD_expressive_behavior_R3	,	taskD_expressive_behavior_R4	,	taskD_expressive_behavior_R5	,	taskD_expressive_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskD_dominant_behavior_R1	,	taskD_dominant_behavior_R2	,	taskD_dominant_behavior_R3	,	taskD_dominant_behavior_R4	,	taskD_dominant_behavior_R5	,	taskD_dominant_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskD_arrogant_behavior_R1	,	taskD_arrogant_behavior_R2	,	taskD_arrogant_behavior_R3	,	taskD_arrogant_behavior_R4	,	taskD_arrogant_behavior_R5	,	taskD_arrogant_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskD_cooperative_behavior_R1	,	taskD_cooperative_behavior_R2	,	taskD_cooperative_behavior_R3	,	taskD_cooperative_behavior_R4	,	taskD_cooperative_behavior_R5	,	taskD_cooperative_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskD_aggressive_behavior_R1  ,  taskD_aggressive_behavior_R2	,	taskD_aggressive_behavior_R3	,	taskD_aggressive_behavior_R4	,	taskD_aggressive_behavior_R5	,	taskD_aggressive_behavior_R6))))

## TASK E
describe(Pils_DirectObs_Agg[,c("taskE_expressive_behavior","taskE_dominant_behavior","taskE_arrogant_behavior","taskE_cooperative_behavior","taskE_aggressive_behavior")])

## TASK E: ICCs
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskE_expressive_behavior_R1	,	taskE_expressive_behavior_R2	,	taskE_expressive_behavior_R3	,	taskE_expressive_behavior_R4	,	taskE_expressive_behavior_R5	,	taskE_expressive_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskE_dominant_behavior_R1	,	taskE_dominant_behavior_R2	,	taskE_dominant_behavior_R3	,	taskE_dominant_behavior_R4	,	taskE_dominant_behavior_R5	,	taskE_dominant_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskE_arrogant_behavior_R1	,	taskE_arrogant_behavior_R2	,	taskE_arrogant_behavior_R3	,	taskE_arrogant_behavior_R4	,	taskE_arrogant_behavior_R5	,	taskE_arrogant_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskE_cooperative_behavior_R1	,	taskE_cooperative_behavior_R2	,	taskE_cooperative_behavior_R3	,	taskE_cooperative_behavior_R4	,	taskE_cooperative_behavior_R5	,	taskE_cooperative_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskE_aggressive_behavior_R1  ,  taskE_aggressive_behavior_R2	,	taskE_aggressive_behavior_R3	,	taskE_aggressive_behavior_R4	,	taskE_aggressive_behavior_R5	,	taskE_aggressive_behavior_R6))))

## TASK F
describe(Pils_DirectObs_Agg[,c("taskF_expressive_behavior","taskF_dominant_behavior","taskF_arrogant_behavior","taskF_cooperative_behavior","taskF_aggressive_behavior")])

## TASK F: ICCs
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskF_expressive_behavior_R1	,	taskF_expressive_behavior_R2	,	taskF_expressive_behavior_R3	,	taskF_expressive_behavior_R4	,	taskF_expressive_behavior_R5	,	taskF_expressive_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskF_dominant_behavior_R1	,	taskF_dominant_behavior_R2	,	taskF_dominant_behavior_R3	,	taskF_dominant_behavior_R4	,	taskF_dominant_behavior_R5	,	taskF_dominant_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskF_arrogant_behavior_R1	,	taskF_arrogant_behavior_R2	,	taskF_arrogant_behavior_R3	,	taskF_arrogant_behavior_R4	,	taskF_arrogant_behavior_R5	,	taskF_arrogant_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskF_cooperative_behavior_R1	,	taskF_cooperative_behavior_R2	,	taskF_cooperative_behavior_R3	,	taskF_cooperative_behavior_R4	,	taskF_cooperative_behavior_R5	,	taskF_cooperative_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskF_aggressive_behavior_R1  ,  taskF_aggressive_behavior_R2	,	taskF_aggressive_behavior_R3	,	taskF_aggressive_behavior_R4	,	taskF_aggressive_behavior_R5	,	taskF_aggressive_behavior_R6))))

## TASK G
describe(Pils_DirectObs_Agg[,c("taskG_expressive_behavior","taskG_dominant_behavior","taskG_arrogant_behavior","taskG_cooperative_behavior","taskG_aggressive_behavior")])

## TASK G: ICCs
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskG_expressive_behavior_R1	,	taskG_expressive_behavior_R2	,	taskG_expressive_behavior_R3	,	taskG_expressive_behavior_R4	,	taskG_expressive_behavior_R5	,	taskG_expressive_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskG_dominant_behavior_R1	,	taskG_dominant_behavior_R2	,	taskG_dominant_behavior_R3	,	taskG_dominant_behavior_R4	,	taskG_dominant_behavior_R5	,	taskG_dominant_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskG_arrogant_behavior_R1	,	taskG_arrogant_behavior_R2	,	taskG_arrogant_behavior_R3	,	taskG_arrogant_behavior_R4	,	taskG_arrogant_behavior_R5	,	taskG_arrogant_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskG_cooperative_behavior_R1	,	taskG_cooperative_behavior_R2	,	taskG_cooperative_behavior_R3	,	taskG_cooperative_behavior_R4	,	taskG_cooperative_behavior_R5	,	taskG_cooperative_behavior_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, taskG_aggressive_behavior_R1  ,  taskG_aggressive_behavior_R2	,	taskG_aggressive_behavior_R3	,	taskG_aggressive_behavior_R4	,	taskG_aggressive_behavior_R5	,	taskG_aggressive_behavior_R6))))

## Attractiveness
describe(Pils_DirectObs_Agg[,c("attractiveness_face","hardness_face","styled_hair","neatly_hair","overall_attractiveness","body_shape_r","neatly_clothes","modern_clothes","flashy_clothes","alternativeness_appearance","chic_appearance")])

## Attractiveness: ICCs
#ICCs (2,k): Attractiveness & Task A to Task G
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, attractiveness_face_R1  ,  attractiveness_face_R2	,	attractiveness_face_R3	,	attractiveness_face_R4	,	attractiveness_face_R5	,	attractiveness_face_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, hardness_face_R1	,	hardness_face_R2	,	hardness_face_R3	,	hardness_face_R4	,	hardness_face_R5	,	hardness_face_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, styled_hair_R1	,	styled_hair_R2	,	styled_hair_R3	,	styled_hair_R4	,	styled_hair_R5	,	styled_hair_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, neatly_hair_R1	,	neatly_hair_R2	,	neatly_hair_R3	,	neatly_hair_R4	,	neatly_hair_R5	,	neatly_hair_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, overall_attractiveness_R1	,	overall_attractiveness_R2	,	overall_attractiveness_R3	,	overall_attractiveness_R4	,	overall_attractiveness_R5	,	overall_attractiveness_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, body_shape_R1	,	body_shape_R2	,	body_shape_R3	,	body_shape_R4	,	body_shape_R5	,	body_shape_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, neatly_clothes_R1	,	neatly_clothes_R2	,	neatly_clothes_R3	,	neatly_clothes_R4	,	neatly_clothes_R5	,	neatly_clothes_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, modern_clothes_R1	,	modern_clothes_R2	,	modern_clothes_R3	,	modern_clothes_R4	,	modern_clothes_R5	,	modern_clothes_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, flashy_clothes_R1	,	flashy_clothes_R2	,	flashy_clothes_R3	,	flashy_clothes_R4	,	flashy_clothes_R5	,	flashy_clothes_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, alternativeness_appearance_R1	,	alternativeness_appearance_R2	,	alternativeness_appearance_R3	,	alternativeness_appearance_R4	,	alternativeness_appearance_R5	,	alternativeness_appearance_R6))))
ICC(na.omit(as.matrix(select(Pils_DirectObs_Single, chic_appearance_R1	,	chic_appearance_R2	,	chic_appearance_R3	,	chic_appearance_R4	,	chic_appearance_R5	,	chic_appearance_R6))))

## Group Ratings
## aggregate across tasks
Pils_DirectObs_Group_Agg$Group_Performance <- (Pils_DirectObs_Group_Agg$taskD_performance + Pils_DirectObs_Group_Agg$taskE_performance + Pils_DirectObs_Group_Agg$taskF_performance) / 3
Pils_DirectObs_Group_Agg$Group_Positive_Atmosphere <- (Pils_DirectObs_Group_Agg$taskD_positive_atmosphere + Pils_DirectObs_Group_Agg$taskE_positive_atmosphere + Pils_DirectObs_Group_Agg$taskF_positive_atmosphere) / 3
Pils_DirectObs_Group_Agg$Group_Conflicts <- (Pils_DirectObs_Group_Agg$taskD_conflicts + Pils_DirectObs_Group_Agg$taskE_conflicts + Pils_DirectObs_Group_Agg$taskF_conflicts) / 3

## Means and SDs
describe(Pils_DirectObs_Group_Agg[,c("Group_Performance","Group_Conflicts","Group_Positive_Atmosphere")])

## Group Ratings: ICCs
## 1)Aggregate Across tasks for each rater
## 2) calculate ICC

## Group Rating: Performance
Pils_DirectObs_Group_Single$Group_performance_R1 <- (Pils_DirectObs_Group_Single$taskD_performance_R1 + Pils_DirectObs_Group_Single$taskE_performance_R1 + Pils_DirectObs_Group_Single$taskF_performance_R1) / 3
Pils_DirectObs_Group_Single$Group_performance_R2 <- (Pils_DirectObs_Group_Single$taskD_performance_R2 + Pils_DirectObs_Group_Single$taskE_performance_R2 + Pils_DirectObs_Group_Single$taskF_performance_R2) / 3
Pils_DirectObs_Group_Single$Group_performance_R3 <- (Pils_DirectObs_Group_Single$taskD_performance_R3 + Pils_DirectObs_Group_Single$taskE_performance_R3 + Pils_DirectObs_Group_Single$taskF_performance_R3) / 3
Pils_DirectObs_Group_Single$Group_performance_R4 <- (Pils_DirectObs_Group_Single$taskD_performance_R4 + Pils_DirectObs_Group_Single$taskE_performance_R4 + Pils_DirectObs_Group_Single$taskF_performance_R4) / 3
Pils_DirectObs_Group_Single$Group_performance_R5 <- (Pils_DirectObs_Group_Single$taskD_performance_R5 + Pils_DirectObs_Group_Single$taskE_performance_R5 + Pils_DirectObs_Group_Single$taskF_performance_R5) / 3
Pils_DirectObs_Group_Single$Group_performance_R6 <- (Pils_DirectObs_Group_Single$taskD_performance_R6 + Pils_DirectObs_Group_Single$taskE_performance_R6 + Pils_DirectObs_Group_Single$taskF_performance_R6) / 3
ICC(na.omit(as.matrix(select(Pils_DirectObs_Group_Single, Group_performance_R1,  Group_performance_R2,Group_performance_R3,Group_performance_R4,Group_performance_R5,Group_performance_R6))))

## Group Rating: Conflicts
Pils_DirectObs_Group_Single$Group_conflicts_R1 <- (Pils_DirectObs_Group_Single$taskD_conflicts_R1 + Pils_DirectObs_Group_Single$taskE_conflicts_R1 + Pils_DirectObs_Group_Single$taskF_conflicts_R1) / 3
Pils_DirectObs_Group_Single$Group_conflicts_R2 <- (Pils_DirectObs_Group_Single$taskD_conflicts_R2 + Pils_DirectObs_Group_Single$taskE_conflicts_R2 + Pils_DirectObs_Group_Single$taskF_conflicts_R2) / 3
Pils_DirectObs_Group_Single$Group_conflicts_R3 <- (Pils_DirectObs_Group_Single$taskD_conflicts_R3 + Pils_DirectObs_Group_Single$taskE_conflicts_R3 + Pils_DirectObs_Group_Single$taskF_conflicts_R3) / 3
Pils_DirectObs_Group_Single$Group_conflicts_R4 <- (Pils_DirectObs_Group_Single$taskD_conflicts_R4 + Pils_DirectObs_Group_Single$taskE_conflicts_R4 + Pils_DirectObs_Group_Single$taskF_conflicts_R4) / 3
Pils_DirectObs_Group_Single$Group_conflicts_R5 <- (Pils_DirectObs_Group_Single$taskD_conflicts_R5 + Pils_DirectObs_Group_Single$taskE_conflicts_R5 + Pils_DirectObs_Group_Single$taskF_conflicts_R5) / 3
Pils_DirectObs_Group_Single$Group_conflicts_R6 <- (Pils_DirectObs_Group_Single$taskD_conflicts_R6 + Pils_DirectObs_Group_Single$taskE_conflicts_R6 + Pils_DirectObs_Group_Single$taskF_conflicts_R6) / 3
ICC(na.omit(as.matrix(select(Pils_DirectObs_Group_Single, Group_conflicts_R1,	Group_conflicts_R2,Group_conflicts_R3,Group_conflicts_R4,Group_conflicts_R5,Group_conflicts_R6))))

## Group Rating: Positive Atmosphere
Pils_DirectObs_Group_Single$Group_positive_atmosphere_R1 <- (Pils_DirectObs_Group_Single$taskD_positive_atmosphere_R1 + Pils_DirectObs_Group_Single$taskE_positive_atmosphere_R1 + Pils_DirectObs_Group_Single$taskF_positive_atmosphere_R1) / 3
Pils_DirectObs_Group_Single$Group_positive_atmosphere_R2 <- (Pils_DirectObs_Group_Single$taskD_positive_atmosphere_R2 + Pils_DirectObs_Group_Single$taskE_positive_atmosphere_R2 + Pils_DirectObs_Group_Single$taskF_positive_atmosphere_R2) / 3
Pils_DirectObs_Group_Single$Group_positive_atmosphere_R3 <- (Pils_DirectObs_Group_Single$taskD_positive_atmosphere_R3 + Pils_DirectObs_Group_Single$taskE_positive_atmosphere_R3 + Pils_DirectObs_Group_Single$taskF_positive_atmosphere_R3) / 3
Pils_DirectObs_Group_Single$Group_positive_atmosphere_R4 <- (Pils_DirectObs_Group_Single$taskD_positive_atmosphere_R4 + Pils_DirectObs_Group_Single$taskE_positive_atmosphere_R4 + Pils_DirectObs_Group_Single$taskF_positive_atmosphere_R4) / 3
Pils_DirectObs_Group_Single$Group_positive_atmosphere_R5 <- (Pils_DirectObs_Group_Single$taskD_positive_atmosphere_R5 + Pils_DirectObs_Group_Single$taskE_positive_atmosphere_R5 + Pils_DirectObs_Group_Single$taskF_positive_atmosphere_R5) / 3
Pils_DirectObs_Group_Single$Group_positive_atmosphere_R6 <- (Pils_DirectObs_Group_Single$taskD_positive_atmosphere_R6 + Pils_DirectObs_Group_Single$taskE_positive_atmosphere_R6 + Pils_DirectObs_Group_Single$taskF_positive_atmosphere_R6) / 3
ICC(na.omit(as.matrix(select(Pils_DirectObs_Group_Single, Group_positive_atmosphere_R1,	Group_positive_atmosphere_R2,Group_positive_atmosphere_R3,Group_positive_atmosphere_R4,Group_positive_atmosphere_R5,Group_positive_atmosphere_R6))))

#Table 12 additional analyses#

#Correlations Traits and Behavior
#prepare data
Pils_DirectObs_Agg <- as.data.frame(Pils_DirectObs_Agg)
Pils_DirectObs_Agg$Expressiveness <- rowMeans(Pils_DirectObs_Agg[,c("taskB_expressive_behavior", "taskC_expressive_behavior", "taskD_expressive_behavior", "taskE_expressive_behavior", "taskF_expressive_behavior", "taskG_expressive_behavior")],na.rm=TRUE)
Pils_DirectObs_Agg$Dominant <- rowMeans(Pils_DirectObs_Agg[,c("taskB_dominant_behavior", "taskC_dominant_behavior", "taskD_dominant_behavior", "taskE_dominant_behavior", "taskF_dominant_behavior", "taskG_dominant_behavior")],na.rm=TRUE)
Pils_DirectObs_Agg$Arrogant <- rowMeans(Pils_DirectObs_Agg[,c("taskB_arrogant_behavior", "taskC_arrogant_behavior", "taskD_arrogant_behavior", "taskE_arrogant_behavior", "taskF_arrogant_behavior", "taskG_arrogant_behavior")],na.rm=TRUE)
Pils_DirectObs_Agg$Friendly <- rowMeans(Pils_DirectObs_Agg[,c("taskB_friendly_behavior", "taskC_friendly_behavior")],na.rm=TRUE)
Pils_DirectObs_Agg$Nervousness <- rowMeans(Pils_DirectObs_Agg[,c("taskB_nervous_behavior", "taskC_nervous_behavior")],na.rm=TRUE)
Pils_DirectObs_Agg$Cooperative <- rowMeans(Pils_DirectObs_Agg[,c("taskD_cooperative_behavior", "taskE_cooperative_behavior", "taskF_cooperative_behavior", "taskG_cooperative_behavior")],na.rm=TRUE)
Pils_DirectObs_Agg$Aggressive <- rowMeans(Pils_DirectObs_Agg[,c("taskD_aggressive_behavior", "taskE_aggressive_behavior", "taskF_aggressive_behavior", "taskG_aggressive_behavior")],na.rm=TRUE)
Pils_DirectObs_Agg$StateEx <- rowMeans(Pils_DirectObs_Agg[,c("Expressiveness","Dominant")],na.rm=TRUE)
Pils_DirectObs_Agg$StateAg <- rowMeans(Pils_DirectObs_Agg[,c("Friendly","Cooperative")],na.rm=TRUE)
Pils_DirectObs_Agg$StateDido <- rowMeans(Pils_DirectObs_Agg[,c("Aggressive","Arrogant")],na.rm=TRUE)
Pils_Traits$Dido <- rowMeans(Pils_Traits[,c("dido_narc","dido_mach","dido_psych")],na.rm=TRUE)
Pils_Traits_Acq_Agg$Dido_acqu <- rowMeans(Pils_Traits_Acq_Agg[,c("dido_narc_acqu","dido_mach_acqu","dido_psych_acqu")],na.rm=TRUE)
CorrelationBehavior <- merge(select(Pils_DirectObs_Agg,id_a,StateEx, StateAg, StateDido,Nervousness), select(Pils_Traits, id_a, big5_n, big5_e, big5_a, narq_riv, Dido ), by = "id_a")
CorrelationBehavior_acq <- merge(select(Pils_DirectObs_Agg,id_a, StateEx, StateAg, StateDido, Nervousness), select(Pils_Traits_Acq_Agg, id_a, big5_n_acqu, big5_e_acqu, big5_a_acqu, narq_riv_acqu, Dido_acqu), by = "id_a")

#correlations
#self-report and behavior
corr.test(CorrelationBehavior,adjust ="none")
#acqu-report and behavior
corr.test(CorrelationBehavior_acq,adjust ="none")


############################################################################################
## 3 Preparation for CONNECT                                                              ##
############################################################################################

### a. set working directory
setwd("B:/AEBack/02_Forschung/01_PaperinPreparation/Methodenpaper/Data_Connect/ohneoderrandomID") #please add your own working directory here

### b. read relevant datasets CONNECT
Connect_Zero_long <- read_sav("01_connect_zeroacquaintanceexperiment_long.sav",user_na = FALSE)
Connect_Zero_effects <- read_sav("02_connect_zeroacquaintanceexperiment_effects.sav",user_na = FALSE)
Connect_Traits_t1 <- read_sav("03_connect_onlinesurvey_self_t1.sav",user_na = FALSE)
Connect_Traits_t2 <- read_sav("04_connect_onlinesurvey_self_t2.sav",user_na = FALSE)
Connect_Traits_t3 <- read_sav("05_connect_onlinesurvey_self_t3.sav",user_na = FALSE)
Connect_Traits_t4 <- read_sav("06_connect_onlinesurvey_self_t4.sav",user_na = FALSE)
Connect_Traits_t5 <- read_sav("07_connect_onlinesurvey_self_t5.sav",user_na = FALSE)
Connect_Traits_Acq_Agg <- read_sav("08_connect_onlinesurvey_t1_acquaintances_aggregated.sav",user_na = FALSE)
Connect_Traits_Acq_Single <- read_sav("09_connect_onlinesurvey_t1_acquaintances_single.sav",user_na = FALSE)
Connect_Diary_Long<- read_sav("10_connect_timebasedassessment_long.sav",user_na = FALSE)
Connect_App_Unique<- read_sav("12_connect_eventbasedassessment_uniqueinteractions.sav",user_na = FALSE)
Connect_App_Long<- read_sav("13_connect_eventbasedassessment_long.sav",user_na = FALSE)
Connect_Cognitive <- read_sav("16_connect_directobservations_cognitiveabilities.sav",user_na = FALSE)
Connect_DirectObs_Agg <- read_sav("17_connect_directobservations_appearancebehavior_aggregated.sav",user_na = FALSE)
Connect_DirectObs_Single <- read_sav("18_connect_directobservations_appearancebehavior_single.sav",user_na = FALSE)
Connect_Economic <- read_sav("19_connect_directobservations_economictasks.sav",user_na = FALSE)

############################################################################################
## 4 Descriptive Results CONNECT                                                          ##
############################################################################################

############################################################################################
## E. TABLE 11                                                                            ##
############################################################################################

## de-select late starters (late starters did not take part in the zero acquaintance experiment)
Connect_Zero_effects_2 <- Connect_Zero_effects[!is.na(Connect_Zero_effects$SR_good_bad_mood_zero_pre),]

## State Affect: Pre (M, SD)
describe(Connect_Zero_effects_2[,c("SR_good_bad_mood_zero_pre","SR_bored_activated_zero_pre","SR_dis_satisfiedwmyself_zero_pre","SR_nervous_relaxed_zero_pre","SR_inhibited_determined_zero_pre")])

## State Affect: Post (M, SD)
describe(Connect_Zero_effects_2[,c("SR_good_bad_mood_zero_post","SR_bored_activated_zero_post","SR_dis_satisfiedwmyself_zero_post","SR_nervous_relaxed_zero_post","SR_inhibited_determined_zero_post")])

#t.tests and effects
t.test(Connect_Zero_effects_2$SR_good_bad_mood_zero_pre, Connect_Zero_effects_2$SR_good_bad_mood_zero_post, paired=TRUE )
cohen.d (as.numeric(Connect_Zero_effects_2$SR_good_bad_mood_zero_pre), as.numeric(Connect_Zero_effects_2$SR_good_bad_mood_zero_post),paired=T, na.rm = T)
t.test(Connect_Zero_effects_2$SR_bored_activated_zero_pre, Connect_Zero_effects_2$SR_bored_activated_zero_post, paired=TRUE )
cohen.d (as.numeric(Connect_Zero_effects_2$SR_bored_activated_zero_pre), as.numeric(Connect_Zero_effects_2$SR_bored_activated_zero_post),paired=T, na.rm = T)
t.test(Connect_Zero_effects_2$SR_dis_satisfiedwmyself_zero_pre, Connect_Zero_effects_2$SR_dis_satisfiedwmyself_zero_post, paired=TRUE )
cohen.d (as.numeric(Connect_Zero_effects_2$SR_dis_satisfiedwmyself_zero_pre), as.numeric(Connect_Zero_effects_2$SR_dis_satisfiedwmyself_zero_post),paired=T, na.rm = T)
t.test(Connect_Zero_effects_2$SR_nervous_relaxed_zero_pre, Connect_Zero_effects_2$SR_nervous_relaxed_zero_post, paired=TRUE )
cohen.d (as.numeric(Connect_Zero_effects_2$SR_nervous_relaxed_zero_pre), as.numeric(Connect_Zero_effects_2$SR_nervous_relaxed_zero_post),paired=T, na.rm = T)
t.test(Connect_Zero_effects_2$SR_inhibited_determined_zero_pre, Connect_Zero_effects_2$SR_inhibited_determined_zero_post, paired=TRUE )
cohen.d (as.numeric(Connect_Zero_effects_2$SR_inhibited_determined_zero_pre), as.numeric(Connect_Zero_effects_2$SR_inhibited_determined_zero_post),paired=T, na.rm = T)


############################################################################################
## F. TABLE 12                                                                            ##
############################################################################################

## make sure to delete unintended self-ratings
Connect_Zero_long2 <- as.data.frame(subset(Connect_Zero_long, Connect_Zero_long$id_a != Connect_Zero_long$id_p))

## means and variances
summary(lmer(knowing_zero ~ 1 + (1|id_a), data = Connect_Zero_long2))
RR(knowing_zero~id_a*id_p, data = Connect_Zero_long2 , na.rm=TRUE)

summary(lmer(liking_zero ~ 1 + (1|id_a), data = Connect_Zero_long2))
RR(liking_zero~id_a*id_p, data = Connect_Zero_long2 , na.rm=TRUE)

summary(lmer(metaliking_zero ~ 1 + (1|id_a), data = Connect_Zero_long2))
RR(metaliking_zero~id_a*id_p, data = Connect_Zero_long2 , na.rm=TRUE)

summary(lmer(dominant_zero ~ 1 + (1|id_a), data = Connect_Zero_long2))
RR(dominant_zero~id_a*id_p, data = Connect_Zero_long2 , na.rm=TRUE)

summary(lmer(affectionate_zero ~ 1 + (1|id_a), data = Connect_Zero_long2))
RR(affectionate_zero~id_a*id_p, data = Connect_Zero_long2 , na.rm=TRUE)


############################################################################################
## G. TABLE 13                                                                            ##
############################################################################################

## SELF-REPORT T1: M & SD
describe(Connect_Traits_t1$age_t1)
table (Connect_Traits_t1$sex_t1)
describe(Connect_Traits_t1[,c("big5_n_t1","big5_e_t1","big5_o_t1","big5_a_t1","big5_c_t1","rses_t1","saq_t1","npi_t1","narq_adm_t1","narq_riv_t1","dido_mach_t1","dido_psych_t1","dido_narc_t1")])
describe(Connect_Cognitive[,c("wmc_solved","wmc_span","mwtb_total","raven_total")])

## SELF-REPORT T1: alpha
psych::alpha(as.data.frame(select(Connect_Traits_t1, big5_5_t1,big5_10_t1,big5_15_t1_r))) #N
psych::alpha(as.data.frame(select(Connect_Traits_t1, big5_2_t1,big5_8_t1,big5_12_t1_r))) #E
psych::alpha(as.data.frame(select(Connect_Traits_t1, big5_4_t1,big5_9_t1,big5_14_t1))) #0
psych::alpha(as.data.frame(select(Connect_Traits_t1, big5_3_t1_r,big5_6_t1,big5_13_t1,big5_16_t1,big5_17_t1_r))) #A
psych::alpha(as.data.frame(select(Connect_Traits_t1, big5_1_t1,big5_7_t1_r,big5_11_t1))) #C
psych::alpha(as.data.frame(select(Connect_Traits_t1, rses_1_t1, rses_2_t1_r, rses_3_t1, rses_4_t1, rses_5_t1_r, rses_6_t1_r, rses_7_t1, rses_8_t1_r, rses_9_t1_r, rses_10_t1))) #RSES
psych::alpha(as.data.frame(select(Connect_Traits_t1, saq_1_t1, saq_2_t1, saq_3_t1, saq_4_t1, saq_5_t1, saq_6_t1, saq_7_t1, saq_8_t1, saq_9_t1, saq_attr_t1))) #SAQ
psych::alpha(as.data.frame(select(Connect_Traits_t1, npi_1_t1, npi_2_t1, npi_3_t1, npi_4_t1_r, npi_5_t1_r, npi_6_t1, npi_7_t1_r, npi_8_t1, npi_9_t1_r, npi_10_t1_r,npi_11_t1, npi_12_t1, npi_13_t1, npi_14_t1, npi_15_t1_r, npi_16_t1, npi_17_t1_r, npi_18_t1_r, npi_19_t1_r, npi_20_t1_r, npi_21_t1, npi_22_t1_r, npi_23_t1_r, npi_24_t1, npi_25_t1, npi_26_t1_r, npi_27_t1, npi_28_t1_r, npi_29_t1, npi_30_t1, npi_31_t1, npi_32_t1_r, npi_33_t1, npi_34_t1, npi_35_t1_r, npi_36_t1, npi_37_t1, npi_38_t1, npi_39_t1, npi_40_t1_r))) #NPI
psych::alpha(as.data.frame(select(Connect_Traits_t1, narq_1_t1, narq_2_t1, narq_3_t1, narq_5_t1, narq_7_t1, narq_8_t1, narq_15_t1, narq_16_t1, narq_18_t1))) #NARQ-Adm
psych::alpha(as.data.frame(select(Connect_Traits_t1, narq_4_t1, narq_6_t1, narq_9_t1, narq_10_t1, narq_11_t1, narq_12_t1, narq_13_t1, narq_14_t1, narq_17_t1))) #NARQ-Riv
psych::alpha(as.data.frame(select(Connect_Traits_t1, dido_1_t1, dido_4_t1, dido_7_t1, dido_10_t1))) #DIDO-Mach
psych::alpha(as.data.frame(select(Connect_Traits_t1, dido_2_t1, dido_5_t1, dido_8_t1, dido_11_t1))) #DIDO-Psych
psych::alpha(as.data.frame(select(Connect_Traits_t1, dido_3_t1, dido_6_t1, dido_9_t1, dido_12_t1))) #DIDO-Narc
## some Items excluded from analyses: Likely variables with missing values are  wmc_8_2 wmc_5_7 wmc_6_4 wmc_6_8 wmc_6_9  Error in principal(x, scores = FALSE) : I am sorry: missing values (NAs) in the correlation matrix do not allow me to continue. Please drop those variables and try again
psych::alpha(as.data.frame(select(Connect_Cognitive, wmc_4_11, wmc_4_12, wmc_4_13, wmc_4_14, wmc_4_16, wmc_4_17, wmc_4_18, wmc_4_19, wmc_5_1, wmc_5_2, wmc_5_3, wmc_5_4,wmc_5_5,wmc_5_8, wmc_5_9, wmc_5_10, wmc_5_11, wmc_6_1, wmc_6_2, wmc_6_3,wmc_6_5,wmc_6_6, wmc_6_10,wmc_6_11, wmc_6_12,wmc_6_13,wmc_7_1, wmc_7_2, wmc_7_3,wmc_7_4,wmc_7_5, wmc_7_6,wmc_7_7, wmc_7_9,wmc_7_10, wmc_7_11,wmc_7_12,wmc_7_13,wmc_7_14,wmc_7_15,wmc_8_1,wmc_8_3,wmc_8_4,wmc_8_5,wmc_8_6,wmc_8_7,wmc_8_8,wmc_8_10,wmc_8_11,wmc_8_12,wmc_8_13,wmc_8_14,wmc_8_15,wmc_8_16,wmc_8_17))) ## WMC - solved equations
psych::alpha(as.data.frame(select(Connect_Cognitive, wmc_4_15, wmc_4_20, wmc_5_6, wmc_5_12, wmc_6_7, wmc_6_14, wmc_7_8, wmc_7_16, wmc_8_9, wmc_8_18))) #WMC - memory span
psych::alpha(as.data.frame(select(Connect_Cognitive, mwtb1, mwtb2, mwtb3, mwtb4, mwtb5,mwtb6,mwtb7,mwtb8,mwtb9,mwtb10,mwtb11,mwtb12,mwtb13,mwtb14,mwtb15,mwtb16,mwtb17,mwtb18,mwtb19,mwtb20,mwtb21,mwtb22,mwtb23,mwtb24,mwtb25,mwtb26,mwtb27,mwtb28,mwtb29,mwtb30,mwtb31,mwtb32,mwtb33,mwtb34,mwtb35,mwtb37))) #MWTB
psych::alpha(as.data.frame(select(Connect_Cognitive, raven1, raven2, raven3, raven4, raven5,raven6,raven7,raven8,raven9,raven10,raven11,raven12,raven13,raven14,raven15))) #raven

## INFORMANT-REPORT T1: M & SD
describe(Connect_Traits_Acq_Single$age_acqu)
table(Connect_Traits_Acq_Single$sex_acqu)
describe(as.data.frame(Connect_Traits_Acq_Agg[,c("big5_n_acqu","big5_e_acqu","big5_o_acqu","big5_a_acqu","big5_c_acqu","rses_acqu","saq_acqu","npi_short_acqu","narq_adm_acqu","narq_riv_acqu","dido_mach_acqu","dido_psych_acqu","dido_narc_acqu")]))

## INFORMANT-REPORT T1: alpha

psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, big5_5_acqu,big5_10_acqu,big5_15_acqu_r))) #N
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, big5_2_acqu,big5_8_acqu,big5_12_acqu_r))) #E
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, big5_4_acqu,big5_9_acqu,big5_14_acqu))) #O
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, big5_3_acqu_r,big5_6_acqu,big5_13_acqu,big5_16_acqu,big5_17_acqu_r))) #A
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, big5_1_acqu,big5_7_acqu_r,big5_11_acqu))) #C
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, rses_1_acqu, rses_2_acqu_r, rses_3_acqu, rses_4_acqu, rses_5_acqu_r, rses_6_acqu_r, rses_7_acqu, rses_8_acqu_r, rses_9_acqu_r, rses_10_acqu))) #RSES
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, saq_1_acqu, saq_2_acqu, saq_3_acqu, saq_4_acqu, saq_5_acqu, saq_6_acqu, saq_7_acqu, saq_8_acqu, saq_9_acqu, saq_attr_acqu))) # SAQ
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, npi_1_acqu,npi_4_acqu_r,npi_7_acqu_r,npi_9_acqu_r, npi_10_acqu_r,
npi_12_acqu,npi_13_acqu,npi_18_acqu_r,npi_27_acqu,npi_30_acqu,npi_32_acqu_r,npi_33_acqu,npi_34_acqu,npi_36_acqu,npi_40_acqu_r))) # NPI
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, narq_1_acqu, narq_2_acqu, narq_3_acqu, narq_5_acqu, narq_7_acqu, narq_8_acqu, narq_15_acqu, narq_16_acqu, narq_18_acqu))) #NARQ-Adm
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, narq_4_acqu, narq_6_acqu, narq_9_acqu, narq_10_acqu, narq_11_acqu, narq_12_acqu, narq_13_acqu, narq_14_acqu, narq_17_acqu))) #NARQ-Riv
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, dido_1_acqu, dido_4_acqu, dido_7_acqu, dido_10_acqu))) #DIDO-Mach
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, dido_2_acqu, dido_5_acqu, dido_8_acqu, dido_11_acqu))) #DIDO-Psych
psych::alpha(as.data.frame(select(Connect_Traits_Acq_Agg, dido_3_acqu, dido_6_acqu, dido_9_acqu, dido_12_acqu))) #DIDO-Narc

## SELF-REPORT T2: M & SD
describe(Connect_Traits_t2$age_t2)
table (Connect_Traits_t2$sex_t2)
describe(Connect_Traits_t2[,c("big5_n_t2","big5_e_t2","big5_o_t2","big5_a_t2","big5_c_t2","rses_t2","saq_t2","npi_t2","narq_adm_t2","narq_riv_t2","dido_mach_t2","dido_psych_t2","dido_narc_t2")])

## SELF-REPORT T2: alpha
psych::alpha(as.data.frame(select(Connect_Traits_t2, big5_5_t2,big5_10_t2,big5_15_t2_r))) #N
psych::alpha(as.data.frame(select(Connect_Traits_t2, big5_2_t2,big5_8_t2,big5_12_t2_r))) #E
psych::alpha(as.data.frame(select(Connect_Traits_t2, big5_4_t2,big5_9_t2,big5_14_t2))) #O
psych::alpha(as.data.frame(select(Connect_Traits_t2, big5_3_t2_r,big5_6_t2,big5_13_t2,big5_16_t2,big5_17_t2_r))) #A
psych::alpha(as.data.frame(select(Connect_Traits_t2, big5_1_t2,big5_7_t2_r,big5_11_t2))) #C
psych::alpha(as.data.frame(select(Connect_Traits_t2, rses_1_t2, rses_2_t2_r, rses_3_t2, rses_4_t2, rses_5_t2_r, rses_6_t2_r, rses_7_t2, rses_8_t2_r, rses_9_t2_r, rses_10_t2))) #RSES
psych::alpha(as.data.frame(select(Connect_Traits_t2, saq_1_t2, saq_2_t2, saq_3_t2, saq_4_t2, saq_5_t2, saq_6_t2, saq_7_t2, saq_8_t2, saq_9_t2, saq_attr_t2))) #SAQ
psych::alpha(as.data.frame(select(Connect_Traits_t2, npi_1_t2, npi_2_t2, npi_3_t2, npi_4_t2_r, npi_5_t2_r, npi_6_t2, npi_7_t2_r, npi_8_t2, npi_9_t2_r, npi_10_t2_r, npi_11_t2, npi_12_t2, npi_13_t2, npi_14_t2, npi_15_t2_r, npi_16_t2, npi_17_t2_r, npi_18_t2_r, npi_19_t2_r, npi_20_t2_r, npi_21_t2, npi_22_t2_r, npi_23_t2_r, npi_24_t2, npi_25_t2, npi_26_t2_r, npi_27_t2, npi_28_t2_r, npi_29_t2, npi_30_t2, npi_31_t2, npi_32_t2_r, npi_33_t2, npi_34_t2, npi_35_t2_r, npi_36_t2, npi_37_t2, npi_38_t2, npi_39_t2, npi_40_t2_r))) #NPI
psych::alpha(as.data.frame(select(Connect_Traits_t2, narq_1_t2, narq_2_t2, narq_3_t2, narq_5_t2, narq_7_t2, narq_8_t2, narq_15_t2, narq_16_t2, narq_18_t2))) #NARQ-Adm
psych::alpha(as.data.frame(select(Connect_Traits_t2, narq_4_t2, narq_6_t2, narq_9_t2, narq_10_t2, narq_11_t2, narq_12_t2, narq_13_t2, narq_14_t2, narq_17_t2))) #NARQ-Riv
psych::alpha(as.data.frame(select(Connect_Traits_t2, dido_1_t2, dido_4_t2, dido_7_t2, dido_10_t2))) #DIDO-Mach
psych::alpha(as.data.frame(select(Connect_Traits_t2, dido_2_t2, dido_5_t2, dido_8_t2, dido_11_t2))) #DIDO-Psych
psych::alpha(as.data.frame(select(Connect_Traits_t2, dido_3_t2, dido_6_t2, dido_9_t2, dido_12_t2))) #DIDO-Narc

## SELF-REPORT T3: M & SD
describe(Connect_Traits_t3$age_t3)
table (Connect_Traits_t3$sex_t3)
describe(Connect_Traits_t3[,c("big5_n_t3","big5_e_t3","big5_o_t3","big5_a_t3","big5_c_t3","rses_t3","saq_t3","npi_t3","narq_adm_t3","narq_riv_t3","dido_mach_t3","dido_psych_t3","dido_narc_t3")])

## SELF-REPORT T3: alpha
psych::alpha(as.data.frame(select(Connect_Traits_t3, big5_5_t3,big5_10_t3,big5_15_t3_r))) #N
psych::alpha(as.data.frame(select(Connect_Traits_t3, big5_2_t3,big5_8_t3,big5_12_t3_r))) #E
psych::alpha(as.data.frame(select(Connect_Traits_t3, big5_4_t3,big5_9_t3,big5_14_t3))) #0
psych::alpha(as.data.frame(select(Connect_Traits_t3, big5_3_t3_r,big5_6_t3,big5_13_t3,big5_16_t3,big5_17_t3_r))) #A
psych::alpha(as.data.frame(select(Connect_Traits_t3, big5_1_t3,big5_7_t3_r,big5_11_t3))) #C
psych::alpha(as.data.frame(select(Connect_Traits_t3, rses_1_t3, rses_2_t3_r, rses_3_t3, rses_4_t3, rses_5_t3_r, rses_6_t3_r, rses_7_t3, rses_8_t3_r, rses_9_t3_r, rses_10_t3))) #RSES
psych::alpha(as.data.frame(select(Connect_Traits_t3, saq_1_t3, saq_2_t3, saq_3_t3, saq_4_t3, saq_5_t3, saq_6_t3, saq_7_t3, saq_8_t3, saq_9_t3, saq_attr_t3))) #SAQ
psych::alpha(as.data.frame(select(Connect_Traits_t3, npi_1_t3, npi_2_t3, npi_3_t3, npi_4_t3_r, npi_5_t3_r, npi_6_t3, npi_7_t3_r, npi_8_t3, npi_9_t3_r, npi_10_t3_r,npi_11_t3, npi_12_t3, npi_13_t3, npi_14_t3, npi_15_t3_r, npi_16_t3, npi_17_t3_r, npi_18_t3_r, npi_19_t3_r, npi_20_t3_r, npi_21_t3,npi_22_t3_r, npi_23_t3_r, npi_24_t3, npi_25_t3, npi_26_t3_r, npi_27_t3, npi_28_t3_r, npi_29_t3, npi_30_t3, npi_31_t3, npi_32_t3_r,npi_33_t3, npi_34_t3, npi_35_t3_r, npi_36_t3, npi_37_t3, npi_38_t3, npi_39_t3, npi_40_t3_r))) #NPI
psych::alpha(as.data.frame(select(Connect_Traits_t3, narq_1_t3, narq_2_t3, narq_3_t3, narq_5_t3, narq_7_t3, narq_8_t3, narq_15_t3, narq_16_t3, narq_18_t3))) #NARQ-Adm
psych::alpha(as.data.frame(select(Connect_Traits_t3, narq_4_t3, narq_6_t3, narq_9_t3, narq_10_t3, narq_11_t3, narq_12_t3, narq_13_t3, narq_14_t3, narq_17_t3))) #NARQ-Riv
psych::alpha(as.data.frame(select(Connect_Traits_t3, dido_1_t3, dido_4_t3, dido_7_t3, dido_10_t3))) #DIDO-Mach
psych::alpha(as.data.frame(select(Connect_Traits_t3, dido_2_t3, dido_5_t3, dido_8_t3, dido_11_t3))) #DIDO-Psych
psych::alpha(as.data.frame(select(Connect_Traits_t3, dido_3_t3, dido_6_t3, dido_9_t3, dido_12_t3))) #DIDO-Narc

## SELF-REPORT T4: M & SD
describe(Connect_Traits_t4$age_t4)
table (Connect_Traits_t4$sex_t4)
describe(Connect_Traits_t4[,c("big5_n_t4","big5_e_t4","big5_o_t4","big5_a_t4","big5_c_t4","rses_t4","saq_t4","npi_t4","narq_adm_t4","narq_riv_t4","dido_mach_t4","dido_psych_t4","dido_narc_t4")])

## SELF-REPORT T4: alphas
psych::alpha(as.data.frame(select(Connect_Traits_t4, big5_5_t4,big5_10_t4,big5_15_t4_r))) #N
psych::alpha(as.data.frame(select(Connect_Traits_t4, big5_2_t4,big5_8_t4,big5_12_t4_r))) #E
psych::alpha(as.data.frame(select(Connect_Traits_t4, big5_4_t4,big5_9_t4,big5_14_t4))) #0
psych::alpha(as.data.frame(select(Connect_Traits_t4, big5_3_t4_r,big5_6_t4,big5_13_t4,big5_16_t4,big5_17_t4_r))) #A
psych::alpha(as.data.frame(select(Connect_Traits_t4, big5_1_t4,big5_7_t4_r,big5_11_t4))) #C
psych::alpha(as.data.frame(select(Connect_Traits_t4, rses_1_t4, rses_2_t4_r, rses_3_t4, rses_4_t4, rses_5_t4_r, rses_6_t4_r, rses_7_t4, rses_8_t4_r, rses_9_t4_r, rses_10_t4))) #RSES
psych::alpha(as.data.frame(select(Connect_Traits_t4, saq_1_t4, saq_2_t4, saq_3_t4, saq_4_t4, saq_5_t4, saq_6_t4, saq_7_t4, saq_8_t4, saq_9_t4, saq_attr_t4))) #SAQ
psych::alpha(as.data.frame(select(Connect_Traits_t4, npi_1_t4, npi_2_t4, npi_3_t4, npi_4_t4_r, npi_5_t4_r, npi_6_t4, npi_7_t4_r, npi_8_t4, npi_9_t4_r, npi_10_t4_r,npi_11_t4, npi_12_t4, npi_13_t4, npi_14_t4, npi_15_t4_r, npi_16_t4, npi_17_t4_r, npi_18_t4_r, npi_19_t4_r, npi_20_t4_r, npi_21_t4,npi_22_t4_r, npi_23_t4_r, npi_24_t4, npi_25_t4, npi_26_t4_r, npi_27_t4, npi_28_t4_r, npi_29_t4, npi_30_t4, npi_31_t4, npi_32_t4_r,npi_33_t4, npi_34_t4, npi_35_t4_r, npi_36_t4, npi_37_t4, npi_38_t4, npi_39_t4, npi_40_t4_r))) #NPI
psych::alpha(as.data.frame(select(Connect_Traits_t4, narq_1_t4, narq_2_t4, narq_3_t4, narq_5_t4, narq_7_t4, narq_8_t4, narq_15_t4, narq_16_t4, narq_18_t4))) #NARQ-Adm
psych::alpha(as.data.frame(select(Connect_Traits_t4, narq_4_t4, narq_6_t4, narq_9_t4, narq_10_t4, narq_11_t4, narq_12_t4, narq_13_t4, narq_14_t4, narq_17_t4))) #NARQ-Riv
psych::alpha(as.data.frame(select(Connect_Traits_t4, dido_1_t4, dido_4_t4, dido_7_t4, dido_10_t4))) #DIDO-Mach
psych::alpha(as.data.frame(select(Connect_Traits_t4, dido_2_t4, dido_5_t4, dido_8_t4, dido_11_t4))) #DIDO-Psych
psych::alpha(as.data.frame(select(Connect_Traits_t4, dido_3_t4, dido_6_t4, dido_9_t4, dido_12_t4))) #DIDO-Narc

## SELF-REPORT T5: M & SD
describe(Connect_Traits_t5$age_t5)
table (Connect_Traits_t5$sex_t5)
describe(Connect_Traits_t5[,c("big5_n_t5","big5_e_t5","big5_o_t5","big5_a_t5","big5_c_t5","rses_t5","saq_t5","npi_t5","narq_adm_t5","narq_riv_t5","dido_mach_t5","dido_psych_t5","dido_narc_t5")])

## SELF-REPORT T5: alpha
psych::alpha(as.data.frame(select(Connect_Traits_t5, big5_5_t5,big5_10_t5,big5_15_t5_r))) #N
psych::alpha(as.data.frame(select(Connect_Traits_t5, big5_2_t5,big5_8_t5,big5_12_t5_r))) #E
psych::alpha(as.data.frame(select(Connect_Traits_t5, big5_4_t5,big5_9_t5,big5_14_t5))) #0
psych::alpha(as.data.frame(select(Connect_Traits_t5, big5_3_t5_r,big5_6_t5,big5_13_t5,big5_16_t5,big5_17_t5_r))) #A
psych::alpha(as.data.frame(select(Connect_Traits_t5, big5_1_t5,big5_7_t5_r,big5_11_t5))) #C
psych::alpha(as.data.frame(select(Connect_Traits_t5, rses_1_t5, rses_2_t5_r, rses_3_t5, rses_4_t5, rses_5_t5_r, rses_6_t5_r, rses_7_t5, rses_8_t5_r, rses_9_t5_r, rses_10_t5))) #RSES
psych::alpha(as.data.frame(select(Connect_Traits_t5, saq_1_t5, saq_2_t5, saq_3_t5, saq_4_t5, saq_5_t5, saq_6_t5, saq_7_t5, saq_8_t5, saq_9_t5, saq_attr_t5))) #SAQ
psych::alpha(as.data.frame(select(Connect_Traits_t5, npi_1_t5, npi_2_t5, npi_3_t5, npi_4_t5_r, npi_5_t5_r, npi_6_t5, npi_7_t5_r, npi_8_t5, npi_9_t5_r, npi_10_t5_r,npi_11_t5, npi_12_t5, npi_13_t5, npi_14_t5, npi_15_t5_r, npi_16_t5, npi_17_t5_r, npi_18_t5_r, npi_19_t5_r, npi_20_t5_r,npi_21_t5,npi_22_t5_r, npi_23_t5_r, npi_24_t5, npi_25_t5, npi_26_t5_r, npi_27_t5, npi_28_t5_r, npi_29_t5, npi_30_t5, npi_31_t5, npi_32_t5_r,npi_33_t5, npi_34_t5, npi_35_t5_r, npi_36_t5, npi_37_t5, npi_38_t5, npi_39_t5, npi_40_t5_r))) #NPI
psych::alpha(as.data.frame(select(Connect_Traits_t5, narq_1_t5, narq_2_t5, narq_3_t5, narq_5_t5, narq_7_t5, narq_8_t5, narq_15_t5, narq_16_t5, narq_18_t5))) #NARQ-Adm
psych::alpha(as.data.frame(select(Connect_Traits_t5, narq_4_t5, narq_6_t5, narq_9_t5, narq_10_t5, narq_11_t5, narq_12_t5, narq_13_t5, narq_14_t5, narq_17_t5))) #NARQ-Riv
psych::alpha(as.data.frame(select(Connect_Traits_t5, dido_1_t5, dido_4_t5, dido_7_t5, dido_10_t5))) #DIDO-Mach
psych::alpha(as.data.frame(select(Connect_Traits_t5, dido_2_t5, dido_5_t5, dido_8_t5, dido_11_t5))) #DIDO-Psych
psych::alpha(as.data.frame(select(Connect_Traits_t5, dido_3_t5, dido_6_t5, dido_9_t5, dido_12_t5))) #DIDO-Narc


############################################################################################
## H. TABLE 14                                                                            ##
############################################################################################

## Correlations: Self-reports T1
CorrelationSelfT1 <- merge (as.data.frame(select(Connect_Traits_t1, id_a, big5_n_t1, big5_e_t1, big5_o_t1, big5_a_t1, big5_c_t1, rses_t1, saq_t1, npi_t1, narq_adm_t1, narq_riv_t1, dido_mach_t1, dido_psych_t1, dido_narc_t1)),as.data.frame(select(Connect_Cognitive, id_a, wmc_solved, wmc_span, mwtb_total, raven_total)), by ="id_a" )

## Correlations T1
SelfvaluesT1 <- round (corr.test(CorrelationSelfT1[c("big5_n_t1","big5_e_t1","big5_o_t1","big5_a_t1","big5_c_t1","rses_t1","saq_t1","npi_t1","narq_adm_t1","narq_riv_t1","dido_mach_t1","dido_psych_t1","dido_narc_t1","wmc_solved","wmc_span","mwtb_total","raven_total")],adjust ="none")[[1]], digits =2)
SelfvaluesT1

## Ns of correlations T1
round(corr.test(CorrelationSelfT1[c("big5_n_t1","big5_e_t1","big5_o_t1","big5_a_t1","big5_c_t1","rses_t1","saq_t1","npi_t1","narq_adm_t1","narq_riv_t1","dido_mach_t1","dido_psych_t1","dido_narc_t1","wmc_solved","wmc_span","mwtb_total","raven_total")],adjust ="none")[[2]])

## Significance (p-values) of correlations T!
SelfSignificanceT1 <- round (corr.test(CorrelationSelfT1[c("big5_n_t1","big5_e_t1","big5_o_t1","big5_a_t1","big5_c_t1","rses_t1","saq_t1","npi_t1","narq_adm_t1","narq_riv_t1","dido_mach_t1","dido_psych_t1","dido_narc_t1","wmc_solved","wmc_span","mwtb_total","raven_total")],adjust ="none")[[4]], digits =3)
SelfSignificanceT1

## Correlations Self-reports T5
CorrelationSelfT5 <- merge (as.data.frame(select(Connect_Traits_t5, id_a, big5_n_t5, big5_e_t5, big5_o_t5, big5_a_t5, big5_c_t5, rses_t5, saq_t5, npi_t5, narq_adm_t5, narq_riv_t5, dido_mach_t5, dido_psych_t5, dido_narc_t5)),as.data.frame(select(Connect_Cognitive, id_a, wmc_solved, wmc_span, mwtb_total, raven_total)), by ="id_a" )

## Correlations T5
SelfvaluesT5 <- round (corr.test(CorrelationSelfT5[c("big5_n_t5","big5_e_t5","big5_o_t5","big5_a_t5","big5_c_t5","rses_t5","saq_t5","npi_t5","narq_adm_t5","narq_riv_t5","dido_mach_t5","dido_psych_t5","dido_narc_t5","wmc_solved","wmc_span","mwtb_total","raven_total")],adjust ="none")[[1]], digits =2)
SelfvaluesT5

## Ns of correlations T5
round(corr.test(CorrelationSelfT5[c("big5_n_t5","big5_e_t5","big5_o_t5","big5_a_t5","big5_c_t5","rses_t5","saq_t5","npi_t5","narq_adm_t5","narq_riv_t5","dido_mach_t5","dido_psych_t5","dido_narc_t5","wmc_solved","wmc_span","mwtb_total","raven_total")],adjust ="none")[[2]])

## Significances of correlations T5
SelfSignificanceT5 <- round (corr.test(CorrelationSelfT5[c("big5_n_t5","big5_e_t5","big5_o_t5","big5_a_t5","big5_c_t5","rses_t5","saq_t5","npi_t5","narq_adm_t5","narq_riv_t5","dido_mach_t5","dido_psych_t5","dido_narc_t5","wmc_solved","wmc_span","mwtb_total","raven_total")],adjust ="none")[[4]], digits =3)
SelfSignificanceT5

#Correlations self with aquaintance reports
CorrelationSelfAcquT1 <- merge (as.data.frame(select(Connect_Traits_t1, id_a, big5_n_t1, big5_e_t1, big5_o_t1, big5_a_t1, big5_c_t1, rses_t1, saq_t1, npi_short_t1, narq_adm_t1, narq_riv_t1, dido_mach_t1, dido_psych_t1, dido_narc_t1)),as.data.frame(select(Connect_Traits_Acq_Agg, id_a, big5_n_acqu, big5_e_acqu, big5_o_acqu, big5_a_acqu, big5_c_acqu,rses_acqu, saq_acqu, npi_short_acqu, narq_adm_acqu, narq_riv_acqu, dido_mach_acqu, dido_psych_acqu, dido_narc_acqu)), by ="id_a" )

# Correlations self-/informant-reports
AcquvaluesT1 <- round (corr.test(CorrelationSelfAcquT1,adjust ="none")[[1]], digits =2)
## again in the diagonal
AcquvaluesT1[c("big5_n_t1", "big5_e_t1", "big5_o_t1", "big5_a_t1", "big5_c_t1", "rses_t1", "saq_t1", "npi_short_t1", "narq_adm_t1", "narq_riv_t1", "dido_mach_t1", "dido_psych_t1", "dido_narc_t1"),c("big5_n_acqu", "big5_e_acqu", "big5_o_acqu", "big5_a_acqu", "big5_c_acqu","rses_acqu", "saq_acqu", "npi_short_acqu", "narq_adm_acqu", "narq_riv_acqu", "dido_mach_acqu", "dido_psych_acqu", "dido_narc_acqu")]

## Ns of correlations self-/informant-reportsc
AcquNT1 <- round (corr.test(CorrelationSelfAcquT1,adjust ="none")[[2]])
AcquNT1[c("big5_n_t1", "big5_e_t1", "big5_o_t1", "big5_a_t1", "big5_c_t1", "rses_t1", "saq_t1", "npi_short_t1", "narq_adm_t1", "narq_riv_t1", "dido_mach_t1", "dido_psych_t1", "dido_narc_t1"),c("big5_n_acqu", "big5_e_acqu", "big5_o_acqu", "big5_a_acqu", "big5_c_acqu","rses_acqu", "saq_acqu", "npi_short_acqu", "narq_adm_acqu", "narq_riv_acqu", "dido_mach_acqu", "dido_psych_acqu", "dido_narc_acqu")]

## Significance
AcquSignificanceT1 <- round (corr.test(CorrelationSelfAcquT1,adjust ="none")[[4]], digits =3)
AcquSignificanceT1[c("big5_n_t1", "big5_e_t1", "big5_o_t1", "big5_a_t1", "big5_c_t1", "rses_t1", "saq_t1", "npi_short_t1", "narq_adm_t1", "narq_riv_t1", "dido_mach_t1", "dido_psych_t1", "dido_narc_t1"),c("big5_n_acqu", "big5_e_acqu", "big5_o_acqu", "big5_a_acqu", "big5_c_acqu","rses_acqu", "saq_acqu", "npi_short_acqu", "narq_adm_acqu", "narq_riv_acqu", "dido_mach_acqu", "dido_psych_acqu", "dido_narc_acqu")]


############################################################################################
## I. TABLE 15                                                                            ##
############################################################################################

## Preparation of Self-Perceptions: Phases
Connect_Selfperceptions_Phase1 <- as.data.frame(subset(Connect_Diary_Long, Connect_Diary_Long$id_a == Connect_Diary_Long$id_p & Connect_Diary_Long$diary_nr_total < 8))
Connect_Selfperceptions_Phase2 <- as.data.frame(subset(Connect_Diary_Long, Connect_Diary_Long$id_a == Connect_Diary_Long$id_p & Connect_Diary_Long$diary_nr_total > 7 & Connect_Diary_Long$diary_nr_total < 15 ))
Connect_Selfperceptions_Phase3 <- as.data.frame(subset(Connect_Diary_Long, Connect_Diary_Long$id_a == Connect_Diary_Long$id_p & Connect_Diary_Long$diary_nr_total > 14 & Connect_Diary_Long$diary_nr_total < 22 ))

## Preparation of Other-Perceptions: Phases
Connect_Otherperceptions_Phase1 <- as.data.frame(subset(Connect_Diary_Long, Connect_Diary_Long$id_a != Connect_Diary_Long$id_p & Connect_Diary_Long$diary_nr_total < 8))
Connect_Otherperceptions_Phase2 <- as.data.frame(subset(Connect_Diary_Long, Connect_Diary_Long$id_a != Connect_Diary_Long$id_p & Connect_Diary_Long$diary_nr_total > 7 & Connect_Diary_Long$diary_nr_total < 15 ))
Connect_Otherperceptions_Phase3 <- as.data.frame(subset(Connect_Diary_Long, Connect_Diary_Long$id_a != Connect_Diary_Long$id_p & Connect_Diary_Long$diary_nr_total > 14 & Connect_Diary_Long$diary_nr_total < 22 ))

## PHASE 1

## SELF-PERCEPTIONS
summary(lmer(liking ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase1))

## no multilevel for other variables because there was only 1 timepoint, just Ms and Sds
describe(Connect_Selfperceptions_Phase1[,c("extraversion_introversion","critical_warmhearted","reliable_careless","anxious_stable","interested_conventional","dominant_submissive","affectionate_coldhearted","intelligent_unintelligent","attractive_unattractive","admiration","rivalry")])

## OTHER-PERCEPTIONS

## Means
summary(lmer(knowing ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(liking ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase1))
summary(lmer(metaliking ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase1))
summary(lmer(extraversion_introversion ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase1))
summary(lmer(critical_warmhearted ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase1))
summary(lmer(reliable_careless ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(anxious_stable ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase1))
summary(lmer(interested_conventional ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase1))
summary(lmer(dominant_submissive ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(affectionate_coldhearted ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(intelligent_unintelligent ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(attractive_unattractive ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(admiration ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(rivalry ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(relsatisfied ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(relimportant ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(conflict ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(emosupport ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(instsupport ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(exchange ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))
summary(lmer(acceptance ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase1))

## Variances
summary(lmer(knowing ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(liking ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(metaliking ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(extraversion_introversion ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(critical_warmhearted ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(reliable_careless ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(anxious_stable ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(interested_conventional ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(dominant_submissive ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(affectionate_coldhearted ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(intelligent_unintelligent ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(attractive_unattractive ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(admiration ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(rivalry ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(relsatisfied ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(relimportant ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(conflict ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(emosupport ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(instsupport ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(exchange ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))
summary(lmer(acceptance ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase1))

## Phase2

## Self-Perceptions

summary(lmer(liking ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(extraversion_introversion ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(critical_warmhearted ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(reliable_careless ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(anxious_stable ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(interested_conventional ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(dominant_submissive ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(affectionate_coldhearted ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(intelligent_unintelligent ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(attractive_unattractive ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(admiration ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))
summary(lmer(rivalry ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase2))

## Other-Perceptions

## Means
summary(lmer(knowing ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(liking ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase2))
summary(lmer(metaliking ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase2))
summary(lmer(extraversion_introversion ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase2))
summary(lmer(critical_warmhearted ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase2))
summary(lmer(reliable_careless ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(anxious_stable ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase2))
summary(lmer(interested_conventional ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase2))
summary(lmer(dominant_submissive ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(affectionate_coldhearted ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(intelligent_unintelligent ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(attractive_unattractive ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(admiration ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(rivalry ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(relsatisfied ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(relimportant ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(conflict ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(emosupport ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(instsupport ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(exchange ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))
summary(lmer(acceptance ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase2))

## Variances
summary(lmer(knowing ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(liking ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(metaliking ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(extraversion_introversion ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(critical_warmhearted ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(reliable_careless ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(anxious_stable ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(interested_conventional ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(dominant_submissive ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(affectionate_coldhearted ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(intelligent_unintelligent ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(attractive_unattractive ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(admiration ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(rivalry ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(relsatisfied ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(relimportant ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(conflict ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(emosupport ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(instsupport ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(exchange ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))
summary(lmer(acceptance ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase2))

## PHASE 3

## Self-Perceptions
summary(lmer(liking ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(extraversion_introversion ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(critical_warmhearted ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(reliable_careless ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(anxious_stable ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(interested_conventional ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(dominant_submissive ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(affectionate_coldhearted ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(intelligent_unintelligent ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(attractive_unattractive ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(admiration ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))
summary(lmer(rivalry ~ 1 + (1|id_a), data = Connect_Selfperceptions_Phase3))

## Other-Perceptions

## Means
summary(lmer(knowing ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(liking ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase3))
summary(lmer(metaliking ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase3))
summary(lmer(extraversion_introversion ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase3))
summary(lmer(critical_warmhearted ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase3))
summary(lmer(reliable_careless ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(anxious_stable ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase3))
summary(lmer(interested_conventional ~ 1 + (1|id_a) , data = Connect_Otherperceptions_Phase3))
summary(lmer(dominant_submissive ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(affectionate_coldhearted ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(intelligent_unintelligent ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(attractive_unattractive ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(admiration ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(rivalry ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(relsatisfied ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(relimportant ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(conflict ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(emosupport ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(instsupport ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(exchange ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))
summary(lmer(acceptance ~ 1 + (1|id_a)  , data = Connect_Otherperceptions_Phase3))

## Variances
summary(lmer(knowing ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(liking ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(metaliking ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(extraversion_introversion ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(critical_warmhearted ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(reliable_careless ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(anxious_stable ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(interested_conventional ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(dominant_submissive ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(affectionate_coldhearted ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(intelligent_unintelligent ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(attractive_unattractive ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(admiration ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(rivalry ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(relsatisfied ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(relimportant ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(conflict ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(emosupport ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(instsupport ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(exchange ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))
summary(lmer(acceptance ~ 1 + (1|id_a) + (1|id_p) , data = Connect_Otherperceptions_Phase3))


############################################################################################
## J. TABLE 16                                                                            ##
############################################################################################

## Preparation of Self-Perceptions
Connect_App_Selfperceptions_Phase1 <- as.data.frame(subset(Connect_App_Unique, day > 2 & day < 24 )) # Day 1 and 2 = Phase 0; not included in Table
Connect_App_Selfperceptions_Phase2 <- as.data.frame(subset(Connect_App_Unique, day > 58 & day < 66  ))
Connect_App_Selfperceptions_Phase3 <- as.data.frame(subset(Connect_App_Unique, day > 106 & day < 114 ))

## Preparation of Other-Perceptions
Connect_App_Otherperceptions_Phase1 <- as.data.frame(subset(Connect_App_Long, day  > 2 & day < 24 )) # Day 1 and 2 = Phase 0; not included in Table
Connect_App_Otherperceptions_Phase2 <- as.data.frame(subset(Connect_App_Long, day > 58 & day < 66  ))
Connect_App_Otherperceptions_Phase3 <- as.data.frame(subset(Connect_App_Long, day > 106 & day < 114 ))

## PHASE 1

## self-Perceptions

summary(lmer(SR_dominant_submissive ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_sociable_reclusive ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_friendly_unfriendly ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_arrogant_modest ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_exploiting_cooperative ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_selfrevealing_reserved ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_reliable_unreliable ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_good_bad_mood ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_bored_activated ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_nervous_relaxed ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_inhibited_determined ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_dis_satisfiedwmyself ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))
summary(lmer(SR_satisfied_dis_winteraction ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase1))

## Other-Perceptions

## Means
summary(lmer(OR_dominant_submissive ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_sociable_reclusive ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_friendly_unfriendly ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_arrogant_modest ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_exploiting_cooperative ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_selfrevealing_reserved ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_reliable_unreliable ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(IR_interesting_uninteresting ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(IR_positive_negative ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(IR_meta_interesting_uninteresting ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(IR_meta_positive_negative ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase1))

## Variances
summary(lmer(OR_dominant_submissive ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_sociable_reclusive ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_friendly_unfriendly ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_arrogant_modest ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_exploiting_cooperative ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_selfrevealing_reserved ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(OR_reliable_unreliable ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(IR_interesting_uninteresting ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(IR_positive_negative ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(IR_meta_interesting_uninteresting ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))
summary(lmer(IR_meta_positive_negative ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase1))

## PHASE 2

## Self-Perceptions

summary(lmer(SR_dominant_submissive ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_sociable_reclusive ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_friendly_unfriendly ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_arrogant_modest ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_exploiting_cooperative ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_selfrevealing_reserved ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_reliable_unreliable ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_good_bad_mood ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_bored_activated ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_nervous_relaxed ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_inhibited_determined ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_dis_satisfiedwmyself ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))
summary(lmer(SR_satisfied_dis_winteraction ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase2))

## Other-Perceptions

## Means
summary(lmer(OR_dominant_submissive ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_sociable_reclusive ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_friendly_unfriendly ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_arrogant_modest ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_exploiting_cooperative ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_selfrevealing_reserved ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_reliable_unreliable ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(IR_interesting_uninteresting ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(IR_positive_negative ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(IR_meta_interesting_uninteresting ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(IR_meta_positive_negative ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase2))

## Variances
summary(lmer(OR_dominant_submissive ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_sociable_reclusive ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_friendly_unfriendly ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_arrogant_modest ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_exploiting_cooperative ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_selfrevealing_reserved ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(OR_reliable_unreliable ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(IR_interesting_uninteresting ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(IR_positive_negative ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(IR_meta_interesting_uninteresting ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))
summary(lmer(IR_meta_positive_negative ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase2))

## PHASE 3

## Self-Percpetions
summary(lmer(SR_dominant_submissive ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_sociable_reclusive ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_friendly_unfriendly ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_arrogant_modest ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_exploiting_cooperative ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_selfrevealing_reserved ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_reliable_unreliable ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_good_bad_mood ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_bored_activated ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_nervous_relaxed ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_inhibited_determined ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_dis_satisfiedwmyself ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))
summary(lmer(SR_satisfied_dis_winteraction ~ 1 + (1|id_a), data = Connect_App_Selfperceptions_Phase3))

## Other-Percpetions

## Means
summary(lmer(OR_dominant_submissive ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_sociable_reclusive ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_friendly_unfriendly ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_arrogant_modest ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_exploiting_cooperative ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_selfrevealing_reserved ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_reliable_unreliable ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(IR_interesting_uninteresting ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(IR_positive_negative ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(IR_meta_interesting_uninteresting ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(IR_meta_positive_negative ~ 1  +(1|id_a), data = Connect_App_Otherperceptions_Phase3))

## Variances
summary(lmer(OR_dominant_submissive ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_sociable_reclusive ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_friendly_unfriendly ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_arrogant_modest ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_exploiting_cooperative ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_selfrevealing_reserved ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(OR_reliable_unreliable ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(IR_interesting_uninteresting ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(IR_positive_negative ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(IR_meta_interesting_uninteresting ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))
summary(lmer(IR_meta_positive_negative ~ 1 +(1|id_p) +(1|id_a), data = Connect_App_Otherperceptions_Phase3))


############################################################################################
## K. TABLE 17                                                                            ##
############################################################################################

## Attractiveness 1 (zero-acquaintance)

## Means and SDs
describe(Connect_DirectObs_Agg[,c("attractiveness_face_SI1","hardness_face_SI1","styled_hair_SI1","neat_hair_SI1","flashy_clothes_SI1","neat_clothes_SI1","modern_clothes_SI1","attractiveness_body_SI1")])

## ICCs
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, attractiveness_face_SI1_R1  ,  attractiveness_face_SI1_R2	,	attractiveness_face_SI1_R3	,	attractiveness_face_SI1_R4	,	attractiveness_face_SI1_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, hardness_face_SI1_R1	,	hardness_face_SI1_R2	,	hardness_face_SI1_R3	,	hardness_face_SI1_R4	,	hardness_face_SI1_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, styled_hair_SI1_R1	,	styled_hair_SI1_R2	,	styled_hair_SI1_R3	,	styled_hair_SI1_R4	,	styled_hair_SI1_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, neat_hair_SI1_R1	,	neat_hair_SI1_R2	,	neat_hair_SI1_R3	,	neat_hair_SI1_R4	,	neat_hair_SI1_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, flashy_clothes_SI1_R1	,	flashy_clothes_SI1_R2	,	flashy_clothes_SI1_R3	,	flashy_clothes_SI1_R4	,	flashy_clothes_SI1_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, neat_clothes_SI1_R1	,	neat_clothes_SI1_R2	,	neat_clothes_SI1_R3	,	neat_clothes_SI1_R4	,	neat_clothes_SI1_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, modern_clothes_SI1_R1	,	modern_clothes_SI1_R2	,	modern_clothes_SI1_R3	,	modern_clothes_SI1_R4	,	modern_clothes_SI1_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, attractiveness_body_SI1_R1	,	attractiveness_body_SI1_R2	,	attractiveness_body_SI1_R3	,	attractiveness_body_SI1_R4	,	attractiveness_body_SI1_R5))))

## Attractiveness 2 (laboratory)

## Means and SDs
describe(Connect_DirectObs_Agg[,c("attractiveness_face_SI2","hardness_face_SI2","styled_hair_SI2","neat_hair_SI2","flashy_clothes_SI2","neat_clothes_SI2","modern_clothes_SI2","attractiveness_body_SI2")])

## Iccs
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, attractiveness_face_SI2_R1	,	attractiveness_face_SI2_R2	,	attractiveness_face_SI2_R3	,	attractiveness_face_SI2_R4	,	attractiveness_face_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, hardness_face_SI2_R1	,	hardness_face_SI2_R2	,	hardness_face_SI2_R3	,	hardness_face_SI2_R4	,	hardness_face_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, styled_hair_SI2_R1	,	styled_hair_SI2_R2	,	styled_hair_SI2_R3	,	styled_hair_SI2_R4	,	styled_hair_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, neat_hair_SI2_R1	,	neat_hair_SI2_R2	,	neat_hair_SI2_R3	,	neat_hair_SI2_R4	,	neat_hair_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, flashy_clothes_SI2_R1	,	flashy_clothes_SI2_R2	,	flashy_clothes_SI2_R3	,	flashy_clothes_SI2_R4	,	flashy_clothes_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, neat_clothes_SI2_R1	,	neat_clothes_SI2_R2	,	neat_clothes_SI2_R3	,	neat_clothes_SI2_R4	,	neat_clothes_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, modern_clothes_SI2_R1	,	modern_clothes_SI2_R2	,	modern_clothes_SI2_R3	,	modern_clothes_SI2_R4	,	modern_clothes_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, attractiveness_body_SI2_R1	,	attractiveness_body_SI2_R2	,	attractiveness_body_SI2_R3	,	attractiveness_body_SI2_R4	,	attractiveness_body_SI2_R5))))

## Self-Introduction 1 (zero-acquaintance)

## Means and SDs
describe(Connect_DirectObs_Agg[,c("expressiveness_SI1","selfconfidence_SI1","arrogance_SI1","friendliness_SI1","nervousness_SI1")])

## Iccs
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single,expressiveness_SI1_R1,expressiveness_SI1_R2,expressiveness_SI1_R3,	expressiveness_SI1_R4,expressiveness_SI1_R5,expressiveness_SI1_R6,expressiveness_SI1_R7))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, selfconfidence_SI1_R1,selfconfidence_SI1_R2,selfconfidence_SI1_R3,selfconfidence_SI1_R4,selfconfidence_SI1_R5,selfconfidence_SI1_R6,selfconfidence_SI1_R7))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single,arrogance_SI1_R1,arrogance_SI1_R2,arrogance_SI1_R3,arrogance_SI1_R4,arrogance_SI1_R5,arrogance_SI1_R6,arrogance_SI1_R7))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single,friendliness_SI1_R1,friendliness_SI1_R2,friendliness_SI1_R3,friendliness_SI1_R4,friendliness_SI1_R5,friendliness_SI1_R6,friendliness_SI1_R7))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, nervousness_SI1_R1	,	nervousness_SI1_R2	,	nervousness_SI1_R3	,	nervousness_SI1_R4	,	nervousness_SI1_R5,	nervousness_SI1_R6,	nervousness_SI1_R7))))

## Small-Talk (laboratory)

## Preparation: create variables for Mean and SDs
Connect_DirectObs_Agg$expressiveness_SMT <- (Connect_DirectObs_Agg$expressiveness_SMT_a  + Connect_DirectObs_Agg$expressiveness_SMT_b + Connect_DirectObs_Agg$expressiveness_SMT_c)/3
Connect_DirectObs_Agg$warmth_SMT <- (Connect_DirectObs_Agg$warmth_SMT_a  + Connect_DirectObs_Agg$warmth_SMT_b + Connect_DirectObs_Agg$warmth_SMT_c)/3
Connect_DirectObs_Agg$nervousness_SMT <- (Connect_DirectObs_Agg$nervousness_SMT_a  + Connect_DirectObs_Agg$nervousness_SMT_b)/2

## Means and SDs
describe(Connect_DirectObs_Agg[,c("expressiveness_SMT","arrogance_SMT","warmth_SMT","nervousness_SMT")])

## Preparation for ICCs

#Small Talk R1
Connect_DirectObs_Single$warmth_SMT_R1 <- (Connect_DirectObs_Single$warmth_SMT_a_R1  + Connect_DirectObs_Single$warmth_SMT_b_R1 + Connect_DirectObs_Single$warmth_SMT_c_R1) /3
Connect_DirectObs_Single$expressiveness_SMT_R1 <- (Connect_DirectObs_Single$expressiveness_SMT_a_R1  + Connect_DirectObs_Single$expressiveness_SMT_b_R1 + Connect_DirectObs_Single$expressiveness_SMT_c_R1) /3
Connect_DirectObs_Single$nervousness_SMT_R1 <- (Connect_DirectObs_Single$nervousness_SMT_a_R1  + Connect_DirectObs_Single$nervousness_SMT_b_R1) /2
#Small Talk R2
Connect_DirectObs_Single$warmth_SMT_R2 <- (Connect_DirectObs_Single$warmth_SMT_a_R2  + Connect_DirectObs_Single$warmth_SMT_b_R2 + Connect_DirectObs_Single$warmth_SMT_c_R2) /3
Connect_DirectObs_Single$expressiveness_SMT_R2 <- (Connect_DirectObs_Single$expressiveness_SMT_a_R2  + Connect_DirectObs_Single$expressiveness_SMT_b_R2 + Connect_DirectObs_Single$expressiveness_SMT_c_R2) /3
Connect_DirectObs_Single$nervousness_SMT_R2 <- (Connect_DirectObs_Single$nervousness_SMT_a_R2  + Connect_DirectObs_Single$nervousness_SMT_b_R2) /2
#Small Talk R3
Connect_DirectObs_Single$warmth_SMT_R3 <- (Connect_DirectObs_Single$warmth_SMT_a_R3  + Connect_DirectObs_Single$warmth_SMT_b_R3 + Connect_DirectObs_Single$warmth_SMT_c_R3) /3
Connect_DirectObs_Single$expressiveness_SMT_R3 <- (Connect_DirectObs_Single$expressiveness_SMT_a_R3  + Connect_DirectObs_Single$expressiveness_SMT_b_R3 + Connect_DirectObs_Single$expressiveness_SMT_c_R3) /3
Connect_DirectObs_Single$nervousness_SMT_R3 <- (Connect_DirectObs_Single$nervousness_SMT_a_R3  + Connect_DirectObs_Single$nervousness_SMT_b_R3) /2
#Small Talk R4
Connect_DirectObs_Single$warmth_SMT_R4 <- (Connect_DirectObs_Single$warmth_SMT_a_R4  + Connect_DirectObs_Single$warmth_SMT_b_R4 + Connect_DirectObs_Single$warmth_SMT_c_R4) /3
Connect_DirectObs_Single$expressiveness_SMT_R4 <- (Connect_DirectObs_Single$expressiveness_SMT_a_R4  + Connect_DirectObs_Single$expressiveness_SMT_b_R4 + Connect_DirectObs_Single$expressiveness_SMT_c_R4) /3
Connect_DirectObs_Single$nervousness_SMT_R4 <- (Connect_DirectObs_Single$nervousness_SMT_a_R4  + Connect_DirectObs_Single$nervousness_SMT_b_R4) /2
#Small Talk R5
Connect_DirectObs_Single$warmth_SMT_R5 <- (Connect_DirectObs_Single$warmth_SMT_a_R5  + Connect_DirectObs_Single$warmth_SMT_b_R5 + Connect_DirectObs_Single$warmth_SMT_c_R5) /3
Connect_DirectObs_Single$expressiveness_SMT_R5 <- (Connect_DirectObs_Single$expressiveness_SMT_a_R5  + Connect_DirectObs_Single$expressiveness_SMT_b_R5 + Connect_DirectObs_Single$expressiveness_SMT_c_R5) /3
Connect_DirectObs_Single$nervousness_SMT_R5 <- (Connect_DirectObs_Single$nervousness_SMT_a_R5  + Connect_DirectObs_Single$nervousness_SMT_b_R5) /2

## ICCs

ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, expressiveness_SMT_R1  ,	expressiveness_SMT_R2	,	expressiveness_SMT_R3	,	expressiveness_SMT_R4	,	expressiveness_SMT_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, arrogance_SMT_R1  ,	arrogance_SMT_R2	,	arrogance_SMT_R3	,	arrogance_SMT_R4	,	arrogance_SMT_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, warmth_SMT_R1,  warmth_SMT_R2	,	warmth_SMT_R3	,	warmth_SMT_R4	,	warmth_SMT_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, nervousness_SMT_R1	,	nervousness_SMT_R2	,	nervousness_SMT_R3	,	nervousness_SMT_R4	,	nervousness_SMT_R5))))


## Self Introduction 2 (laboratory)

## Preparation: create variables for Mean and SDs
Connect_DirectObs_Agg$expressiveness_SI2 <- (Connect_DirectObs_Agg$expressiveness_SI2_a  + Connect_DirectObs_Agg$expressiveness_SI2_b + Connect_DirectObs_Agg$expressiveness_SI2_c + Connect_DirectObs_Agg$expressiveness_SI2_d)/4
Connect_DirectObs_Agg$selfconfidence_SI2 <- (Connect_DirectObs_Agg$selfconfidence_SI2_a  + Connect_DirectObs_Agg$selfconfidence_SI2_b + Connect_DirectObs_Agg$selfconfidence_SI2_c)  /3
Connect_DirectObs_Agg$warmth_SI2 <- (Connect_DirectObs_Agg$warmth_SI2_a  + Connect_DirectObs_Agg$warmth_SI2_b ) /2
Connect_DirectObs_Agg$nervousness_SI2 <- (Connect_DirectObs_Agg$nervousness_SI2_a  + Connect_DirectObs_Agg$nervousness_SI2_b + Connect_DirectObs_Agg$nervousness_SI2_c + Connect_DirectObs_Agg$nervousness_SI2_d)/4

## Means and SDs
describe(Connect_DirectObs_Agg[,c("expressiveness_SI2","selfconfidence_SI2","arrogance_SI2","warmth_SI2","nervousness_SI2")])

## Preparation for ICCs

#Self IntroductionR1
Connect_DirectObs_Single$expressiveness_SI2_R1 <- (Connect_DirectObs_Single$expressiveness_SI2_a_R1  + Connect_DirectObs_Single$expressiveness_SI2_b_R1 + Connect_DirectObs_Single$expressiveness_SI2_c_R1 + Connect_DirectObs_Single$expressiveness_SI2_d_R1) /4
Connect_DirectObs_Single$nervousness_SI2_R1 <- (Connect_DirectObs_Single$nervousness_SI2_a_R1  + Connect_DirectObs_Single$nervousness_SI2_b_R1 + Connect_DirectObs_Single$nervousness_SI2_c_R1 + Connect_DirectObs_Single$nervousness_SI2_d_R1) /4
Connect_DirectObs_Single$selfconfidence_SI2_R1 <- (Connect_DirectObs_Single$selfconfidence_SI2_a_R1  + Connect_DirectObs_Single$selfconfidence_SI2_b_R1 + Connect_DirectObs_Single$selfconfidence_SI2_c_R1)  /3
Connect_DirectObs_Single$warmth_SI2_R1 <- (Connect_DirectObs_Single$warmth_SI2_a_R1  + Connect_DirectObs_Single$warmth_SI2_b_R1 )  /2
#Self Introduction R2
Connect_DirectObs_Single$expressiveness_SI2_R2 <- (Connect_DirectObs_Single$expressiveness_SI2_a_R2  + Connect_DirectObs_Single$expressiveness_SI2_b_R2 + Connect_DirectObs_Single$expressiveness_SI2_c_R2 + Connect_DirectObs_Single$expressiveness_SI2_d_R2) /4
Connect_DirectObs_Single$nervousness_SI2_R2 <- (Connect_DirectObs_Single$nervousness_SI2_a_R2  + Connect_DirectObs_Single$nervousness_SI2_b_R2 + Connect_DirectObs_Single$nervousness_SI2_c_R2 + Connect_DirectObs_Single$nervousness_SI2_d_R2) /4
Connect_DirectObs_Single$selfconfidence_SI2_R2 <- (Connect_DirectObs_Single$selfconfidence_SI2_a_R2  + Connect_DirectObs_Single$selfconfidence_SI2_b_R2 + Connect_DirectObs_Single$selfconfidence_SI2_c_R2)  /3
Connect_DirectObs_Single$warmth_SI2_R2 <- (Connect_DirectObs_Single$warmth_SI2_a_R2  + Connect_DirectObs_Single$warmth_SI2_b_R2 )  /2
#Self Introduction R3
Connect_DirectObs_Single$expressiveness_SI2_R3 <- (Connect_DirectObs_Single$expressiveness_SI2_a_R3  + Connect_DirectObs_Single$expressiveness_SI2_b_R3 + Connect_DirectObs_Single$expressiveness_SI2_c_R3 + Connect_DirectObs_Single$expressiveness_SI2_d_R3) /4
Connect_DirectObs_Single$nervousness_SI2_R3 <- (Connect_DirectObs_Single$nervousness_SI2_a_R3  + Connect_DirectObs_Single$nervousness_SI2_b_R3 + Connect_DirectObs_Single$nervousness_SI2_c_R3 + Connect_DirectObs_Single$nervousness_SI2_d_R3) /4
Connect_DirectObs_Single$selfconfidence_SI2_R3 <- (Connect_DirectObs_Single$selfconfidence_SI2_a_R3  + Connect_DirectObs_Single$selfconfidence_SI2_b_R3 + Connect_DirectObs_Single$selfconfidence_SI2_c_R3)  /3
Connect_DirectObs_Single$warmth_SI2_R3 <- (Connect_DirectObs_Single$warmth_SI2_a_R3  + Connect_DirectObs_Single$warmth_SI2_b_R3 )  /2
#Self Introduction R4
Connect_DirectObs_Single$expressiveness_SI2_R4 <- (Connect_DirectObs_Single$expressiveness_SI2_a_R4  + Connect_DirectObs_Single$expressiveness_SI2_b_R4 + Connect_DirectObs_Single$expressiveness_SI2_c_R4 + Connect_DirectObs_Single$expressiveness_SI2_d_R4) /4
Connect_DirectObs_Single$nervousness_SI2_R4 <- (Connect_DirectObs_Single$nervousness_SI2_a_R4  + Connect_DirectObs_Single$nervousness_SI2_b_R4 + Connect_DirectObs_Single$nervousness_SI2_c_R4 + Connect_DirectObs_Single$nervousness_SI2_d_R4) /4
Connect_DirectObs_Single$selfconfidence_SI2_R4 <- (Connect_DirectObs_Single$selfconfidence_SI2_a_R4  + Connect_DirectObs_Single$selfconfidence_SI2_b_R4 + Connect_DirectObs_Single$selfconfidence_SI2_c_R4)  /3
Connect_DirectObs_Single$warmth_SI2_R4 <- (Connect_DirectObs_Single$warmth_SI2_a_R4  + Connect_DirectObs_Single$warmth_SI2_b_R4 )  /2
#Self Introduction R5
Connect_DirectObs_Single$expressiveness_SI2_R5 <- (Connect_DirectObs_Single$expressiveness_SI2_a_R5  + Connect_DirectObs_Single$expressiveness_SI2_b_R5 + Connect_DirectObs_Single$expressiveness_SI2_c_R5 + Connect_DirectObs_Single$expressiveness_SI2_d_R5) /4
Connect_DirectObs_Single$nervousness_SI2_R5 <- (Connect_DirectObs_Single$nervousness_SI2_a_R5  + Connect_DirectObs_Single$nervousness_SI2_b_R5 + Connect_DirectObs_Single$nervousness_SI2_c_R5 + Connect_DirectObs_Single$nervousness_SI2_d_R5) /4
Connect_DirectObs_Single$selfconfidence_SI2_R5 <- (Connect_DirectObs_Single$selfconfidence_SI2_a_R5  + Connect_DirectObs_Single$selfconfidence_SI2_b_R5 + Connect_DirectObs_Single$selfconfidence_SI2_c_R5)  /3
Connect_DirectObs_Single$warmth_SI2_R5 <- (Connect_DirectObs_Single$warmth_SI2_a_R5  + Connect_DirectObs_Single$warmth_SI2_b_R5 )  /2

## ICCs

ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, expressiveness_SI2_R1  ,	expressiveness_SI2_R2	,	expressiveness_SI2_R3	,	expressiveness_SI2_R4	,	expressiveness_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, selfconfidence_SI2_R1  ,	selfconfidence_SI2_R2	,	selfconfidence_SI2_R3	,	selfconfidence_SI2_R4	,	selfconfidence_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, arrogance_SI2_R1  ,	arrogance_SI2_R2	,	arrogance_SI2_R3	,	arrogance_SI2_R4	,	arrogance_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, warmth_SI2_R1  ,	warmth_SI2_R2	,	warmth_SI2_R3	,	warmth_SI2_R4	,	warmth_SI2_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, nervousness_SI2_R1	,	nervousness_SI2_R2	,	nervousness_SI2_R3	,	nervousness_SI2_R4	,	nervousness_SI2_R5))))

## Stress Test (laboratory)

## Preparation: create variables for Mean and SDs
Connect_DirectObs_Agg$nervousness_ST <- (Connect_DirectObs_Agg$nervousness_ST_a  + Connect_DirectObs_Agg$nervousness_ST_b + Connect_DirectObs_Agg$nervousness_ST_c  + Connect_DirectObs_Agg$nervousness_ST_d  )  /4
Connect_DirectObs_Agg$intellectual_ST <- (Connect_DirectObs_Agg$intellectual_ST_a  + Connect_DirectObs_Agg$intellectual_ST_b + Connect_DirectObs_Agg$intellectual_ST_c  + Connect_DirectObs_Agg$intellectual_ST_d  )  /4
Connect_DirectObs_Agg$expressiveness_ST <- (Connect_DirectObs_Agg$expressiveness_ST_a  + Connect_DirectObs_Agg$expressiveness_ST_b + Connect_DirectObs_Agg$expressiveness_ST_c  + Connect_DirectObs_Agg$expressiveness_ST_d  )  /4
Connect_DirectObs_Agg$selfconfidence_ST <- (Connect_DirectObs_Agg$selfconfidence_ST_a  + Connect_DirectObs_Agg$selfconfidence_ST_b) /2

## Means and SDs
describe(Connect_DirectObs_Agg[,c("expressiveness_ST","selfconfidence_ST","nervousness_ST","intellectual_ST")])

## Preparation for ICCs

#StressTest R1
Connect_DirectObs_Single$nervousness_ST_R1 <- (Connect_DirectObs_Single$nervousness_ST_a_R1  + Connect_DirectObs_Single$nervousness_ST_b_R1 + Connect_DirectObs_Single$nervousness_ST_c_R1  + Connect_DirectObs_Single$nervousness_ST_d_R1  )  /4
Connect_DirectObs_Single$intellectual_ST_R1 <- (Connect_DirectObs_Single$intellectual_ST_a_R1  + Connect_DirectObs_Single$intellectual_ST_b_R1 + Connect_DirectObs_Single$intellectual_ST_c_R1  + Connect_DirectObs_Single$intellectual_ST_d_R1  )  /4
Connect_DirectObs_Single$expressiveness_ST_R1 <- (Connect_DirectObs_Single$expressiveness_ST_a_R1  + Connect_DirectObs_Single$expressiveness_ST_b_R1 + Connect_DirectObs_Single$expressiveness_ST_c_R1  + Connect_DirectObs_Single$expressiveness_ST_d_R1  )  /4
Connect_DirectObs_Single$selfconfidence_ST_R1 <- (Connect_DirectObs_Single$selfconfidence_ST_a_R1  + Connect_DirectObs_Single$selfconfidence_ST_b_R1)  /2
#StressTest R2
Connect_DirectObs_Single$nervousness_ST_R2 <- (Connect_DirectObs_Single$nervousness_ST_a_R2  + Connect_DirectObs_Single$nervousness_ST_b_R2 + Connect_DirectObs_Single$nervousness_ST_c_R2  + Connect_DirectObs_Single$nervousness_ST_d_R2  )  /4
Connect_DirectObs_Single$intellectual_ST_R2 <- (Connect_DirectObs_Single$intellectual_ST_a_R2  + Connect_DirectObs_Single$intellectual_ST_b_R2 + Connect_DirectObs_Single$intellectual_ST_c_R2  + Connect_DirectObs_Single$intellectual_ST_d_R2  )  /4
Connect_DirectObs_Single$expressiveness_ST_R2 <- (Connect_DirectObs_Single$expressiveness_ST_a_R2  + Connect_DirectObs_Single$expressiveness_ST_b_R2 + Connect_DirectObs_Single$expressiveness_ST_c_R2  + Connect_DirectObs_Single$expressiveness_ST_d_R2  )  /4
Connect_DirectObs_Single$selfconfidence_ST_R2 <- (Connect_DirectObs_Single$selfconfidence_ST_a_R2  + Connect_DirectObs_Single$selfconfidence_ST_b_R2)  /2
#StressTest R3
Connect_DirectObs_Single$nervousness_ST_R3 <- (Connect_DirectObs_Single$nervousness_ST_a_R3  + Connect_DirectObs_Single$nervousness_ST_b_R3 + Connect_DirectObs_Single$nervousness_ST_c_R3  + Connect_DirectObs_Single$nervousness_ST_d_R3  )  /4
Connect_DirectObs_Single$intellectual_ST_R3 <- (Connect_DirectObs_Single$intellectual_ST_a_R3  + Connect_DirectObs_Single$intellectual_ST_b_R3 + Connect_DirectObs_Single$intellectual_ST_c_R3  + Connect_DirectObs_Single$intellectual_ST_d_R3  )  /4
Connect_DirectObs_Single$expressiveness_ST_R3 <- (Connect_DirectObs_Single$expressiveness_ST_a_R3  + Connect_DirectObs_Single$expressiveness_ST_b_R3 + Connect_DirectObs_Single$expressiveness_ST_c_R3  + Connect_DirectObs_Single$expressiveness_ST_d_R3  )  /4
Connect_DirectObs_Single$selfconfidence_ST_R3 <- (Connect_DirectObs_Single$selfconfidence_ST_a_R3  + Connect_DirectObs_Single$selfconfidence_ST_b_R3)  /2
#StressTest R4
Connect_DirectObs_Single$nervousness_ST_R4 <- (Connect_DirectObs_Single$nervousness_ST_a_R4  + Connect_DirectObs_Single$nervousness_ST_b_R4 + Connect_DirectObs_Single$nervousness_ST_c_R4  + Connect_DirectObs_Single$nervousness_ST_d_R4  )  /4
Connect_DirectObs_Single$intellectual_ST_R4 <- (Connect_DirectObs_Single$intellectual_ST_a_R4  + Connect_DirectObs_Single$intellectual_ST_b_R4 + Connect_DirectObs_Single$intellectual_ST_c_R4  + Connect_DirectObs_Single$intellectual_ST_d_R4)  /4
Connect_DirectObs_Single$expressiveness_ST_R4 <- (Connect_DirectObs_Single$expressiveness_ST_a_R4  + Connect_DirectObs_Single$expressiveness_ST_b_R4 + Connect_DirectObs_Single$expressiveness_ST_c_R4  + Connect_DirectObs_Single$expressiveness_ST_d_R4  )  /4
Connect_DirectObs_Single$selfconfidence_ST_R4 <- (Connect_DirectObs_Single$selfconfidence_ST_a_R4  + Connect_DirectObs_Single$selfconfidence_ST_b_R4)  /2
#StressTest R5
Connect_DirectObs_Single$nervousness_ST_R5 <- (Connect_DirectObs_Single$nervousness_ST_a_R5  + Connect_DirectObs_Single$nervousness_ST_b_R5 + Connect_DirectObs_Single$nervousness_ST_c_R5  + Connect_DirectObs_Single$nervousness_ST_d_R5  )  /4
Connect_DirectObs_Single$intellectual_ST_R5 <- (Connect_DirectObs_Single$intellectual_ST_a_R5  + Connect_DirectObs_Single$intellectual_ST_b_R5 + Connect_DirectObs_Single$intellectual_ST_c_R5  + Connect_DirectObs_Single$intellectual_ST_d_R5  )  /4
Connect_DirectObs_Single$expressiveness_ST_R5 <- (Connect_DirectObs_Single$expressiveness_ST_a_R5  + Connect_DirectObs_Single$expressiveness_ST_b_R5 + Connect_DirectObs_Single$expressiveness_ST_c_R5  + Connect_DirectObs_Single$expressiveness_ST_d_R5  )  /4
Connect_DirectObs_Single$selfconfidence_ST_R5 <- (Connect_DirectObs_Single$selfconfidence_ST_a_R5  + Connect_DirectObs_Single$selfconfidence_ST_b_R5)  /2

## ICCs

ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, expressiveness_ST_R1  ,	expressiveness_ST_R2	,	expressiveness_ST_R3	,	expressiveness_ST_R4	,	expressiveness_ST_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, selfconfidence_ST_R1	,	selfconfidence_ST_R2	,	selfconfidence_ST_R3	,	selfconfidence_ST_R4	,	selfconfidence_ST_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, nervousness_ST_R1	,	nervousness_ST_R2	,	nervousness_ST_R3	,	nervousness_ST_R4	,	nervousness_ST_R5))))
ICC(na.omit(as.matrix(select(Connect_DirectObs_Single, intellectual_ST_R1	,	intellectual_ST_R2	,	intellectual_ST_R3	,	intellectual_ST_R4	,	intellectual_ST_R5))))

#PGG Means and SDs
describe(Connect_Economic[,c("pgg1","pgg2")])

#Additional Analysis Table 19
Connect_DirectObs_Agg <- as.data.frame(Connect_DirectObs_Agg)
Connect_DirectObs_Agg$Expressiveness <- rowMeans(Connect_DirectObs_Agg[,c("expressiveness_SI1", "expressiveness_SMT", "expressiveness_SI2", "expressiveness_ST")],na.rm=TRUE)
Connect_DirectObs_Agg$Selfconfidence <- rowMeans(Connect_DirectObs_Agg[,c("selfconfidence_SI1", "selfconfidence_SI2", "selfconfidence_ST")],na.rm=TRUE)
Connect_DirectObs_Agg$Arrogance <- rowMeans(Connect_DirectObs_Agg[,c("arrogance_SI1", "arrogance_SI2", "arrogance_SMT")],na.rm=TRUE)
Connect_DirectObs_Agg$Friendly <- Connect_DirectObs_Agg$friendliness_SI1
Connect_DirectObs_Agg$Warmth <- rowMeans(Connect_DirectObs_Agg[,c("warmth_SMT", "warmth_SI2")],na.rm=TRUE)
Connect_DirectObs_Agg$Nervous <- rowMeans(Connect_DirectObs_Agg[,c("nervousness_SI1", "nervousness_SMT", "nervousness_SI2", "nervousness_ST")],na.rm=TRUE)
Connect_DirectObs_Agg$Int <- Connect_DirectObs_Agg$intellectual_ST

#mean states
Connect_DirectObs_Agg$stateex <- rowMeans(Connect_DirectObs_Agg[,c("Expressiveness", "Selfconfidence")],na.rm=TRUE)
Connect_DirectObs_Agg$statea <- rowMeans(Connect_DirectObs_Agg[,c("Friendly", "Warmth")],na.rm=TRUE)

#meantraits
Connect_Traits_t1$dido_t1 <- rowMeans(Connect_Traits_t1[,c("dido_mach_t1", "dido_narc_t1","dido_psych_t1")],na.rm=TRUE)
Connect_Traits_Acq_Agg$dido_acqu <- rowMeans(Connect_Traits_Acq_Agg[,c("dido_mach_acqu", "dido_narc_acqu","dido_psych_acqu")],na.rm=TRUE)

CorrelationBehavior2 <- merge(select(Connect_DirectObs_Agg,id_a, stateex, statea, Arrogance, Nervous), select(Connect_Traits_t1, id_a, big5_n_t1, big5_e_t1, big5_a_t1, narq_riv_t1, dido_t1), by = "id_a")
CorrelationInt <- merge(select(Connect_DirectObs_Agg,id_a, Int), select(Connect_Cognitive, id_a, raven_total), by = "id_a")
CorrelationBehavior_acq2 <- merge(select(Connect_DirectObs_Agg,id_a, stateex, statea, Arrogance, Nervous), select(Connect_Traits_Acq_Agg, id_a, big5_n_acqu, big5_e_acqu,  big5_a_acqu,  narq_riv_acqu,dido_acqu), by = "id_a")
#values self-reports correlation with behavior
corr.test(CorrelationBehavior2,adjust ="none")
#values acqu-reports correlation with behavior
corr.test(CorrelationBehavior_acq2,adjust ="none")
#Values Intelligence
corr.test(CorrelationInt,adjust ="none")
