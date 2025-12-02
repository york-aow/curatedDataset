#### Check number of unique variable responses #### 

length(unique(as.factor(dataframe$variable))) #replace 'dataframe' and 'variable' with your dataframe and variable name


#### Variable info ####

str(dataframe) #replace 'dataframe' with your dataframe name
head(dataframe)
sapply(dataframe, class) 


##### Merge Survey Module Dataframes ####

library(dplyr)

mergeddataframe <- full_join(dataframe1, dataframe2, #replace with your dataframe names where appropriate
                             by = "aow_recruitment_id",
                             keep = FALSE)

# re: "Note: Data were not all collected at the same time, see differences in age
# (months) between measurements. An example is given below (consult the 
# aow_curated_variables.csv for full list of age related variables).

table(dataframe$age_m_survey, dataframe$age_m_heightweight) #replace 'dataframe' with your dataframe name


#### Summary of scale total scores ####

summary(dataframe$edeqs_total) #replace 'dataframe' with your dataframe name
summary(dataframe$rcads_anx_total)
summary(dataframe$rcads_dep_total)
summary(dataframe$rcads_total)
summary(dataframe$swemwbs_total)
summary(dataframe$sdq_con_total)
summary(dataframe$sdq_emo_total)
summary(dataframe$sdq_hyp_total)
summary(dataframe$sdq_pee_total)
summary(dataframe$sdq_pro_total)
summary(dataframe$sdq_int_total)
summary(dataframe$sdq_ext_total)
summary(dataframe$sdq_dif_total)
summary(dataframe$ucla3_total)
summary(dataframe$ucla4_total)
summary(dataframe$brs_total)
summary(dataframe$yaps_total)


#### birth place table ####

table(dataframe$fam_birth_place, useNA = "ifany") #replace 'dataframe' with your dataframe name


#### ethnicity table ####

table(dataframe$ethnicity, useNA = "ifany") #replace 'dataframe' with your dataframe name


#### languages tables ####

table(dataframe$lang_home, useNA = "ifany") #replace 'dataframe' with your dataframe name
table(dataframe$lang_number, useNA = "ifany")


#### religion table/labels ####

table(dataframe$religion, useNA = "ifany") #replace 'dataframe' with your dataframe name

library(haven)
print_labels(dataframe$activities_religious) # 0 = No, 1 = Yes - once, 2 = Yes - more than once


#### disability table/labels ####

table(dataframe$disability_time, useNA = 'ifany') #replace 'dataframe' with your dataframe name

library(haven)
print_labels(dataframe$disability) # 1 = Yes, 2 = No
print_labels(dataframe$disability_limit) # 1 = Yes a lot, 2 = Yes a little, 3 = Not at all