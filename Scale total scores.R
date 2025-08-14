install.packages("psych")

library(psych)

#### Create recode functions ####

# Recode Likert scale 1-4 to 0-3
recode14_03 <- function(x) {
  case_match(x,
             1 ~ 0,
             2 ~ 1,
             3 ~ 2,
             4 ~ 3,
             NA ~ NA
  )
}

# Reverse code 0-2 Likert scale
recode02_20 <- function(x) {
  case_match(x,
             0 ~ 2,
             1 ~ 1,
             2 ~ 0,
             NA ~ NA
  )
}



#### EDE-QS ####

# Create list of items
EDEQS_items <-
  c("awb2_12_eat_hbt_1_a5",
    "awb2_12_eat_hbt_2_a5",
    "awb2_12_eat_hbt_3_a5",
    "awb2_12_eat_hbt_4_a5",
    "awb2_12_eat_hbt_5_a5",
    "awb2_12_eat_hbt_6_a5",
    "awb2_12_eat_hbt_7_a5",
    "awb2_12_eat_hbt_8_a5",
    "awb2_12_eat_hbt_9_a5",
    "awb2_12_eat_hbt_10_a5",
    "awb2_12_wght_1_a5",
    "awb2_12_wght_2_a5")

# Check item responses
table(both_mods_24_x$awb2_12_eat_hbt_1_a5)
table(both_mods_24_x$awb2_12_eat_hbt_2_a5)
table(both_mods_24_x$awb2_12_eat_hbt_3_a5)
table(both_mods_24_x$awb2_12_eat_hbt_4_a5)
table(both_mods_24_x$awb2_12_eat_hbt_5_a5)
table(both_mods_24_x$awb2_12_eat_hbt_6_a5)
table(both_mods_24_x$awb2_12_eat_hbt_7_a5)
table(both_mods_24_x$awb2_12_eat_hbt_8_a5)
table(both_mods_24_x$awb2_12_eat_hbt_9_a5)
table(both_mods_24_x$awb2_12_eat_hbt_10_a5)
table(both_mods_24_x$awb2_12_wght_1_a5)
table(both_mods_24_x$awb2_12_wght_2_a5)

summary(both_mods_24_x$awb2_12_eat_hbt_1_a5)
summary(both_mods_24_x$awb2_12_eat_hbt_2_a5)
summary(both_mods_24_x$awb2_12_eat_hbt_3_a5)
summary(both_mods_24_x$awb2_12_eat_hbt_4_a5)
summary(both_mods_24_x$awb2_12_eat_hbt_5_a5)
summary(both_mods_24_x$awb2_12_eat_hbt_6_a5)
summary(both_mods_24_x$awb2_12_eat_hbt_7_a5)
summary(both_mods_24_x$awb2_12_eat_hbt_8_a5)
summary(both_mods_24_x$awb2_12_eat_hbt_9_a5)
summary(both_mods_24_x$awb2_12_eat_hbt_10_a5)
summary(both_mods_24_x$awb2_12_wght_1_a5)
summary(both_mods_24_x$awb2_12_wght_2_a5)

# Recode item responses from 1-4 to 0-3
both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(EDEQS_items), recode14_03))

# Check new item response frequencies and NA's against old (above)
for(item in both_mods_24_x[EDEQS_items]){print(summary(item))}
for(item in both_mods_24_x[EDEQS_items]){print(table(item))}

# How many responded 0-days to Q9 (and therefore skipped Q10)?
table(as_factor(both_mods_24_x$awb2_12_eat_hbt_9_a5))
sum(is.na(both_mods_24_x$awb2_12_eat_hbt_9_a5))
table(as_factor(both_mods_24_x$awb2_12_eat_hbt_10_a5))
sum(is.na(both_mods_24_x$awb2_12_eat_hbt_10_a5))

# For those that responded 0 for item 9, impute 0 for item 10

both_mods_24_x <- both_mods_24_x %>%
  mutate(awb2_12_eat_hbt_10_a5 = ifelse(awb2_12_eat_hbt_9_a5 == 0, 0, awb2_12_eat_hbt_10_a5))

sum(is.na(both_mods_24_x$awb2_12_eat_hbt_9_a5))
sum(is.na(both_mods_24_x$awb2_12_eat_hbt_10_a5))
sum(is.na(both_mods_24_x$awb2_12_wght_1_a5))

table(as.factor(both_mods_24_x$awb2_12_eat_hbt_9_a5),
      as.factor(both_mods_24_x$awb2_12_eat_hbt_10_a5))

# Identify complete EDE-QS responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(EDEQS_complete = rowSums(is.na(select(., !!!EDEQS_items))) == 0) # !!! removes quote marks from the list items

table(both_mods_24_x$EDEQS_complete)
6411+1144 # 7555
6411/7555 # 84.9% complete / 15% missing

# Compute total scores for those with complete EDEQS, and assign NA to those without complete EDEQS
both_mods_24_x <- both_mods_24_x %>%
  mutate(EDEQS_total = ifelse(EDEQS_complete, rowSums(select(., !!!EDEQS_items), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$EDEQS_total)
table(as_factor(both_mods_24_x$EDEQS_total))
table(as_factor(both_mods_24_x$EDEQS_total), both_mods_24_x$EDEQS_complete)

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(EDEQS_nas = rowSums(is.na(select(., !!!EDEQS_items))))

table(as_factor(both_mods_24_x$EDEQS_nas))
6411+1144#7555
table(as_factor(both_mods_24_x$EDEQS_nas), as_factor(both_mods_24_x$EDEQS_complete))

# Inter-item correlations
cor(both_mods_24_x[EDEQS_items], use = "pairwise.complete.obs")

# Internal reliability
alpha(both_mods_24_x[EDEQS_items])


#### RCADS ####

# Create list of depression items
rcads_dep_items <- c("awb2_1_illhealth_1",
                  "awb2_1_illhealth_4",
                  "awb2_1_illhealth_8",
                  "awb2_1_illhealth_10",
                  "awb2_1_illhealth_13",
                  "awb2_1_illhealth_15",
                  "awb2_1_illhealth_16",
                  "awb2_1_illhealth_19",
                  "awb2_1_illhealth_21",
                  "awb2_1_illhealth_24")

# Create list of anxiety items
rcads_anx_items <- c("awb2_1_illhealth_2",
                     "awb2_1_illhealth_3",
                     "awb2_1_illhealth_5",
                     "awb2_1_illhealth_6",
                     "awb2_1_illhealth_7",
                     "awb2_1_illhealth_9",
                     "awb2_1_illhealth_11",
                     "awb2_1_illhealth_12",
                     "awb2_1_illhealth_14",
                     "awb2_1_illhealth_17",
                     "awb2_1_illhealth_18",
                     "awb2_1_illhealth_20",
                     "awb2_1_illhealth_22",
                     "awb2_1_illhealth_23",
                     "awb2_1_illhealth_25")

# Check item statistcs
for(item in both_mods_24_x[rcads_anx_items]){print(summary(item))}
for(item in both_mods_24_x[rcads_anx_items]){print(table(item))}

for(item in both_mods_24_x[rcads_dep_items]){print(summary(item))}
for(item in both_mods_24_x[rcads_dep_items]){print(table(item))}

# Recode Likert scale from 1-4 to 03
both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(rcads_anx_items), recode14_03))

both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(rcads_dep_items), recode14_03))

# Check item statistics again (above)

# Identify complete responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_dep_complete = rowSums(is.na(select(., !!!rcads_dep_items))) == 0) # !!! removes quote marks from the list items

both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_anx_complete = rowSums(is.na(select(., !!!rcads_anx_items))) == 0) # !!! removes quote marks from the list items

table(both_mods_24_x$rcads_dep_complete)
table(both_mods_24_x$rcads_anx_complete)
table(both_mods_24_x$rcads_dep_complete, both_mods_24_x$rcads_anx_complete)

# Compute total scores for those with complete RCADS subscale, and assign NA to those with incomplete
both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_dep_total = ifelse(rcads_dep_complete, rowSums(select(., !!!rcads_dep_items), na.rm = TRUE), NA))

both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_anx_total = ifelse(rcads_anx_complete, rowSums(select(., !!!rcads_anx_items), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$rcads_dep_total)
table(as_factor(both_mods_24_x$rcads_dep_total))
table(as_factor(both_mods_24_x$rcads_dep_total), both_mods_24_x$rcads_dep_complete)

summary(both_mods_24_x$rcads_anx_total)
table(as_factor(both_mods_24_x$rcads_anx_total))
table(as_factor(both_mods_24_x$rcads_anx_total), both_mods_24_x$rcads_anx_complete)

# Inter-item correlations
cor(both_mods_24_x[rcads_dep_items], use = "pairwise.complete.obs")
cor(both_mods_24_x[rcads_anx_items], use = "pairwise.complete.obs")

# Internal reliability
alpha(both_mods_24_x[rcads_dep_items])
alpha(both_mods_24_x[rcads_anx_items])

# Correlation between subscales
cor(both_mods_24_x$rcads_dep_total, both_mods_24_x$rcads_anx_total, use = "pairwise.complete.obs")

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_dep_nas = rowSums(is.na(select(., !!!rcads_dep_items))))

table(as_factor(both_mods_24_x$rcads_dep_nas))
table(as_factor(both_mods_24_x$rcads_dep_nas), as_factor(both_mods_24_x$rcads_dep_complete))

both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_anx_nas = rowSums(is.na(select(., !!!rcads_anx_items))))

table(as_factor(both_mods_24_x$rcads_anx_nas))
table(as_factor(both_mods_24_x$rcads_anx_nas), as_factor(both_mods_24_x$rcads_anx_complete))

# Create RCADS scale total score (sum depression and anxiety, leave NA for any missing either subscale)
both_mods_24_x$rcads_total <- 
  ifelse(is.na(both_mods_24_x$rcads_anx_total) | is.na(both_mods_24_x$rcads_dep_total), NA, 
         both_mods_24_x$rcads_anx_total + both_mods_24_x$rcads_dep_total)

summary(both_mods_24_x$rcads_total)
summary(both_mods_24_x$rcads_dep_total)
summary(both_mods_24_x$rcads_anx_total)
table(as_factor(both_mods_24_x$rcads_total), as_factor(both_mods_24_x$rcads_anx_complete))
table(as_factor(both_mods_24_x$rcads_total), as_factor(both_mods_24_x$rcads_dep_complete))


#### SWEMWBS ####

# Create a list of SWEMWBS items
swemwbs_items <- c("awb2_2_optmstc_1_a4",
                   "awb2_2_useful_2_a4",
                   "awb2_2_relxed_3_a4",
                   "awb2_2_problems_4_a4",
                   "awb2_2_think_clr_5_a4",
                   "awb2_2_close_othrs_6_a4",
                   "awb2_2_own_mnd_7_a4")

# Check item statistcs
summary(both_mods_24_x$awb2_2_optmstc_1_a4)
for(item in both_mods_24_x[swemwbs_items]){print(summary(item))}
table(both_mods_24_x$awb2_2_optmstc_1_a4)
for(item in both_mods_24_x[swemwbs_items]){print(table(item))}

# Identify complete SWEMWBS responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(swemwbs_complete = rowSums(is.na(select(., !!!swemwbs_items))) == 0)

table(both_mods_24_x$swemwbs_complete)

# Compute total scores for those with complete SWEMWBS, and assign NA to those without complete EDEQS
both_mods_24_x <- both_mods_24_x %>%
  mutate(swemwbs_total = ifelse(swemwbs_complete, rowSums(select(., !!!swemwbs_items), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$swemwbs_total)
table(as_factor(both_mods_24_x$swemwbs_total))
table(as_factor(both_mods_24_x$swemwbs_total), both_mods_24_x$swemwbs_complete)

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(swemwbs_nas = rowSums(is.na(select(., !!!swemwbs_items))))

table(as_factor(both_mods_24_x$swemwbs_nas))
table(as_factor(both_mods_24_x$swemwbs_nas), as_factor(both_mods_24_x$swemwbs_complete))


#### SDQ ####

# List items

sdq_emo_items <- c("awb2_1_sdq_3_a10",
                   "awb2_1_sdq_8_a10",
                   "awb2_1_sdq_13_a10",
                   "awb2_1_sdq_16_a10",
                   "awb2_1_sdq_24_a10")

sdq_con_items <- c("awb2_1_sdq_5_a10",
                   "awb2_1_sdq_7_a10", # This item is reverse coded
                   "awb2_1_sdq_12_a10",
                   "awb2_1_sdq_18_a10",
                   "awb2_1_sdq_22_a10")

sdq_hyp_items <- c("awb2_1_sdq_2_a10",
                   "awb2_1_sdq_10_a10", 
                   "awb2_1_sdq_15_a10",
                   "awb2_1_sdq_21_a10", # This item is reverse coded
                   "awb2_1_sdq_25_a10") # This item is reverse coded

sdq_peer_items <- c("awb2_1_sdq_6_a10",
                    "awb2_1_sdq_11_a10", # This item is reverse coded
                    "awb2_1_sdq_14_a10", # This item is reverse coded
                    "awb2_1_sdq_19_a10", 
                    "awb2_1_sdq_23_a10")

sdq_pro_items <- c("awb2_1_sdq_1_a10",
                   "awb2_1_sdq_4_a10", 
                   "awb2_1_sdq_9_a10", 
                   "awb2_1_sdq_17_a10", 
                   "awb2_1_sdq_20_a10")

sdq_items <- c("awb2_1_sdq_3_a10",
               "awb2_1_sdq_8_a10",
               "awb2_1_sdq_13_a10",
               "awb2_1_sdq_16_a10",
               "awb2_1_sdq_24_a10",
               "awb2_1_sdq_5_a10",
               "awb2_1_sdq_7_a10", # This item is reverse coded
               "awb2_1_sdq_12_a10",
               "awb2_1_sdq_18_a10",
               "awb2_1_sdq_22_a10",
               "awb2_1_sdq_2_a10",
               "awb2_1_sdq_10_a10", 
               "awb2_1_sdq_15_a10",
               "awb2_1_sdq_21_a10", # This item is reverse coded
               "awb2_1_sdq_25_a10", # This item is reverse coded
               "awb2_1_sdq_6_a10",
               "awb2_1_sdq_11_a10", # This item is reverse coded
               "awb2_1_sdq_14_a10", # This item is reverse coded
               "awb2_1_sdq_19_a10", 
               "awb2_1_sdq_23_a10",
               "awb2_1_sdq_1_a10",
               "awb2_1_sdq_4_a10", 
               "awb2_1_sdq_9_a10", 
               "awb2_1_sdq_17_a10", 
               "awb2_1_sdq_20_a10") 

sdq_reverse_items <- c("awb2_1_sdq_7_a10", # This item is reverse coded
                       "awb2_1_sdq_21_a10", # This item is reverse coded
                       "awb2_1_sdq_25_a10", # This item is reverse coded
                       "awb2_1_sdq_11_a10", # This item is reverse coded
                       "awb2_1_sdq_14_a10") # This item is reverse coded

# Check item statistcs
summary(both_mods_24_x$awb2_1_sdq_3_a10)
for(item in both_mods_24_x[sdq_items]){print(summary(item))}
for(item in both_mods_24_x[sdq_items]){print(table(item))}

# Check inter-tem correlations to make sure reverse coded items haven't already been reverse coded
cor(both_mods_24_x[sdq_items], use = "pairwise.complete.obs")

# Recode reverse scored item responses from 0-2 to 2-0
both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(sdq_reverse_items), recode02_20))












#### END ####