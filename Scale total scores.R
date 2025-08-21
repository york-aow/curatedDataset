#install.packages("psych")

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

# Reverse code 1-5 Likert scale
recode15_51 <- function(x) {
  case_match(x,
             1 ~ 5,
             2 ~ 4,
             3 ~ 3,
             4 ~ 2,
             5 ~ 1,
             NA ~ NA
  )
}


#### EDE-QS ####

# Create list of items
edeqs_items <-
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
table(both_mods_24_x$awb2_12_eat_hbt_1_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_eat_hbt_2_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_eat_hbt_3_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_eat_hbt_4_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_eat_hbt_5_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_eat_hbt_6_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_eat_hbt_7_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_eat_hbt_8_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_eat_hbt_9_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_eat_hbt_10_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_wght_1_a5, useNA = "ifany")
table(both_mods_24_x$awb2_12_wght_2_a5, useNA = "ifany")

# Recode item responses from 1-4 to 0-3
both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(edeqs_items), recode14_03))

# Check new item response frequencies and NA's against old (above)
for(item in edeqs_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}



# How many responded 0-days to Q9 (and therefore skipped Q10)?
table(as_factor(both_mods_24_x$awb2_12_eat_hbt_9_a5), useNA = "ifany")
table(as_factor(both_mods_24_x$awb2_12_eat_hbt_10_a5), useNA = "ifany")

# For those that responded 0 for item 9, impute 0 for item 10

both_mods_24_x <- both_mods_24_x %>%
  mutate(awb2_12_eat_hbt_10_a5 = ifelse(awb2_12_eat_hbt_9_a5 == 0, 0, awb2_12_eat_hbt_10_a5))

table(as_factor(both_mods_24_x$awb2_12_eat_hbt_9_a5), useNA = "ifany")
table(as_factor(both_mods_24_x$awb2_12_eat_hbt_10_a5), useNA = "ifany")
table(as_factor(both_mods_24_x$awb2_12_wght_1_a5), useNA = "ifany")

table(as.factor(both_mods_24_x$awb2_12_eat_hbt_9_a5),
      as.factor(both_mods_24_x$awb2_12_eat_hbt_10_a5), useNA = "ifany")

# Identify complete EDE-QS responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(edeqs_complete = rowSums(is.na(select(., !!!edeqs_items))) == 0) # !!! removes quote marks from the list items

table(both_mods_24_x$edeqs_complete, useNA = "ifany")
6411+1144 # 7555
6411/7555 # 84.9% complete / 15% missing

# Compute total scores for those with complete EDE-QS, and assign NA to those without complete edeqs
both_mods_24_x <- both_mods_24_x %>%
  mutate(edeqs_total = ifelse(edeqs_complete, rowSums(select(., !!!edeqs_items), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$edeqs_total)
table(as_factor(both_mods_24_x$edeqs_total), both_mods_24_x$edeqs_complete, useNA = "ifany")

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(edeqs_nas = rowSums(is.na(select(., !!!edeqs_items))))

table(as_factor(both_mods_24_x$edeqs_nas), useNA = "ifany")
6411+1144#7555
table(as_factor(both_mods_24_x$edeqs_complete), as_factor(both_mods_24_x$edeqs_nas), useNA = "ifany")


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

rcads_items <- c(rcads_anx_items, rcads_dep_items)

# Check item responses
for(item in rcads_anx_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}
for(item in rcads_dep_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}

# Recode Likert scale from 1-4 to 03
both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(rcads_anx_items), recode14_03))

both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(rcads_dep_items), recode14_03))

# Check item statistics again (above)
for(item in rcads_anx_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}
for(item in rcads_dep_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}

# Identify complete responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_dep_complete = rowSums(is.na(select(., !!!rcads_dep_items))) == 0) # !!! removes quote marks from the list items

both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_anx_complete = rowSums(is.na(select(., !!!rcads_anx_items))) == 0) # !!! removes quote marks from the list items

table(both_mods_24_x$rcads_dep_complete, useNA = "ifany")
table(both_mods_24_x$rcads_anx_complete, useNA = "ifany")
table(both_mods_24_x$rcads_dep_complete, both_mods_24_x$rcads_anx_complete, useNA = "ifany")

# Compute total scores for those with complete RCADS subscale, and assign NA to those with incomplete
both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_dep_total = ifelse(rcads_dep_complete, rowSums(select(., !!!rcads_dep_items), na.rm = TRUE), NA))

both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_anx_total = ifelse(rcads_anx_complete, rowSums(select(., !!!rcads_anx_items), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$rcads_dep_total)
table(as_factor(both_mods_24_x$rcads_dep_total), useNA = "ifany")
table(both_mods_24_x$rcads_dep_complete, as_factor(both_mods_24_x$rcads_dep_total), useNA = "ifany")

summary(both_mods_24_x$rcads_anx_total)
table(as_factor(both_mods_24_x$rcads_anx_total), useNA = "ifany")
table(both_mods_24_x$rcads_anx_complete, as_factor(both_mods_24_x$rcads_anx_total), useNA = "ifany")

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_dep_nas = rowSums(is.na(select(., !!!rcads_dep_items))))

table(as_factor(both_mods_24_x$rcads_dep_nas), useNA = "ifany")
table(as_factor(both_mods_24_x$rcads_dep_nas), as_factor(both_mods_24_x$rcads_dep_complete), useNA = "ifany")

both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_anx_nas = rowSums(is.na(select(., !!!rcads_anx_items))))

table(as_factor(both_mods_24_x$rcads_anx_nas), useNA = "ifany")
table(as_factor(both_mods_24_x$rcads_anx_nas), as_factor(both_mods_24_x$rcads_anx_complete), useNA = "ifany")

# Create RCADS scale total score (sum depression and anxiety, leave NA for any missing either subscale)
both_mods_24_x$rcads_total <- 
  ifelse(is.na(both_mods_24_x$rcads_anx_total) | is.na(both_mods_24_x$rcads_dep_total), NA, 
         both_mods_24_x$rcads_anx_total + both_mods_24_x$rcads_dep_total)

summary(both_mods_24_x$rcads_total)
summary(both_mods_24_x$rcads_dep_total)
summary(both_mods_24_x$rcads_anx_total)
table(as_factor(both_mods_24_x$rcads_anx_complete), as_factor(both_mods_24_x$rcads_total), useNA = "ifany")
table(as_factor(both_mods_24_x$rcads_dep_complete), as_factor(both_mods_24_x$rcads_total), useNA = "ifany")


#### SWEMWBS ####

# Create a list of SWEMWBS items
swemwbs_items <- c("awb2_2_optmstc_1_a4",
                   "awb2_2_useful_2_a4",
                   "awb2_2_relxed_3_a4",
                   "awb2_2_problems_4_a4",
                   "awb2_2_think_clr_5_a4",
                   "awb2_2_close_othrs_6_a4",
                   "awb2_2_own_mnd_7_a4")

# Check item responses
table(both_mods_24_x$awb2_2_optmstc_1_a4, useNA = "ifany")
for(item in swemwbs_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}

# Identify complete SWEMWBS responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(swemwbs_complete = rowSums(is.na(select(., !!!swemwbs_items))) == 0)

table(both_mods_24_x$swemwbs_complete)

# Compute total scores for those with complete SWEMWBS, and assign NA to those without
both_mods_24_x <- both_mods_24_x %>%
  mutate(swemwbs_total = ifelse(swemwbs_complete, rowSums(select(., !!!swemwbs_items), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$swemwbs_total)
table(as_factor(both_mods_24_x$swemwbs_total), useNA = "ifany")
table(both_mods_24_x$swemwbs_complete, as_factor(both_mods_24_x$swemwbs_total), useNA = "ifany")

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(swemwbs_nas = rowSums(is.na(select(., !!!swemwbs_items))))

table(as_factor(both_mods_24_x$swemwbs_nas))
table(as_factor(both_mods_24_x$swemwbs_nas), as_factor(both_mods_24_x$swemwbs_complete), useNA = "ifany")


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

sdq_items <- c(sdq_emo_items, sdq_con_items, sdq_hyp_items, sdq_peer_items, sdq_pro_items)

sdq_int_items <- c(sdq_emo_items, sdq_peer_items)

sdq_ext_items <- c(sdq_con_items, sdq_hyp_items) 

sdq_dif_items <- c(sdq_int_items, sdq_ext_items)

sdq_reverse_items <- c("awb2_1_sdq_7_a10", # This item is reverse coded
                       "awb2_1_sdq_21_a10", # This item is reverse coded
                       "awb2_1_sdq_25_a10", # This item is reverse coded
                       "awb2_1_sdq_11_a10", # This item is reverse coded
                       "awb2_1_sdq_14_a10") # This item is reverse coded

# Check item responses
for(item in sdq_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}

# Check inter-tem correlations to make sure reverse coded items haven't already been reverse coded
cor(both_mods_24_x[sdq_items], use = "pairwise.complete.obs")

# Recode reverse scored item responses from 0-2 to 2-0
both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(sdq_reverse_items), recode02_20))

# Identify complete subscale responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(sdq_con_complete = rowSums(is.na(select(., !!!sdq_con_items))) == 0)

both_mods_24_x <- both_mods_24_x %>%
  mutate(sdq_emo_complete = rowSums(is.na(select(., !!!sdq_emo_items))) == 0)

both_mods_24_x <- both_mods_24_x %>%
  mutate(sdq_hyp_complete = rowSums(is.na(select(., !!!sdq_hyp_items))) == 0)

both_mods_24_x <- both_mods_24_x %>%
  mutate(sdq_peer_complete = rowSums(is.na(select(., !!!sdq_peer_items))) == 0)

both_mods_24_x <- both_mods_24_x %>%
  mutate(sdq_pro_complete = rowSums(is.na(select(., !!!sdq_pro_items))) == 0)

table(both_mods_24_x$sdq_con_complete)
table(both_mods_24_x$sdq_emo_complete)
table(both_mods_24_x$sdq_hyp_complete)
table(both_mods_24_x$sdq_peer_complete)
table(both_mods_24_x$sdq_pro_complete)


# Compute subscale total scores for those with complete subscale, and assign NA to those without
both_mods_24_x <- both_mods_24_x %>%
  mutate(sdq_con_total = ifelse(sdq_con_complete, rowSums(select(., !!!sdq_con_items), na.rm = TRUE), NA)) %>%
  mutate(sdq_emo_total = ifelse(sdq_emo_complete, rowSums(select(., !!!sdq_emo_items), na.rm = TRUE), NA)) %>%
  mutate(sdq_hyp_total = ifelse(sdq_hyp_complete, rowSums(select(., !!!sdq_hyp_items), na.rm = TRUE), NA)) %>%
  mutate(sdq_peer_total = ifelse(sdq_peer_complete, rowSums(select(., !!!sdq_peer_items), na.rm = TRUE), NA)) %>%
  mutate(sdq_pro_total = ifelse(sdq_pro_complete, rowSums(select(., !!!sdq_pro_items), na.rm = TRUE), NA))

# Sense check
table(both_mods_24_x$sdq_con_complete, as_factor(both_mods_24_x$sdq_con_total), useNA = "ifany")
table(both_mods_24_x$sdq_emo_complete, as_factor(both_mods_24_x$sdq_emo_total), useNA = "ifany")
table(both_mods_24_x$sdq_hyp_complete, as_factor(both_mods_24_x$sdq_hyp_total), useNA = "ifany")
table(both_mods_24_x$sdq_peer_complete, as_factor(both_mods_24_x$sdq_peer_total), useNA = "ifany")
table(both_mods_24_x$sdq_pro_complete, as_factor(both_mods_24_x$sdq_pro_total), useNA = "ifany")

# Compute internalising and externalising scores

both_mods_24_x$sdq_ext_total <- 
  ifelse(is.na(both_mods_24_x$sdq_con_total) | is.na(both_mods_24_x$sdq_hyp_total), NA, 
         both_mods_24_x$sdq_con_total + both_mods_24_x$sdq_hyp_total)

summary(both_mods_24_x$sdq_ext_total)
table(both_mods_24_x$sdq_con_complete, both_mods_24_x$sdq_ext_total, useNA = "ifany")
table(both_mods_24_x$sdq_hyp_complete, both_mods_24_x$sdq_ext_total, useNA = "ifany")

both_mods_24_x$sdq_int_total <- 
  ifelse(is.na(both_mods_24_x$sdq_emo_total) | is.na(both_mods_24_x$sdq_peer_total), NA, 
         both_mods_24_x$sdq_emo_total + both_mods_24_x$sdq_peer_total)

summary(both_mods_24_x$sdq_int_total)
table(both_mods_24_x$sdq_emo_complete, both_mods_24_x$sdq_int_total, useNA = "ifany")
table(both_mods_24_x$sdq_peer_complete, both_mods_24_x$sdq_int_total, useNA = "ifany")

# Compute total difficulties 

both_mods_24_x$sdq_dif_total <- 
  ifelse(is.na(both_mods_24_x$sdq_ext_total) | is.na(both_mods_24_x$sdq_int_total), NA, 
         both_mods_24_x$sdq_ext_total + both_mods_24_x$sdq_int_total)

summary(both_mods_24_x$sdq_dif_total)
table(both_mods_24_x$sdq_int_total, both_mods_24_x$sdq_ext_total, useNA = "ifany")
# Nobody scored 20 on both internalising and externalising - hence max 35(/40)
table(both_mods_24_x$sdq_dif_total, useNA = "ifany")


#### UCLA-3 ####

ucla3_items <- c("awb2_4_loneliness_1",
                   "awb2_4_loneliness_2",
                   "awb2_4_loneliness_3")

# Check item responses
for(item in ucla3_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}

# Identify complete UCLA-3 responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(ucla3_complete = rowSums(is.na(select(., !!!ucla3_items))) == 0)

table(both_mods_24_x$ucla3_complete)

# Compute total scores for those with complete UCLA-3, and assign NA to those without
both_mods_24_x <- both_mods_24_x %>%
  mutate(ucla3_total = ifelse(ucla3_complete, rowSums(select(., !!!ucla3_items), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$ucla3_total)
table(as_factor(both_mods_24_x$ucla3_total), useNA = "ifany")
table(both_mods_24_x$ucla3_complete, as_factor(both_mods_24_x$ucla3_total), useNA = "ifany")

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(ucla3_nas = rowSums(is.na(select(., !!!ucla3_items))))

table(as_factor(both_mods_24_x$ucla3_nas))
table(as_factor(both_mods_24_x$ucla3_nas), as_factor(both_mods_24_x$ucla3_complete), useNA = "ifany")


#### UCLA-4 ####

ucla4_items <- c("awb2_4_loneliness_1",
                 "awb2_4_loneliness_2",
                 "awb2_4_loneliness_3",
                 "awb2_4_loneliness_4")

# Check item responses
for(item in ucla4_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}

# Identify complete UCLA-4 responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(ucla4_complete = rowSums(is.na(select(., !!!ucla4_items))) == 0)

table(both_mods_24_x$ucla4_complete)

# Compute total scores for those with complete UCLA-4, and assign NA to those without
both_mods_24_x <- both_mods_24_x %>%
  mutate(ucla4_total = ifelse(ucla4_complete, rowSums(select(., !!!ucla4_items), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$ucla4_total)
table(as_factor(both_mods_24_x$ucla4_total), useNA = "ifany")
table(both_mods_24_x$ucla4_complete, as_factor(both_mods_24_x$ucla4_total), useNA = "ifany")

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(ucla4_nas = rowSums(is.na(select(., !!!ucla4_items))))

table(as_factor(both_mods_24_x$ucla4_nas))
table(as_factor(both_mods_24_x$ucla4_nas), as_factor(both_mods_24_x$ucla4_complete), useNA = "ifany")


#### BRS ####

brs_items <- c("awb2_9_resil1_a5",
               "awb2_9_resil2_a5", # This item is reverse coded
               "awb2_9_resil3_a5",
               "awb2_9_resil4_a5", # This item is reverse coded
               "awb2_9_resil5_a5",
               "awb2_9_resil6_a5") # This item is reverse coded

brs_reverse_items <- c("awb2_9_resil2_a5", # This item is reverse coded
                       "awb2_9_resil4_a5", # This item is reverse coded
                       "awb2_9_resil6_a5") # This item is reverse coded

# Check item responses
for(item in brs_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}

# Check inter-tem correlations to make sure reverse coded items haven't already been reverse coded
cor(both_mods_24_x[brs_items], use = "pairwise.complete.obs")

# Recode reverse scored item responses from 0-2 to 2-0
both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(brs_reverse_items), recode15_51))

# Identify complete BRS responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(brs_complete = rowSums(is.na(select(., !!!brs_items))) == 0)

table(both_mods_24_x$brs_complete)

# Compute total scores for those with complete BRS, and assign NA to those without
both_mods_24_x <- both_mods_24_x %>%
  mutate(brs_total = ifelse(brs_complete, rowSums(select(., !!!brs_items), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$brs_total)
table(as_factor(both_mods_24_x$brs_total), useNA = "ifany")
table(both_mods_24_x$brs_complete, as_factor(both_mods_24_x$brs_total), useNA = "ifany")

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(brs_nas = rowSums(is.na(select(., !!!brs_items))))

table(as_factor(both_mods_24_x$brs_nas))
table(as_factor(both_mods_24_x$brs_nas), as_factor(both_mods_24_x$brs_complete), useNA = "ifany")


#### YAP-S ####

yaps_items <- c("awb4_2_outside_schl_1_r7",
                "awb4_2_outside_schl_2_r7",
                "awb4_2_outside_schl_3_r7",
                "awb4_2_outside_schl_4_r7",
                "awb4_2_overall_a5")

# Check item responses
for(item in yaps_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}

# Identify complete YAP-S responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(yaps_complete = rowSums(is.na(select(., !!!yaps_items))) == 0)

table(both_mods_24_x$yaps_complete)

# Compute total scores for those with complete YAP-S, and assign NA to those without
both_mods_24_x <- both_mods_24_x %>%
  mutate(yaps_total = ifelse(yaps_complete, rowSums(select(., !!!yaps_items), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$yaps_total)
table(as_factor(both_mods_24_x$yaps_total), useNA = "ifany")
table(both_mods_24_x$yaps_complete, as_factor(both_mods_24_x$yaps_total), useNA = "ifany")

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(yaps_nas = rowSums(is.na(select(., !!!yaps_items))))

table(as_factor(both_mods_24_x$yaps_nas))
table(as_factor(both_mods_24_x$yaps_nas), as_factor(both_mods_24_x$yaps_complete), useNA = "ifany")


#### GHSQ ####

ghsq_items <- c("awb2_9_seek_hlp_ppl_1_r4",
                "awb2_9_seek_hlp_ppl_2",
                "awb2_9_seek_hlp_ppl_3",
                "awb2_9_seek_hlp_ppl_4",
                "awb2_9_seek_hlp_ppl_5",
                "awb2_9_seek_hlp_ppl_6",
                "awb2_9_seek_hlp_ppl_7",
                "awb2_9_seek_hlp_ppl_8",
                "awb2_9_seek_hlp_ppl_10",  # This item is reverse coded
                "awb2_9_seek_hlp_ppl_othr_a3")

# Check item responses
for(item in ghsq_items) {print(table(both_mods_24_x[[item]], useNA = "ifany"))}
# The last item has been binarised...


#### Reliability ####

# Inter-item correlations
cor(both_mods_24_x[edeqs_items], use = "pairwise.complete.obs")
cor(both_mods_24_x[rcads_dep_items], use = "pairwise.complete.obs")
cor(both_mods_24_x[rcads_anx_items], use = "pairwise.complete.obs")
cor(both_mods_24_x[yaps_items], use = "pairwise.complete.obs")

# Correlation between subscales
cor(both_mods_24_x$rcads_dep_total, both_mods_24_x$rcads_anx_total, use = "pairwise.complete.obs")
print(ucla4_items)

# Item reliability
# Under "Item statistics" check raw.r (item-total correlation) and mean (item mean) - for prorating
alpha(both_mods_24_x[sdq_con_items])
alpha(both_mods_24_x[sdq_emo_items])
alpha(both_mods_24_x[sdq_hyp_items])
alpha(both_mods_24_x[sdq_peer_items])
alpha(both_mods_24_x[sdq_pro_items])
alpha(both_mods_24_x[sdq_int_items])
alpha(both_mods_24_x[sdq_ext_items])
alpha(both_mods_24_x[sdq_dif_items])

alpha(both_mods_24_x[brs_items])

alpha(both_mods_24_x[edeqs_items])

alpha(both_mods_24_x[rcads_dep_items])
alpha(both_mods_24_x[rcads_anx_items])
alpha(both_mods_24_x[rcads_items])

alpha(both_mods_24_x[swemwbs_items])

alpha(both_mods_24_x[ucla3_items])
alpha(both_mods_24_x[ucla4_items])

alpha(both_mods_24_x[yaps_items])


#### Pro-rate ####
https://search.r-project.org/CRAN/refmans/misty/html/item.scores.html
install.packages("misty")
library(misty)

both_mods_24_x$sdq_emo_prorated <- item.scores(both_mods_24_x[, sdq_emo_items], 
                                               fun = "sum", # Function = multiply mean of available items by total number of items
                                               n.avail = 3) # Minimum number of available item responses

summary(both_mods_24_x$sdq_emo_prorated)
summary(both_mods_24_x$sdq_emo_total)



#### Rename variables ####