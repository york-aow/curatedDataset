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
rcads_items_dep <- c("awb2_1_illhealth_1",
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
rcads_items_anx <- c("awb2_1_illhealth_2",
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
for(item in both_mods_24_x[rcads_items_anx]){print(summary(item))}
for(item in both_mods_24_x[rcads_items_anx]){print(table(item))}

for(item in both_mods_24_x[rcads_items_dep]){print(summary(item))}
for(item in both_mods_24_x[rcads_items_dep]){print(table(item))}

# Recode Likert scale from 1-4 to 03
both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(rcads_items_anx), recode14_03))

both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(rcads_items_dep), recode14_03))

# Check item statistics again (above)

# Identify complete responses
both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_complete_dep = rowSums(is.na(select(., !!!rcads_items_dep))) == 0) # !!! removes quote marks from the list items

both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_complete_anx = rowSums(is.na(select(., !!!rcads_items_anx))) == 0) # !!! removes quote marks from the list items

table(both_mods_24_x$rcads_complete_dep)
table(both_mods_24_x$rcads_complete_anx)
table(both_mods_24_x$rcads_complete_dep, both_mods_24_x$rcads_complete_anx)

# Compute total scores for those with complete RCADS subscale, and assign NA to those with incomplete
both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_dep_total = ifelse(rcads_complete_dep, rowSums(select(., !!!rcads_items_dep), na.rm = TRUE), NA))

both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_anx_total = ifelse(rcads_complete_anx, rowSums(select(., !!!rcads_items_anx), na.rm = TRUE), NA))

# Sense check
summary(both_mods_24_x$rcads_dep_total)
table(as_factor(both_mods_24_x$rcads_dep_total))
table(as_factor(both_mods_24_x$rcads_dep_total), both_mods_24_x$rcads_complete_dep)

summary(both_mods_24_x$rcads_anx_total)
table(as_factor(both_mods_24_x$rcads_anx_total))
table(as_factor(both_mods_24_x$rcads_anx_total), both_mods_24_x$rcads_complete_anx)

# Inter-item correlations
cor(both_mods_24_x[rcads_items_dep], use = "pairwise.complete.obs")
cor(both_mods_24_x[rcads_items_anx], use = "pairwise.complete.obs")

# Internal reliability
alpha(both_mods_24_x[rcads_items_dep])
alpha(both_mods_24_x[rcads_items_anx])

# Correlation between subscales
cor(both_mods_24_x$rcads_dep_total, both_mods_24_x$rcads_anx_total, use = "pairwise.complete.obs")

# Create a variable that counts the number of NAs across the items for each participant
both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_dep_nas = rowSums(is.na(select(., !!!rcads_items_dep))))

table(as_factor(both_mods_24_x$rcads_dep_nas))
table(as_factor(both_mods_24_x$rcads_dep_nas), as_factor(both_mods_24_x$rcads_complete_dep))

both_mods_24_x <- both_mods_24_x %>%
  mutate(rcads_anx_nas = rowSums(is.na(select(., !!!rcads_items_anx))))

table(as_factor(both_mods_24_x$rcads_anx_nas))
table(as_factor(both_mods_24_x$rcads_anx_nas), as_factor(both_mods_24_x$rcads_complete_anx))

# Create RCADS scale total score (sum depression and anxiety)