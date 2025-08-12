
#### EDE-QS ####

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


# Recode item responses 1-4 to 0-3

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

# Create recode function
recode14_03 <- function(x) {
  case_match(x,
             1 ~ 0,
             2 ~ 1,
             3 ~ 2,
             4 ~ 3,
             NA ~ NA
  )
}

# Recode EDE-QS items
both_mods_24_x <- both_mods_24_x %>%
  mutate(across(all_of(EDEQS_items), recode14_03))

# Check new item response frequencies and NA's against old (above)
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

# How many responded 0-days to Q9?
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


