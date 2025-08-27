# install.packages("misty")

library(misty) # Function to prorate scale scores


# Missing Data ####

# Create a list of scale total scores
measures <- c("edeqs_total", "rcads_anx_total", "rcads_dep_total", "rcads_total",
              "sdq_con_total", "sdq_emo_total", "sdq_hyp_total",
              "sdq_peer_total", "sdq_pro_total", "sdq_int_total",
              "sdq_ext_total", "sdq_dif_total", "swemwbs_total", 
              "brs_total", "ucla3_total", "ucla4_total", "yaps_total")

# Proportion of missing responses on each scale
colMeans(is.na(both_mods_24_x[, measures]))*100
# By year group
colMeans(is.na(year_8[, measures]))*100
colMeans(is.na(year_9[, measures]))*100
colMeans(is.na(year_10[, measures]))*100

# Create variables to count number of NAs on each scale ####

both_mods_24_x <- both_mods_24_x %>%
  mutate(edeqs_nas = rowSums(is.na(select(., !!!edeqs_items)))) %>%
  mutate(rcads_anx_nas = rowSums(is.na(select(., !!!rcads_anx_items)))) %>%
  mutate(rcads_dep_nas = rowSums(is.na(select(., !!!rcads_dep_items)))) %>%
  mutate(swemwbs_nas = rowSums(is.na(select(., !!!swemwbs_items)))) %>%
  mutate(sdq_con_nas = rowSums(is.na(select(., !!!sdq_con_items)))) %>%
  mutate(sdq_emo_nas = rowSums(is.na(select(., !!!sdq_emo_items)))) %>%  
  mutate(sdq_hyp_nas = rowSums(is.na(select(., !!!sdq_hyp_items)))) %>%   
  mutate(sdq_peer_nas = rowSums(is.na(select(., !!!sdq_peer_items)))) %>% 
  mutate(sdq_pro_nas = rowSums(is.na(select(., !!!sdq_pro_items)))) %>%   
  mutate(ucla3_nas = rowSums(is.na(select(., !!!ucla3_items)))) %>%   
  mutate(ucla4_nas = rowSums(is.na(select(., !!!ucla4_items)))) %>%   
  mutate(brs_nas = rowSums(is.na(select(., !!!brs_items)))) %>%   
  mutate(yaps_nas = rowSums(is.na(select(., !!!yaps_items)))) 
  
table(both_mods_24_x$edeqs_nas)
table(both_mods_24_x$rcads_anx_nas)
table(both_mods_24_x$rcads_dep_nas)
table(both_mods_24_x$swemwbs_nas)
table(both_mods_24_x$sdq_con_nas)
table(both_mods_24_x$sdq_emo_nas)
table(both_mods_24_x$sdq_hyp_nas)
table(both_mods_24_x$sdq_peer_nas)
table(both_mods_24_x$sdq_pro_nas)
table(both_mods_24_x$ucla3_nas)
table(both_mods_24_x$ucla4_nas)
table(both_mods_24_x$brs_nas)
table(both_mods_24_x$yaps_nas)

# Add these variables to year group dfs
year_8 <- both_mods_24_x %>% 
  filter(year_group == 8)
year_9 <- both_mods_24_x %>% 
  filter(year_group == 9)
year_10 <- both_mods_24_x %>% 
  filter(year_group == 10)

# Pro-rate missing items ####

## SDQ ####

# SDQ scoring algorithm prorates subscale scores with 3 or more item responses
# https://sdqinfo.org/c9.html

both_mods_24_x$sdq_con_prorated <- item.scores(both_mods_24_x[, sdq_con_items], 
                                               fun = "sum", # Function = multiply mean of available items by total number of items
                                               n.avail = 3) # Minimum number of available item responses
both_mods_24_x$sdq_con_prorated <- round(both_mods_24_x$sdq_con_prorated) # Round to integer

both_mods_24_x$sdq_emo_prorated <- item.scores(both_mods_24_x[, sdq_emo_items], 
                                               fun = "sum", # Function = multiply mean of available items by total number of items
                                               n.avail = 3) # Minimum number of available item responses
both_mods_24_x$sdq_emo_prorated <- round(both_mods_24_x$sdq_emo_prorated)

both_mods_24_x$sdq_hyp_prorated <- item.scores(both_mods_24_x[, sdq_hyp_items], 
                                               fun = "sum", # Function = multiply mean of available items by total number of items
                                               n.avail = 3) # Minimum number of available item responses
both_mods_24_x$sdq_hyp_prorated <- round(both_mods_24_x$sdq_hyp_prorated)

both_mods_24_x$sdq_peer_prorated <- item.scores(both_mods_24_x[, sdq_peer_items], 
                                               fun = "sum", # Function = multiply mean of available items by total number of items
                                               n.avail = 3) # Minimum number of available item responses
both_mods_24_x$sdq_peer_prorated <- round(both_mods_24_x$sdq_peer_prorated)

both_mods_24_x$sdq_pro_prorated <- item.scores(both_mods_24_x[, sdq_pro_items], 
                                               fun = "sum", # Function = multiply mean of available items by total number of items
                                               n.avail = 3) # Minimum number of available item responses
both_mods_24_x$sdq_pro_prorated <- round(both_mods_24_x$sdq_pro_prorated)


table(both_mods_24_x$sdq_con_nas, both_mods_24_x$sdq_con_prorated, useNA = "ifany")
table(both_mods_24_x$sdq_con_total, both_mods_24_x$sdq_con_prorated, useNA = "ifany")

table(both_mods_24_x$sdq_emo_nas, both_mods_24_x$sdq_emo_prorated, useNA = "ifany")
table(both_mods_24_x$sdq_emo_total, both_mods_24_x$sdq_emo_prorated, useNA = "ifany")

table(both_mods_24_x$sdq_hyp_nas, both_mods_24_x$sdq_hyp_prorated, useNA = "ifany")
table(both_mods_24_x$sdq_hyp_total, both_mods_24_x$sdq_hyp_prorated, useNA = "ifany")

table(both_mods_24_x$sdq_hyp_nas, both_mods_24_x$sdq_hyp_prorated, useNA = "ifany")
table(both_mods_24_x$sdq_hyp_total, both_mods_24_x$sdq_hyp_prorated, useNA = "ifany")

### Compute internalising, externalising, total difficulties (SDQ) ####

both_mods_24_x$sdq_ext_prorated <- 
  ifelse(is.na(both_mods_24_x$sdq_con_prorated) | is.na(both_mods_24_x$sdq_hyp_prorated), NA, 
         both_mods_24_x$sdq_con_prorated + both_mods_24_x$sdq_hyp_prorated)

both_mods_24_x$sdq_int_prorated <- 
  ifelse(is.na(both_mods_24_x$sdq_emo_prorated) | is.na(both_mods_24_x$sdq_peer_prorated), NA, 
         both_mods_24_x$sdq_emo_prorated + both_mods_24_x$sdq_peer_prorated)

both_mods_24_x$sdq_dif_prorated <- 
  ifelse(is.na(both_mods_24_x$sdq_ext_prorated) | is.na(both_mods_24_x$sdq_int_prorated), NA, 
         both_mods_24_x$sdq_ext_prorated + both_mods_24_x$sdq_int_prorated)

summary(both_mods_24_x$sdq_ext_total)
summary(both_mods_24_x$sdq_ext_prorated)

summary(both_mods_24_x$sdq_int_total)
summary(both_mods_24_x$sdq_int_prorated)

summary(both_mods_24_x$sdq_dif_total)
summary(both_mods_24_x$sdq_dif_prorated)

## RCADS ####
# RCADS manual recommends prorating up to 2 items on depression scale and 3 items on anxiety scale
# https://rcads.ucla.edu/manualscoring

both_mods_24_x$rcads_dep_prorated <- item.scores(both_mods_24_x[, rcads_dep_items], 
                                               fun = "sum", # Function = multiply mean of available items by total number of items
                                               n.avail = 8) # Minimum number of available item responses
both_mods_24_x$rcads_dep_prorated <- round(both_mods_24_x$rcads_dep_prorated) # Round to integer

both_mods_24_x$rcads_anx_prorated <- item.scores(both_mods_24_x[, rcads_anx_items], 
                                                 fun = "sum", # Function = multiply mean of available items by total number of items
                                                 n.avail = 12) # Minimum number of available item responses
both_mods_24_x$rcads_anx_prorated <- round(both_mods_24_x$rcads_anx_prorated) # Round to integer

### Compute RCADS total score ####

both_mods_24_x$rcads_prorated <- 
  ifelse(is.na(both_mods_24_x$rcads_dep_prorated) | is.na(both_mods_24_x$rcads_anx_prorated), NA, 
         both_mods_24_x$rcads_dep_prorated + both_mods_24_x$rcads_anx_prorated)

summary(both_mods_24_x$rcads_total)
summary(both_mods_24_x$rcads_prorated)

summary(both_mods_24_x$rcads_dep_total)
summary(both_mods_24_x$rcads_dep_prorated)

summary(both_mods_24_x$rcads_anx_total)
summary(both_mods_24_x$rcads_anx_prorated)

## EDEQS ####

both_mods_24_x$edeqs_prorated <- item.scores(both_mods_24_x[, edeqs_items], 
                                               fun = "sum",
                                               p.avail = 0.8) # Minimum proportion of available item responses
both_mods_24_x$edeqs_prorated <- round(both_mods_24_x$edeqs_prorated)

summary(both_mods_24_x$edeqs_total)
summary(both_mods_24_x$edeqs_prorated)

table(both_mods_24_x$edeqs_nas)

## SWEMWBS ####

both_mods_24_x$swemwbs_prorated <- item.scores(both_mods_24_x[, swemwbs_items], 
                                               fun = "sum",
                                               p.avail = 0.8) # Minimum proportion of available item responses
both_mods_24_x$swemwbs_prorated <- round(both_mods_24_x$swemwbs_prorated)

summary(both_mods_24_x$swemwbs_total)
summary(both_mods_24_x$swemwbs_prorated)
table(both_mods_24_x$swemwbs_nas)

## YAPS ####

both_mods_24_x$yaps_prorated <- item.scores(both_mods_24_x[, yaps_items], 
                                            fun = "sum",
                                            p.avail = 0.8) # Minimum proportion of available item responses
both_mods_24_x$yaps_prorated <- round(both_mods_24_x$yaps_prorated)

summary(both_mods_24_x$yaps_total)
summary(both_mods_24_x$yaps_prorated)
table(both_mods_24_x$yaps_nas)
table(both_mods_24_x$yaps_prorated)

## BRS ####

both_mods_24_x$brs_prorated <- item.scores(both_mods_24_x[, brs_items], 
                                           fun = "sum",
                                           p.avail = 0.8) # Minimum proportion of available item responses
both_mods_24_x$brs_prorated <- round(both_mods_24_x$brs_prorated)

summary(both_mods_24_x$brs_total)
summary(both_mods_24_x$brs_prorated)
table(both_mods_24_x$brs_nas)
table(both_mods_24_x$brs_prorated)

# Multiple imputation of scale total scores ####

# Separate imputation model per year group, due to different measures in different years