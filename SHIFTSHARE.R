############################################################
# 2SLS - Humanitarian aid only
# Shift-share IV:
#   Z_it = Shock_t * Share_pre_i
#
# Share_pre_i = sum_{t <= t*} HumAid_US_it / sum_{t <= t*} HumAid_AllDonors_it
# Shock_t = 1[ t > t* ]
#
# Panel: country x year
# Outcome: dummy_sup_mediane
# FE: country + year
# SE: clustered by country
############################################################

library(dplyr)
library(readr)
library(fixest)

############################################################
# 1. Load data
############################################################

conflict <- processed_data_conflict
aid <- processed_data_regression_aid

############################################################
# 2. Keep only the variables we need
#
# IMPORTANT:
# Adapt the variable names below if needed.
# I assume:
#   - country id in aid file: recipient_iso3
#   - year: year
#   - US humanitarian aid: aid_hum_us
#   - Total humanitarian aid from all donors: aid_hum_all
############################################################

aid_hum <- aid %>%
  select(recipient_iso3, year, aid_hum_us, aid_hum_all)

############################################################
# 3. Choose the shock date
#
# Example 1: Trump election
#   t* = 2016
#   Shock_t = 1 if year > 2016
#
# If you want the 2008 crisis instead:
#   replace shock_year <- 2007
############################################################

shock_year <- 2016

############################################################
# 4. Build the predetermined pre-shock humanitarian share
#
# Share_pre_i = sum_{t <= t*} aid_hum_us / sum_{t <= t*} aid_hum_all
#
# This is country-specific and fixed over time.
############################################################

share_pre_hum <- aid_hum %>%
  filter(year <= shock_year) %>%
  group_by(recipient_iso3) %>%
  summarise(
    hum_us_pre  = sum(aid_hum_us, na.rm = TRUE),
    hum_all_pre = sum(aid_hum_all, na.rm = TRUE),
    share_pre_hum = if_else(hum_all_pre > 0, hum_us_pre / hum_all_pre, NA_real_),
    .groups = "drop"
  )

############################################################
# 5. Merge conflict data with aid data and pre-shock shares
############################################################

data_iv <- conflict %>%
  left_join(aid_hum, by = c("iso3" = "recipient_iso3", "year" = "year")) %>%
  left_join(share_pre_hum, by = c("iso3" = "recipient_iso3")) %>%
  mutate(
    shock = if_else(year > shock_year, 1, 0),
    iv_hum = shock * share_pre_hum
  )

############################################################
# 6. Optional: remove rows with missing values
############################################################

data_iv <- data_iv %>%
  filter(
    !is.na(dummy_sup_mediane),
    !is.na(aid_hum_us),
    !is.na(iv_hum)
  )

############################################################
# 7. First stage
#
# HumAid_it = alpha_i + gamma_t + pi * iv_hum_it + u_it
############################################################

fs_hum <- feols(
  aid_hum_us ~ iv_hum | iso3 + year,
  data = data_iv,
  cluster = ~iso3
)

summary(fs_hum)

############################################################
# 8. Reduced form
#
# Conflict_it = alpha_i + gamma_t + rho * iv_hum_it + e_it
############################################################

rf_hum <- feols(
  dummy_sup_mediane ~ iv_hum | iso3 + year,
  data = data_iv,
  cluster = ~iso3
)

summary(rf_hum)

############################################################
# 9. 2SLS / IV second stage
#
# Conflict_it = alpha_i + gamma_t + beta * HumAid_hat_it + eps_it
############################################################

iv_hum_model <- feols(
  dummy_sup_mediane ~ 1 | iso3 + year | aid_hum_us ~ iv_hum,
  data = data_iv,
  cluster = ~iso3
)

summary(iv_hum_model)

############################################################
# 10. Table
############################################################

etable(
  fs_hum, rf_hum, iv_hum_model,
  stage = 1:2,
  tex = TRUE,
  fitstat = ~ n + ivf1,
  title = "Shift-Share IV: Effect of US Humanitarian Aid on Conflict Events",
  label = "tab:iv_hum_shiftshare"
)