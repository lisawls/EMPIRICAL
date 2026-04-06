library(dplyr)
library(fixest)

############################################################
# PARAMETERS
############################################################

shock_year <- 2008

############################################################
# 1. Build pre-shock share
############################################################

aid_hum <- processed_data_regression_aid %>%
  select(recipient_iso3, year, aid_hum_us, aid_hum_all)

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
# 2. Merge with panel and keep only pre-shock years
############################################################

data_pretrend <- processed_data_conflict %>%
  left_join(
    share_pre_hum,
    by = c("iso3" = "recipient_iso3")
  ) %>%
  filter(year < shock_year) %>%
  filter(!is.na(dummy_sup_mediane), !is.na(share_pre_hum))

############################################################
# 3. Create a linear time trend
############################################################

data_pretrend <- data_pretrend %>%
  mutate(
    year_trend = year - min(year)
  )

############################################################
# 4. Pre-trend test
# Does exposure predict different trends BEFORE the shock?
############################################################

pretrend_test <- feols(
  dummy_sup_mediane ~ share_pre_hum:year_trend | iso3 + year,
  data = data_pretrend,
  cluster = ~iso3
)

summary(pretrend_test)