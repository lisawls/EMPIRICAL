library(dplyr)
library(fixest)

conflict <- processed_data_conflict
aid <- processed_data_regression_aid

aid_hum <- aid %>%
  select(recipient_iso3, year, aid_hum_us, aid_hum_all)

shock_year <- 2008

share_pre_hum <- aid_hum %>%
  filter(year <= shock_year) %>%
  group_by(recipient_iso3) %>%
  summarise(
    hum_us_pre  = sum(aid_hum_us, na.rm = TRUE),
    hum_all_pre = sum(aid_hum_all, na.rm = TRUE),
    share_pre_hum = if_else(hum_all_pre > 0, hum_us_pre / hum_all_pre, NA_real_),
    .groups = "drop"
  )
# total US humanitarian aid by year
us_total_by_year <- aid_hum %>%
  group_by(year) %>%
  summarise(
    us_total_hum_year = sum(aid_hum_us, na.rm = TRUE),
    .groups = "drop"
  )

# leave-one-out total: total minus country i's own US humanitarian aid
aid_hum_loo <- aid_hum %>%
  left_join(us_total_by_year, by = "year") %>%
  mutate(
    us_total_hum_loo = us_total_hum_year - coalesce(aid_hum_us, 0),
    log_us_total_hum_loo = log1p(us_total_hum_loo)
  )
data_iv_alt <- conflict %>%
  left_join(
    aid_hum_loo,
    by = c("iso3" = "recipient_iso3", "year" = "year")
  ) %>%
  left_join(
    share_pre_hum,
    by = c("iso3" = "recipient_iso3")
  ) %>%
  mutate(
    iv_hum_alt = share_pre_hum * log_us_total_hum_loo
  ) %>%
  filter(
    !is.na(dummy_sup_mediane),
    !is.na(aid_hum_us),
    !is.na(iv_hum_alt)
  )
fs_alt <- feols(
  aid_hum_us ~ iv_hum_alt | iso3[year] + year,
  data = data_iv_alt,
  cluster = ~iso3
)

rf_alt <- feols(
  dummy_sup_mediane ~ iv_hum_alt | iso3[year] + year,
  data = data_iv_alt,
  cluster = ~iso3
)

iv_alt <- feols(
  dummy_sup_mediane ~ 1 | iso3[year] + year | aid_hum_us ~ iv_hum_alt,
  data = data_iv_alt,
  cluster = ~iso3
)

summary(fs_alt)
summary(rf_alt)
summary(iv_alt)

etable(
  fs_alt, rf_alt, iv_alt,
  stage = 1:2,
  tex = TRUE,
  fitstat = ~ n + ivf1,
  title = "Alternative IV: Pre-period US humanitarian exposure × leave-one-out aggregate US humanitarian aid shock",
  label = "tab:iv_hum_alt_loo"
)