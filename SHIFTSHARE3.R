library(dplyr)
library(readr)
library(lubridate)
library(fixest)

############################################################
# 1. LOAD MAIN DATA
############################################################

conflict <- processed_data_conflict
aid <- processed_data_regression_aid

aid_hum <- aid %>%
  select(recipient_iso3, year, aid_hum_us, aid_hum_all)

############################################################
# 2. LOAD FEMA DATA
############################################################

fema_raw <- DisasterDeclarationsSummaries

############################################################
# 3. CLEAN FEMA DATA
############################################################

# declarationDate is already <dttm> in your data
# so DO NOT parse it again with ymd_hms()

fema_clean <- fema_raw %>%
  mutate(
    year = lubridate::year(declarationDate)
  ) %>%
  filter(
    !is.na(year),
    declarationType == "DR"
  ) %>%
  distinct(femaDeclarationString, .keep_all = TRUE)

############################################################
# 4. BUILD YEARLY US DOMESTIC DISASTER SHOCK
############################################################

fema_yearly <- fema_clean %>%
  group_by(year) %>%
  summarise(
    us_major_disasters = n(),
    log_us_major_disasters = log1p(us_major_disasters),
    .groups = "drop"
  )

print(fema_yearly, n = 50)

############################################################
# 5. CHOOSE PRE-SHOCK PERIOD FOR SHARE
############################################################

shock_year <- 2008

############################################################
# 6. BUILD PREDETERMINED SHARE
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
# 7. MERGE EVERYTHING
############################################################

data_iv_fema <- conflict %>%
  left_join(
    aid_hum,
    by = c("iso3" = "recipient_iso3", "year" = "year")
  ) %>%
  left_join(
    share_pre_hum,
    by = c("iso3" = "recipient_iso3")
  ) %>%
  left_join(
    fema_yearly,
    by = "year"
  ) %>%
  mutate(
    iv_hum_fema = share_pre_hum * log_us_major_disasters
    # alternative:
    # iv_hum_fema = share_pre_hum * us_major_disasters
  ) %>%
  filter(
    !is.na(dummy_sup_mediane),
    !is.na(aid_hum_us),
    !is.na(iv_hum_fema)
  )

############################################################
# 8. QUICK CHECKS
############################################################

cat("Number of rows in fema_clean:", nrow(fema_clean), "\n")
cat("Number of rows in fema_yearly:", nrow(fema_yearly), "\n")
cat("Number of rows in data_iv_fema:", nrow(data_iv_fema), "\n")

summary(data_iv_fema$us_major_disasters)
summary(data_iv_fema$log_us_major_disasters)
summary(data_iv_fema$iv_hum_fema)

############################################################
# 9. FIRST STAGE
############################################################

fs_fema <- feols(
  aid_hum_us ~ iv_hum_fema | iso3[year] + year,
  data = data_iv_fema,
  cluster = ~iso3
)

summary(fs_fema)

############################################################
# 10. REDUCED FORM
############################################################

rf_fema <- feols(
  dummy_sup_mediane ~ iv_hum_fema | iso3[year] + year,
  data = data_iv_fema,
  cluster = ~iso3
)

summary(rf_fema)

############################################################
# 11. SECOND STAGE
############################################################

iv_fema <- feols(
  dummy_sup_mediane ~ 1 | iso3[year] + year | aid_hum_us ~ iv_hum_fema,
  data = data_iv_fema,
  cluster = ~iso3
)

summary(iv_fema)

############################################################
# 12. TABLE
############################################################

etable(
  fs_fema, rf_fema, iv_fema,
  stage = 1:2,
  tex = TRUE,
  fitstat = ~ n + ivf1,
  title = "Alternative IV: Pre-period US humanitarian exposure × US major disaster shock",
  label = "tab:iv_hum_fema"
)