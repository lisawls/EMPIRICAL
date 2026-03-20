############################################################
# Econometric analysis: Effect of US aid on conflict events
#
# Panel structure: country (recipient_iso3) × year
# Outcome variable: nb_event (number of conflict events)
#
# Endogenous variables:
#   aid_us_dev  = US development food aid
#   aid_us_hum  = US humanitarian aid
#
# Instruments:
#   iv_dev = share_pre_dev × shock_dev
#   iv_hum = share_pre_hum × shock_hum
#
# Fixed effects:
#   - Country FE: control for time-invariant country characteristics
#   - Year FE: control for global shocks affecting all countries
#
# Standard errors clustered at the country level
############################################################

# Load econometrics package for fixed-effects and IV estimation
library(fixest)
library(readr)
library(ivreg)
library(dplyr)

conflict <- read_csv("processed_data_conflict.csv")
aid <- read_csv("processed_data_regression_aid.csv")
us_share <- read_csv("processed_stats_aid_africa_share.csv")
shift_share <- read_csv("processed_data_regression_shift_share.csv")
shock_years <- c(
  # 2001,
  # 2002, 
  # 2005,
  # 2006,
  2007,
  # 2008,
  # 2009,
  # 2010,
  2011, 
  2012,
  # 2013,
  # 2014,
  2017, 
  2018, 
  # 2019, 
  # 2020, 
  # 2021, 
  2022)

merge_final <- conflict %>%
  left_join(aid, by = c("iso3" = "recipient_iso3", "year" = "year")) %>%
  bind_cols(
    sapply(shock_years, function(y) as.integer(.$year == y)) %>%
      as.data.frame() %>%
      setNames(paste0("shock_", shock_years))
  )


merge_final_shifshare <- conflict %>%
  left_join(shift_share, by = c("iso3" = "recipient_iso3", "year" = "year"))

############################################################
# Dictionary for clean variable names in tables
############################################################

var_dict <- c(
  iv_dev     = "Shift-Share IV (Development)",
  iv_hum     = "Shift-Share IV (Humanitarian)",
  iso3       = "Country FE",
  year       = "Year FE",
  nb_event = "Number of Conflict Events",
  dummy_sup_mediane = "Indicator for above-median conflict events",
  aid_food_us = "Food",
  aid_hum_us = "Humanitarian aid",
  aid_total_us = "Both aid",
  share_us_food = "US share of development food aid (%)",
  share_us_hum = "US share of humanitarian aid (%)",
  share_us_total = "US share of combined aid (%)"
)

shock_dict <- setNames(
  paste0(" ", shock_years, ""),
  paste0("shock_", shock_years)
)

var_dict <- c(var_dict, shock_dict)

############################################################
# 1. OLS BASELINE REGRESSIONS
#
# Purpose:
# Estimate the correlation between US aid and conflict events
# controlling for country and year fixed effects.
#
# Interpretation:
# These estimates are likely biased due to reverse causality
# (conflict may attract humanitarian aid).
############################################################

# OLS regression for development food aid
ols_dev <- feols(
  dummy_sup_mediane ~ aid_food_us | iso3 + year,
  data = merge_final,
  cluster = ~iso3)

# OLS regression for humanitarian aid
ols_hum <- feols(
  dummy_sup_mediane ~ aid_hum_us | iso3 + year,
  data = merge_final,
  cluster = ~iso3)

etable(ols_dev,ols_hum,
  dict = var_dict,
  tex = TRUE,
  title = "Aid and Conflict: Baseline OLS Estimates",
  label = "tab:aid_ols",
  digits = 3,
  fitstat = ~ n + r2)

ols_models <- list()

for (y in shock_years) {
  shock_var <- paste0("shock_", y)
  
  ols_models[[paste0("dev_", y)]] <- feols(
    as.formula(paste0("dummy_sup_mediane ~ aid_food_us * ", shock_var, " | iso3 + year")),
    data = merge_final,
    cluster = ~iso3
  )
  
  ols_models[[paste0("hum_", y)]] <- feols(
    as.formula(paste0("dummy_sup_mediane ~ aid_hum_us * ", shock_var, " | iso3 + year")),
    data = merge_final,
    cluster = ~iso3
  )
  
  ols_models[[paste0("both_", y)]] <- feols(
    as.formula(paste0("dummy_sup_mediane ~ aid_total_us * ", shock_var, " | iso3 + year")),
    data = merge_final,
    cluster = ~iso3
  )}

ols_dev_models  <- ols_models[grep("^dev_", names(ols_models))]
ols_hum_models  <- ols_models[grep("^hum_", names(ols_models))]
ols_both_models <- ols_models[grep("^both_", names(ols_models))]


etable(
  ols_hum_models,
  dict = var_dict,
  tex = TRUE,
  title = "Aid and Conflict: OLS Estimates, Humanitarian Aid",
  label = "tab:aid_ols_hum",
  digits = 3,
  fitstat = ~ n + r2
)