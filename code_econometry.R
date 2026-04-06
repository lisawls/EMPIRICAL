# Load econometrics package for fixed-effects and IV estimation
library(fixest)
library(readr)
library(dplyr)

# Import country-year conflict and aid data
conflict <- read_csv("processed_data_conflict.csv")
aid <- read_csv("processed_data_regression_aid.csv")
# us_share <- read_csv("processed_stats_aid_africa_share.csv")
# shift_share <- read_csv("processed_data_regression_shift_share.csv")

# Define the set of shock years considered in the analysis
shock_years <- c(
  # 2001,
  # 2002,
  # 2005,
  2006,
  # 2007,
  2008,
  # 2009,
  # 2010,
  2011,
  2012,
  # 2013,
  2014,
  2017)
  # 2018,
  # 2019,
  # 2020,
  # 2021,
  # 2022)

# Merge conflict outcomes with aid data at the country-year level.
merge_final <- conflict %>%
  left_join(aid, by = c("iso3" = "recipient_iso3", "year" = "year"))
  
# Create shock-year indicators:
# shock_y       = 1 only in year y
# shock_post_y  = 1 in year y and all subsequent years
for (y in shock_years) {
    merge_final[[paste0("shock_", y)]] <- as.integer(merge_final$year == y)
    merge_final[[paste0("shock_post_", y)]] <- as.integer(merge_final$year >= y)
  }

# Dictionary for clean variable names in tables
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
  share_us_total = "US share of combined aid (%)")

shock_dict <- setNames(
  paste0(" ", shock_years, ""),
  paste0("shock_", shock_years))

shock_post_dict <- setNames(
  paste0("Post ", shock_years, ""),
  paste0("shock_post_", shock_years))

var_dict <- c(var_dict, shock_dict, shock_post_dict)

# Baseline two-way fixed effects models.
# Country FE absorb time-invariant country characteristics.
# Year FE absorb common shocks affecting all countries in a given year.
# Standard errors are clustered at the country level.
ols_dev <- feols(
  decile_nb_event ~ aid_food_us | iso3 + year,
  data = merge_final,
  cluster = ~iso3)

ols_hum <- feols(
  decile_nb_event ~ aid_hum_us | iso3 + year,
  data = merge_final,
  cluster = ~iso3)

# Heterogeneity analysis: estimate whether the relationship between US aid and conflict differs
# in specific shock years.
ols_models <- list()
for (y in shock_years) {
  shock_var <- paste0("shock_", y)
  
  ols_models[[paste0("hum_", y)]] <- feols(
    as.formula(paste0("dummy_sup_mediane ~ aid_hum_us * ", shock_var, " | iso3 + year")),
    data = merge_final,
    cluster = ~iso3
  )}

# Post-shock specifications: estimate whether the association between aid and conflict changes from a given shock year onward.
ols_models_post <- list()
for (y in shock_years) {
  shock_var <- paste0("shock_post_", y)

  ols_models_post[[paste0("hum_post_", y)]] <- feols(
    as.formula(paste0("dummy_sup_mediane ~ aid_hum_us * ", shock_var, " | iso3 + year")),
    data = merge_final,
    cluster = ~iso3
  )}

etable(ols_dev,ols_hum,
       dict = var_dict,
       tex = TRUE,
       title = "Aid and Conflict: Baseline OLS Estimates",
       label = "tab:aid_ols",
       digits = 3,
       fitstat = ~ n + r2)

etable(
  ols_models,
  dict = var_dict,
  tex = TRUE,
  title = "Aid and Conflict: OLS Estimates, Humanitarian Aid, Shock Year",
  label = "tab:aid_ols_hum",
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  ols_models_post,
  dict = var_dict,
  tex = TRUE,
  title = "Aid and Conflict: OLS Estimates, Humanitarian Aid, Post-Shock",
  label = "tab:aid_ols_hum_post",
  digits = 3,
  fitstat = ~ n + r2
)