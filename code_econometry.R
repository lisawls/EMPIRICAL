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
library(countrycode)
library(readr)
library(ivreg)

conflict <- read_csv("data_selected_countries.csv") %>%
  # filter(year < 2022) %>% 
  # year > 2002) %>% 
  mutate(iso3 = countrycode(country, "country.name", "iso3c"))

shift_share_wide <- read_csv("shift_share_wide.csv")

merge_final <- conflict %>%
  left_join(shift_share_wide, by = c("iso3" = "recipient_iso3", "year" = "year")) %>% 
  select(-recipient)

############################################################
# Dictionary for clean variable names in tables
############################################################

var_dict <- c(
  aid_us_dev = "US Development Food Aid",
  aid_us_hum = "US Humanitarian Aid",
  iv_dev     = "Shift-Share IV (Development)",
  iv_hum     = "Shift-Share IV (Humanitarian)",
  iso3       = "Country FE",
  year       = "Year FE",
  nb_event = "Number of Conflict Events"
)


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
  nb_event ~ aid_us_dev | iso3 + year,
  data = merge_final,
  cluster = ~iso3
)
summary(ols_dev)

# OLS regression for humanitarian aid
ols_hum <- feols(
  nb_event ~ aid_us_hum | iso3 + year,
  data = merge_final,
  cluster = ~iso3
)
summary(ols_hum)


############################################################
# 2. FIRST STAGE REGRESSIONS
#
# Purpose:
# Test whether the shift-share instrument predicts US aid.
#
# Interpretation:
# The coefficient on the IV variable (iv_dev or iv_hum)
# measures how strongly the instrument predicts aid flows.
#
# This stage is crucial for checking instrument relevance.
############################################################

# First stage for development aid
fs_dev <- feols(
  aid_us_dev ~ iv_dev | iso3 + year,
  data = merge_final,
  cluster = ~iso3)

summary(fs_dev)


# First stage for humanitarian aid
fs_hum <- feols(
  aid_us_hum ~ iv_hum | iso3 + year,
  data = merge_final,
  cluster = ~iso3
)

summary(fs_hum)



############################################################
# 3. REDUCED FORM REGRESSION
#
# Purpose:
# Estimate the direct relationship between the instrument
# and the outcome variable.
#
# Interpretation:
# This regression captures the total effect of the instrument
# on conflict events.
#
# The IV estimate is approximately:
#   (Reduced Form coefficient) / (First Stage coefficient)
############################################################

# rf_dev <- feols(
#   nb_event ~ iv_dev | iso3 + year,
#   data = merge_final,
#   cluster = ~iso3
# )
# 
# summary(rf_dev)

############################################################
# 4. IV SECOND STAGE (2SLS ESTIMATION)
#
# Purpose:
# Estimate the causal effect of US aid on conflict events
# using the shift-share instrument.
#
# The syntax:
#   outcome ~ controls | fixed effects | endogenous ~ instrument
#
# Interpretation:
# The coefficient on aid_us_dev (or aid_us_hum)
# measures the causal effect of US aid on conflict intensity.
############################################################

# IV regression for development food aid
iv_dev_model <- feols(
  nb_event ~ 1 | iso3 + year | aid_us_dev ~ iv_dev,
  data = merge_final)

etable(
  iv_dev_model,
  dict = var_dict,
  stage = 1:2,
  tex = TRUE,
  vcov = ~iso3,
  fitstat = ~ n + ivf1,
  title = "IV Estimates: Effect of US Development Food Aid on Conflict Events",
  label = "tab:iv_food"
)

# IV regression for humanitarian aid
iv_hum_model <- feols(
  nb_event ~ 1 | iso3 + year | aid_us_hum ~ iv_hum,
  data = merge_final)

etable(
  iv_hum_model,
  dict = var_dict,
  stage = 1:2,
  tex = TRUE,
  vcov = ~iso3,
  fitstat = ~ n + ivf1,
  title = "IV Estimates: Effect of US Development Humanitarian Aid on Conflict Events",
  label = "tab:iv_food"
)


############################################################
# 1. OLS RESULTS
############################################################

etable(
  ols_dev, ols_hum,
  dict = var_dict,
  tex = TRUE,
  title = "OLS Estimates: US Aid and Conflict Events",
  label = "tab:ols",
  file = "table_ols.tex"
)

############################################################
# 2. FIRST STAGE
############################################################
# 
# etable(
#   fs_dev, fs_hum,
#   dict = var_dict,
#   tex = TRUE,
#   fitstat = ~ n + r2 + ivf,
#   title = "First Stage: Instrument Relevance",
#   label = "tab:first_stage",
#   file = "table_first_stage.tex"
# )
# 
# ############################################################
# # 3. REDUCED FORM
# ############################################################
# 
# etable(
#   rf_dev,
#   dict = var_dict,
#   tex = TRUE,
#   title = "Reduced Form: Instrument and Conflict Events",
#   label = "tab:reduced_form",
#   file = "table_reduced_form.tex"
# )
# 
# ############################################################
# # 4. IV RESULTS (2SLS)
# ############################################################
# 
# etable(
#   iv_dev_model, iv_hum_model,
#   dict = var_dict,
#   tex = TRUE,
#   title = "IV Estimates: Effect of US Aid on Conflict Events",
#   label = "tab:iv",
#   file = "table_iv.tex"
# )