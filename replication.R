############################################################
# BRIDGE TABLE: Nunn-Qian replication -> hybrid -> our data
#
# Column 1 = Their data + their measure
# Column 2 = Their data + our measure
# Column 3 = Our data   + our measure
#
# Main package choices:
# - haven: read .dta
# - fixest: FE OLS with clustered SE
# - modelsummary: nice regression table
############################################################

library(haven)
library(readr)
library(dplyr)
library(stringr)
library(fixest)
library(modelsummary)
library(tidyr)

############################################################
# 0. FILE PATHS
############################################################

FAid_Final <- read_dta("C:/Users/lisaw/Documents/COURS/4.Fac/ECO/M2/S2/Empirical Project/Replication-Files/FAid_Final.dta")
# Nunn-Qian original replication data


processed_data_conflict <- read_csv("processed_data_conflict.csv")
processed_data_regression_aid <- read_csv("processed_data_regression_aid.csv")
# Your processed files

# Output
out_table_dev <- "table_bridge_ols_development.tex"
out_table_hum <- "table_bridge_ols_humanitarian.tex"
out_table_tot <- "table_bridge_ols_total.tex"

############################################################
# 1. LOAD NUNN-QIAN DATA AND REBUILD THE VARIABLES NEEDED
############################################################
nq <- FAid_Final

# Stata log shows they rescale wheat aid and several variables by 1000
# before estimation. It also shows the sample is restricted to 1971-2006. :contentReference[oaicite:2]{index=2}
nq <- nq %>%
  mutate(
    wheat_aid                 = wheat_aid / 1000,
    US_wheat_production       = US_wheat_production / 1000,
    recipient_wheat_prod      = recipient_wheat_prod / 1000,
    recipient_cereals_prod    = recipient_cereals_prod / 1000,
    real_usmilaid             = real_usmilaid / 1000,
    real_us_nonfoodaid_ecaid  = real_us_nonfoodaid_ecaid / 1000,
    non_us_oda_net            = non_us_oda_net / 1000,
    non_us_oda_net2           = non_us_oda_net2 / 1000,
    world_wheat_aid           = world_wheat_aid / 1000,
    world_cereals_aid         = world_cereals_aid / 1000,
    non_US_wheat_aid          = non_US_wheat_aid / 1000,
    non_US_cereals_aid        = non_US_cereals_aid / 1000
  ) %>%
  filter(year >= 1971, year <= 2006)

# Create the variables used to define "in_sample" in the log:
# instrument = l.US_wheat_production * fadum_avg
# plus the baseline controls. :contentReference[oaicite:3]{index=3}
nq <- nq %>%
  arrange(obs, year) %>%
  group_by(obs) %>%
  mutate(
    l_US_wheat_production = lag(US_wheat_production)
  ) %>%
  ungroup()

# Weather x fadum_avg interactions
months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

for (m in months) {
  nq[[paste0("all_Precip_", m, "_faavg")]] <- nq[[paste0("all_Precip_", m)]] * nq$fadum_avg
  nq[[paste0("all_Temp_",   m, "_faavg")]] <- nq[[paste0("all_Temp_",   m)]] * nq$fadum_avg
}

# Income interaction variables appearing in the log. :contentReference[oaicite:4]{index=4}
nq <- nq %>%
  mutate(
    USA_ln_income            = log(USA_rgdpch),
    oil_fadum_avg            = oil_price_2011_USD * fadum_avg,
    US_income_fadum_avg      = USA_ln_income * fadum_avg,
    US_democ_pres_fadum_avg  = US_president_democ * fadum_avg,
    instrument               = l_US_wheat_production * fadum_avg
  )

# Country averages used to build year-specific controls in the log
nq <- nq %>%
  group_by(risocode) %>%
  mutate(
    ln_rgdpch_avg = mean(ln_rgdpch, na.rm = TRUE)
  ) %>%
  ungroup()

# Rebuild the year-specific controls:
# gdp_y2-gdp_y36, usmil_y2-usmil_y36, usec_y2-usec_y36,
# rcereal_y2-rcereal_y36, rimport_y2-rimport_y36. :contentReference[oaicite:5]{index=5}
years_nq <- sort(unique(nq$year))

for (i in seq_along(years_nq)) {
  yy <- years_nq[i]
  ind <- as.integer(nq$year == yy)
  
  nq[[paste0("gdp_y", i)]]     <- nq$ln_rgdpch_avg * ind
  nq[[paste0("usmil_y", i)]]   <- nq$real_usmilaid_avg * ind
  nq[[paste0("usec_y", i)]]    <- nq$real_us_nonfoodaid_ecaid_avg * ind
  nq[[paste0("rcereal_y", i)]] <- nq$recipient_pc_cereals_prod_avg * ind
  nq[[paste0("rimport_y", i)]] <- nq$cereal_pc_import_quantity_avg * ind
}

# Baseline controls exactly as described in the Stata log. :contentReference[oaicite:6]{index=6}
baseline_controls <- c(
  "oil_fadum_avg",
  "US_income_fadum_avg",
  "US_democ_pres_fadum_avg",
  paste0("gdp_y", 2:36),
  paste0("usmil_y", 2:36),
  paste0("usec_y", 2:36),
  paste0("rcereal_y", 2:36),
  paste0("rimport_y", 2:36),
  paste0("all_Precip_", months),
  paste0("all_Temp_", months),
  paste0("all_Precip_", months, "_faavg"),
  paste0("all_Temp_", months, "_faavg")
)

# Recreate the "in_sample" logic:
# in_sample = complete cases for the baseline IV sample.
# The log reports 4089 observations in that sample. :contentReference[oaicite:7]{index=7}
vars_for_insample <- c(
  "intra_state", "wheat_aid", "instrument",
  "risocode", "year", "wb_region",
  baseline_controls
)

nq <- nq %>%
  mutate(
    in_sample = if_else(
      complete.cases(across(all_of(vars_for_insample))),
      1L, NA_integer_
    )
  )

############################################################
# 2. LOAD YOUR DATA
############################################################

conflict <- processed_data_conflict
aid      <- processed_data_regression_aid

# Your own final panel
our_panel <- conflict %>%
  left_join(aid, by = c("iso3" = "recipient_iso3", "year" = "year"))

############################################################
# 3. BUILD THE HYBRID PANEL
############################################################

# Hybrid = NQ country-year panel + your measures merged on iso3/year
# Here I assume:
# - NQ country code = risocode
# - your conflict country code = iso3
# - your aid country code in processed_data_regression_aid = recipient_iso3

hybrid_panel <- nq %>%
  left_join(
    conflict %>%
      select(iso3, year, nb_event, dummy_sup_mediane, quintile_nb_event),
    by = c("risocode" = "iso3", "year" = "year")
  ) %>%
  left_join(
    aid %>%
      select(recipient_iso3, year, aid_food_us, aid_hum_us, aid_total_us),
    by = c("risocode" = "recipient_iso3", "year" = "year")
  )

############################################################
# 4. REGRESSION HELPER
############################################################

# This function creates the exact 3-column bridge table you asked for.
#
# col1_y and col1_x are the original NQ variables
# col2_y and col2_x are your variables, but on NQ's panel
# col3_y and col3_x are your variables on your panel
#
# FE choice:
# - Column 1 uses NQ FE structure: country FE + year×world-region FE
# - Column 2 keeps the same FE structure as close as possible
# - Column 3 uses your natural FE structure: country FE + year FE

make_bridge_table <- function(col1_y, col1_x,
                              col2_y, col2_x,
                              col3_y, col3_x,
                              title_text,
                              output_file) {
  
  # Column 1: pure replication on their in_sample
  d1 <- nq %>%
    filter(in_sample == 1) %>%
    filter(!is.na(.data[[col1_y]]), !is.na(.data[[col1_x]]))
  
  m1 <- feols(
    as.formula(paste0(col1_y, " ~ ", col1_x, " | risocode + year^wb_region")),
    data = d1,
    cluster = ~risocode
  )
  
  # Column 2: their panel + our measures
  d2 <- hybrid_panel %>%
    filter(in_sample == 1) %>%
    filter(!is.na(.data[[col2_y]]), !is.na(.data[[col2_x]]))
  
  m2 <- feols(
    as.formula(paste0(col2_y, " ~ ", col2_x, " | risocode + year^wb_region")),
    data = d2,
    cluster = ~risocode
  )
  
  # Column 3: our panel + our measures
  d3 <- our_panel %>%
    filter(!is.na(.data[[col3_y]]), !is.na(.data[[col3_x]]))
  
  m3 <- feols(
    as.formula(paste0(col3_y, " ~ ", col3_x, " | iso3 + year")),
    data = d3,
    cluster = ~iso3
  )
  
  # Nice labels
  coef_map <- c(
    wheat_aid      = "NQ wheat aid",
    aid_food_us    = "US development food aid",
    aid_hum_us     = "US humanitarian aid",
    aid_total_us   = "US food-related aid (total)"
  )
  
  gof_map <- tribble(
    ~raw,               ~clean,         ~fmt,
    "nobs",             "Observations", 0,
    "r.squared",        "R²",           5,
    "within.r.squared", "Within R²",    5
  )
  
  modelsummary(
    list(
      "Replication\nTheir data + their measure" = m1,
      "Hybrid\nTheir data + our measure"        = m2,
      "Our data\nOur measure"                   = m3
    ),
    coef_map  = coef_map,
    gof_map   = gof_map,
    estimate  = "{estimate}{stars}",
    statistic = "({std.error})",
    stars     = c("*" = .1, "**" = .05, "***" = .01),
    fmt       = 5,
    title     = title_text,
    output    = output_file
  )
  
  invisible(list(m1 = m1, m2 = m2, m3 = m3))
}

############################################################
# 5. RUN THE DEVELOPMENT FOOD AID TABLE
############################################################

# Recommended first bridge table:
# - col1: NQ original outcome/treatment
# - col2: your conflict measure + your development food aid, on NQ panel
# - col3: your conflict measure + your development food aid, on your panel

res_dev <- make_bridge_table(
  col1_y = "any_war",         # or "intra_state" if you want that as the replication outcome
  col1_x = "wheat_aid",
  col2_y = "nb_event",        # you can switch to "dummy_sup_mediane"
  col2_x = "aid_food_us",
  col3_y = "nb_event",        # you can switch to "dummy_sup_mediane"
  col3_x = "aid_food_us",
  title_text = "OLS Bridge Table: Replication vs Hybrid vs Our Data (Development Food Aid)",
  output_file = out_table_dev
)

############################################################
# 6. RUN THE HUMANITARIAN TABLE
############################################################

# There is no exact NQ humanitarian counterpart.
# So column 1 stays their original food-aid replication,
# while columns 2 and 3 use your humanitarian measure.

res_hum <- make_bridge_table(
  col1_y = "any_war",
  col1_x = "wheat_aid",
  col2_y = "nb_event",
  col2_x = "aid_hum_us",
  col3_y = "nb_event",
  col3_x = "aid_hum_us",
  title_text = "OLS Bridge Table: Replication vs Hybrid vs Our Data (Humanitarian Aid)",
  output_file = out_table_hum
)

############################################################
# 7. RUN THE TOTAL-AID TABLE
############################################################

res_tot <- make_bridge_table(
  col1_y = "any_war",
  col1_x = "wheat_aid",
  col2_y = "nb_event",
  col2_x = "aid_total_us",
  col3_y = "nb_event",
  col3_x = "aid_total_us",
  title_text = "OLS Bridge Table: Replication vs Hybrid vs Our Data (Total Food-Related Aid)",
  output_file = out_table_tot
)

############################################################
# 8. OPTIONAL: LATEX OUTPUT INSTEAD OF HTML
############################################################
# If you want LaTeX tables directly, change:
out_table_dev <- "table_bridge_ols_development.tex"
out_table_hum <- "table_bridge_ols_humanitarian.tex"
out_table_tot <- "table_bridge_ols_total.tex"
############################################################