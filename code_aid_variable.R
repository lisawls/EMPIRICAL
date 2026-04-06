# ============================================================
# Empirical Project – Aid Data Cleaning, Descriptive Statistics,
# and Shift-Share Instrument Construction
#
# This script prepares OECD DAC aid data for the empirical analysis
# of US foreign aid to African countries.
#
# Main steps:
# 1. Load and standardize the raw OECD DAC data
# 2. Restrict the sample to constant-price disbursements
# 3. Identify recipient countries versus aggregate regions
# 4. Keep African recipients only
# 5. Build country-year aid aggregates by donor and aid type
# 6. Compute pre-2017 exposure to US aid
# 7. Compute the post-2016 aggregate US aid shock
# 8. Construct a Bartik-style shift-share instrument
#
# Instrument logic:
#   Exposure_i =
#     share of US aid in total aid received by country i before 2017
#
#   Shock_t =
#     change in the aggregate US share of aid after 2016,
#     relative to its pre-2017 average
#
#   Instrument_it =
#     Exposure_i × Shock_t
#
# Data source:
#   OECD DAC (DAC2A), disbursements, constant prices, USD millions
#
# Authors: Lisa Willems, Coline Latge, Juliette Perrenoud
# Date: 25/01/2026
# ============================================================

# 0) Packages----
library(readr)
library(dplyr)
library(countrycode)
library(stringr)
library(ggplot2)
library(tidyr)

# 1) Parameters----
# End of the pre-treatment period and start of the post-treatment period
pre_end <- 2008
post_start <- 2009

# Optional sample restrictions on years
year_min <- 1995
year_max <- NA

# 2) Load raw data and standardize variables----
aid_raw <- read_csv("original_data_aid.csv")

# Build a recipient lookup table:
# - detect whether each recipient code corresponds to a country or an aggregate
# - recover continent information for sovereign countries

iso_lookup <- tibble(recipient_iso3 = unique(aid_raw$RECIPIENT)) %>%
  mutate(
    recipient_en = countrycode(recipient_iso3, "iso3c", "country.name.en"),
    continent = countrycode(recipient_iso3, "iso3c", "continent"),
    recipient_type = case_when(
      recipient_iso3 == "XKV" ~ "country",
      !is.na(recipient_en) ~ "country",
      TRUE ~ "aggregate"
    ),
    continent = if_else(recipient_type == "country", continent, NA_character_)
  )

aid_clean <- aid_raw %>%
  filter(`Type de prix` == "Prix constants") %>%
  transmute(
    recipient_iso3 = RECIPIENT,
    recipient = Receveur,
    donor = Donneur,
    donor_iso3 = DONOR,
    measure = recode(
      Mesure,
      "Aide alimentaire développementale" = "Food Aid Development",
      "Aide humanitaire" = "Humanitarian Aid",
      .default = Mesure
    ),
    year = as.integer(TIME_PERIOD),
    # amount = if_else(coalesce(OBS_VALUE, 0) < 0, NA_real_, coalesce(OBS_VALUE, 0)),
    amount = coalesce(OBS_VALUE, 0),
    is_usa = DONOR == "USA"
  ) %>%
  left_join(iso_lookup, by = "recipient_iso3")


# 3) Restrict the sample to Africa----
# Keep: African sovereign countries & African aggregate recipients whose names start with "Afrique"
# Exclude: Mayotte and Saint Helena, which are not part of the relevant sample
aid_africa <- aid_clean %>%
  filter(
    (recipient_type == "country" & continent == "Africa") |
      (recipient_type == "aggregate" & str_detect(recipient, "^Afrique")),
    !recipient_iso3 %in% c("MYT", "SHN"),
    is.na(year_max) | year <= year_max,
    is.na(year_min) | year >= year_min
  ) %>%
  select(-continent)
# write.csv(aid_africa, "processed_data_stats_aid_africa.csv", row.names = FALSE)


# 4) Construct country-year aid aggregates (US vs. total aid)----
# For each country-year and aid category, compute:
# - total aid from all donors
# - aid from the United States only
# Then reshape the data to one row per country-year with separate 
# variables for food aid, humanitarian aid, and their total.

tot_aid_country_africa <- aid_africa %>%
  filter(recipient_type == "country",
         measure %in% c("Food Aid Development", "Humanitarian Aid")) %>%
  group_by(recipient_iso3, recipient_en, measure, year) %>%
  summarise(
    aid_all = sum(amount, na.rm = TRUE),
    aid_us = sum(amount[is_usa], na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(
    measure = case_when(
      measure == "Food Aid Development" ~ "food",
      measure == "Humanitarian Aid" ~ "hum")) %>%
  pivot_wider(
    names_from = measure,
    values_from = c(aid_all, aid_us),
    names_sep = "_",
    values_fill = 0
  ) %>% 
  transmute(
    recipient_iso3,
    recipient = recipient_en,
    year,
    aid_food_all = aid_all_food,
    aid_hum_all = aid_all_hum,
    aid_food_us = aid_us_food,
    aid_hum_us = aid_us_hum,
    aid_total_all = aid_food_all + aid_hum_all,
    aid_total_us = aid_food_us + aid_hum_us) %>% 
  filter(year > 1994)

conflict <- read.csv("processed_data_conflict.csv")

crisis <- conflict %>%
  left_join(tot_aid_country_africa, by = c("iso3" = "recipient_iso3", "year" = "year")) %>% 
  group_by(year) %>%
  summarise(total_hum = sum(aid_hum_us, na.rm = TRUE)) 

write.csv(tot_aid_country_africa, "processed_data_regression_aid.csv", row.names = FALSE)

 
# 4bis) Country-year share of US aid in total aid----
# These variables describe the share of US aid in total aid received
# by each country in a given year, separately by aid type and in total.
 aid_shares_cty_year <- tot_aid_country_africa %>%
   mutate(share_us_food = if_else(
       aid_food_all > 0 & aid_food_us >= 0,
       aid_food_us / aid_food_all,
       NA_real_),
     share_us_hum = if_else(
       aid_hum_all > 0 & aid_hum_us >= 0,
       aid_hum_us / aid_hum_all,
       NA_real_),
     share_us_total = if_else(
       aid_total_all > 0 & aid_total_us >= 0,
       aid_total_us / aid_total_all,
       NA_real_)) %>%
   select(
     recipient_iso3,
     recipient,
     year,
     starts_with("aid_"),
     starts_with("share_us_"))
 
# write.csv(aid_shares_cty_year, "processed_stats_aid_africa_share.csv", row.names = FALSE)

# 5) Construct the exposure term----
# Exposure is defined as pre-2017 dependence on US aid.
# For each country, compute: 
 # share_pre = cumulative US aid received before 2017 / cumulative total aid received before 2017
# The exposure is calculated separately for each type of aid.
 
pre_shares <- tot_aid_country_africa %>%
  filter(year <= pre_end) %>%
  group_by(recipient_iso3, recipient) %>%
  summarise(
    us_pre_food = sum(aid_food_us, na.rm = TRUE),
    us_pre_hum = sum(aid_hum_us, na.rm = TRUE),
    us_pre_total = sum(aid_total_us, na.rm = TRUE),
    all_pre_food = sum(aid_food_all, na.rm = TRUE),
    all_pre_hum = sum(aid_hum_all, na.rm = TRUE),
    all_pre_total = sum(aid_total_all, na.rm = TRUE),
    .groups = "drop"
  ) %>%
   mutate(share_pre_food = if_else(
       all_pre_food > 0 & us_pre_food >= 0,
       us_pre_food / all_pre_food,
       NA_real_),
     share_pre_hum = if_else(
       all_pre_hum > 0 & us_pre_hum >= 0,
       us_pre_hum / all_pre_hum,
       NA_real_),
     share_pre_total = if_else(
       all_pre_total > 0 & us_pre_total >= 0,
       us_pre_total / all_pre_total,
       NA_real_))

conflict <- read_csv("processed_data_conflict.csv") %>%
  distinct(country) %>% 
  mutate(iso3 = countrycode(country, "country.name", "iso3c"))

merge <- conflict %>%
  left_join(pre_shares, by = c("iso3" = "recipient_iso3")) %>% 
  select(country, share_pre_hum)

library(knitr)

merge %>%
  arrange(desc(share_pre_hum)) %>% 
  mutate(share_pre_hum = round(share_pre_hum, 2)) %>% 
  kable(format = "latex",
        booktabs = TRUE,
        longtable = TRUE,
        digits = 2,
        col.names = c("Country", "Humanitarian aid (US share, %)"),
        caption = "Country-level U.S. humanitarian aid share before 2017",
        align = c("l", "r")) %>%
  #kable_styling(latex_options = c("repeat_header")) %>%
  cat()


summary_stats <- merge %>%
  summarise(
    Mean = mean(share_pre_hum, na.rm = TRUE),
    Median = median(share_pre_hum, na.rm = TRUE),
    SD = sd(share_pre_hum, na.rm = TRUE),
    Min = min(share_pre_hum, na.rm = TRUE),
    Max = max(share_pre_hum, na.rm = TRUE),
    N = sum(!is.na(share_pre_hum))
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

summary_stats %>%
  kable(format = "latex",
        booktabs = TRUE,
        caption = "Descriptive statistics of U.S. humanitarian aid share before 2017",
        align = "c") %>%
  kable_styling(latex_options = "hold_position") %>%
  cat()

# pays_ayant_recu_aide_us_total <- tot_aid_country_africa %>%
#   group_by(recipient_iso3, recipient) %>%
#   summarise(
#     total_us_recu = sum(aid_total_us, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(total_us_recu > 0)
# 
# pays_ayant_recu_aide_us_hum <- tot_aid_country_africa %>%
#   group_by(recipient_iso3, recipient) %>%
#   summarise(
#     hum_us_recu = sum(aid_hum_us, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(hum_us_recu > 0)
# 
# 
# pays_ayant_recu_aide_us_food <- tot_aid_country_africa %>%
#   group_by(recipient_iso3, recipient) %>%
#   summarise(
#     food_us_recu = sum(aid_food_us, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(food_us_recu > 0)
# 
# tableau_pays_aide_us <- pays_ayant_recu_aide_us_total %>%
#   full_join(pays_ayant_recu_aide_us_hum, by = c("recipient_iso3", "recipient")) %>%
#   full_join(pays_ayant_recu_aide_us_food, by = c("recipient_iso3", "recipient")) %>%
#   mutate(
#     total_us = !is.na(total_us_recu),
#     hum_us = !is.na(hum_us_recu),
#     food_us = !is.na(food_us_recu)
#   ) %>%
#   select(recipient_iso3, recipient, total_us, hum_us, food_us)


# write.csv(tableau_pays_aide_us, "processed_data_list_pays.csv", row.names = FALSE)

# 6) Construct the shock term----
 # 1. Compute the aggregate US share of aid in each year across all African recipient countries:
 #   us_share_t = total US aid in year t / total aid from all donors in year t
 # 2. Compute the average of this aggregate US share during the pre-2017 period.
 # The shock is defined as:
   # - 0 before 2017
   # - the deviation from the pre-period average from 2017 onward 
shocks <- tot_aid_country_africa %>%
  group_by(year) %>%
  summarise(
    us_food = sum(aid_food_us, na.rm = TRUE),
    us_hum = sum(aid_hum_us, na.rm = TRUE),
    us_total = sum(aid_total_us, na.rm = TRUE),
    all_food = sum(aid_food_all, na.rm = TRUE),
    all_hum = sum(aid_hum_all, na.rm = TRUE),
    all_total = sum(aid_total_all, na.rm = TRUE),
    .groups = "drop"
  )  %>%
   mutate(
     us_share_food = if_else(
       all_food > 0 & us_food >= 0,
       us_food / all_food,
       NA_real_
     ),
     us_share_hum = if_else(
       all_hum > 0 & us_hum >= 0,
       us_hum / all_hum,
       NA_real_
     ),
     us_share_total = if_else(
       all_total > 0 & us_total >= 0,
       us_total / all_total,
       NA_real_
     )
   ) %>%
  mutate(
    us_share_food_pre = mean(us_share_food[year <= pre_end], na.rm = TRUE),
    us_share_hum_pre = mean(us_share_hum[year <= pre_end], na.rm = TRUE),
    us_share_total_pre = mean(us_share_total[year <= pre_end], na.rm = TRUE),
    shock_food = if_else(year >= post_start, us_share_food - us_share_food_pre, 0),
    shock_hum = if_else(year >= post_start, us_share_hum - us_share_hum_pre, 0),
    shock_total = if_else(year >= post_start, us_share_total - us_share_total_pre, 0)
  ) %>%
  select(year, shock_food, shock_hum, shock_total)

# 7) Construct the shift-share instrument----
 # Merge: the country-level exposure term and the year-level shock term
 # Compute the final instrument: iv_it = share_pre_i × shock_t

shift_share <- tot_aid_country_africa %>%
  left_join(pre_shares, by = c("recipient_iso3", "recipient")) %>%
  left_join(shocks, by = "year") %>%
  transmute(
    recipient_iso3,
    recipient,
    year,
    aid_food_us,
    iv_food = share_pre_food * shock_food,
    aid_hum_us,
    iv_hum = share_pre_hum * shock_hum,
    aid_total_us,
    iv_total = share_pre_total * shock_total
  )

# write.csv(shift_share, "processed_data_regression_shift_share.csv", row.names = FALSE)