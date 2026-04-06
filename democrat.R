library(fixest)

# 1. Variable politique US
us_politics <- tibble(
  year = 1990:2024,
  democrat = case_when(
    year >= 1990 & year <= 1992 ~ 0,
    year >= 1993 & year <= 2000 ~ 1,
    year >= 2001 & year <= 2008 ~ 0,
    year >= 2009 & year <= 2016 ~ 1,
    year >= 2017 & year <= 2020 ~ 0,
    year >= 2021 & year <= 2024 ~ 1))

tot_aid_country_africa <- tot_aid_country_africa %>% select(recipient_iso3, recipient, year, aid_hum_all,aid_hum_us)

# 2. Exposition pré-2017 : part du pays dans l'aide humanitaire US totale avant 2017
pre_exposure <- tot_aid_country_africa %>%
  filter(year >= pre_end, year <= post_start) %>%
  group_by(recipient_iso3, recipient) %>%
  summarise(
    us_pre_hum = sum(aid_hum_us, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    exposure_hum = us_pre_hum / sum(us_pre_hum, na.rm = TRUE)
  )



# 3. Panel final
panel_iv <- tot_aid_country_africa %>%
  left_join(pre_exposure, by = c("recipient_iso3", "recipient")) %>%
  left_join(us_politics, by = "year") %>%
  mutate(
    z_dem_hum = democrat * exposure_hum,
    ln_aid_hum_us = log1p(aid_hum_us)
  )

conflict <- read_csv("processed_data_conflict.csv")

panel_iv <- conflict %>%
  left_join(panel_iv, by = c("iso3" = "recipient_iso3", "year" = "year"))


# 4. First stage simple
m_first <- feols(
  aid_hum_us ~ z_dem_hum | iso3 + year,
  cluster = ~iso3,
  data = panel_iv
)

summary(m_first)


iv_model <- feols(
  dummy_sup_mediane ~ 1 | iso3 + year | aid_hum_us ~ z_dem_hum,
  cluster = ~iso3,
  data = panel_iv
)

summary(iv_model)
summary(m_first)