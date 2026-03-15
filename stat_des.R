# 0. Packages----
library(tidyverse)
library(scales)
library(dplyr)
library(knitr)
library(kableExtra)

# 1. Load data----
aid_africa <- read_csv("processed_data_stats_aid_africa.csv")
aid_shares_cty_year <- read_csv("processed_stats_aid_africa_share.csv")

output_dir <- "stats_des"

# 2. Helper functions----

# Compute within-country variation over time for one share variable
compute_within_country_variation <- function(data, share_var) {
  data %>%
    group_by(recipient_iso3, recipient) %>%
    summarise(
      n_years = sum(!is.na(.data[[share_var]])),
      mean_share = mean(.data[[share_var]], na.rm = TRUE),
      sd_share = sd(.data[[share_var]], na.rm = TRUE),
      var_share = var(.data[[share_var]], na.rm = TRUE),
      min_share = min(.data[[share_var]], na.rm = TRUE),
      p25_share = quantile(.data[[share_var]], 0.25, na.rm = TRUE),
      median_share = median(.data[[share_var]], na.rm = TRUE),
      p75_share = quantile(.data[[share_var]], 0.75, na.rm = TRUE),
      max_share = max(.data[[share_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(var_share))
}

# Compute across-country variation by year for one share variable
compute_across_country_variation <- function(data, share_var, aid_var) {
  data %>%
    filter(.data[[aid_var]] > 0) %>%
    group_by(year) %>%
    summarise(
      n_countries = sum(!is.na(.data[[share_var]])),
      mean_share = mean(.data[[share_var]], na.rm = TRUE),
      sd_share = sd(.data[[share_var]], na.rm = TRUE),
      var_share = var(.data[[share_var]], na.rm = TRUE),
      min = min(.data[[share_var]], na.rm = TRUE),
      p25 = quantile(.data[[share_var]], 0.25, na.rm = TRUE),
      median = median(.data[[share_var]], na.rm = TRUE),
      p75 = quantile(.data[[share_var]], 0.75, na.rm = TRUE),
      max = max(.data[[share_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year)
}

# Summarise distribution for one variable
summarise_distribution <- function(data, share_var) {
  summary(data[[share_var]])
}

# Plot histogram for one variable in one year
plot_histogram_year <- function(data, share_var, year_focus, title_text) {
  data %>%
    filter(year == year_focus) %>%
    ggplot(aes(x = .data[[share_var]])) +
    geom_histogram(bins = 30) +
    labs(
      title = paste(title_text, "in", year_focus),
      x = "US aid share in total aid",
      y = "Number of countries"
    ) +
    theme_minimal()
}

# Plot density for one variable in one year
plot_density_year <- function(data, share_var, year_focus, title_text) {
  data %>%
    filter(year == year_focus) %>%
    ggplot(aes(x = .data[[share_var]])) +
    geom_density() +
    labs(
      title = paste(title_text, "in", year_focus),
      x = "US aid share in total aid",
      y = "Density"
    ) +
    theme_minimal()
}

# Compute mean and median of variance
summarise_variance <- function(data) {
  data %>%
    summarise(
      mean_var = mean(var_share, na.rm = TRUE),
      median_var = median(var_share, na.rm = TRUE)
    )
}

# Save a plot to the output directory
save_plot <- function(plot_obj, filename, width = 6, height = 4, dpi = 300) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    dpi = dpi
  )
}

# 3. Analysis of US aid shares----

share_vars <- c(
  hum = "share_us_hum",
  food = "share_us_food",
  total = "share_us_total"
)

# aid_vars <- c(
#   hum = "aid_hum_us",
#   food = "aid_food_us",
#   total = "aid_total_us"
# )

aid_vars <- c(
  hum = "aid_hum_all",
  food = "aid_food_all",
  total = "aid_total_all"
)

## 3.1 Within-country variation over time----
var_within_country_hum <- compute_within_country_variation(aid_shares_cty_year, share_vars["hum"]) 

var_within_country_hum10 <- var_within_country_hum %>% 
  arrange(desc(var_share)) %>%
  slice_head(n = 10) %>%
  mutate(Panel = "Panel A. Humanitarian aid")

var_within_country_food <- compute_within_country_variation(aid_shares_cty_year, share_vars["food"])

var_within_country_food10 <- var_within_country_food %>%
  arrange(desc(var_share)) %>%
  slice_head(n = 10) %>%
  mutate(Panel = "Panel B. Food aid")

var_within_country_total <- compute_within_country_variation(aid_shares_cty_year, share_vars["total"])

var_within_country_total10 <- var_within_country_total %>%
  arrange(desc(var_share)) %>%
  slice_head(n = 10) %>%
  mutate(Panel = "Panel C. Humanitarian and Food aid")

## 3.2 Across-country variation by year----

var_across_countries_by_year_hum <- compute_across_country_variation(
  aid_shares_cty_year, share_vars["hum"], aid_vars["hum"]) 

var_across_countries_by_year_hum_selec <- var_across_countries_by_year_hum%>%
  filter(year %in% c(1995, 2000, 2005, 2015, 2020, 2024)) %>% 
  mutate(Panel = "Panel A. Humanitarian aid")

var_across_countries_by_year_food <- compute_across_country_variation(
  aid_shares_cty_year, share_vars["food"], aid_vars["food"]) 

var_across_countries_by_year_food_selec <- var_across_countries_by_year_food%>%
  filter(year %in% c(1980, 1990, 2000, 2005, 2015, 2020, 2024)) %>% 
  mutate(Panel = "Panel B. Food aid")

var_across_countries_by_year_total <- compute_across_country_variation(
  aid_shares_cty_year, share_vars["total"], aid_vars["total"]) 

var_across_countries_by_year_total_selec <- var_across_countries_by_year_total%>%
  filter(year %in% c(1980, 1990, 2000, 2005, 2015, 2020, 2024)) %>% 
  mutate(Panel = "Panel C. Humanitarian and Food aid")

## 3.3 Distribution across countries in a selected year----

year_focus <- 2003
dist_year <- aid_shares_cty_year %>%
  filter(year == year_focus)

hist_year_food <- plot_histogram_year(
  aid_shares_cty_year,
  share_var = "share_us_food",
  year_focus = year_focus,
  title_text = "US food aid share distribution")

hist_year_hum <- plot_histogram_year(
  aid_shares_cty_year,
  share_var = "share_us_hum",
  year_focus = year_focus,
  title_text = "US humanitarian aid share distribution")

hist_year_total <- plot_histogram_year(
  aid_shares_cty_year,
  share_var = "share_us_total",
  year_focus = year_focus,
  title_text = "US food and humanitarian aid share distribution")

save_plot(hist_year_food, paste0("distrib_", year_focus, "_food.jpg"))
save_plot(hist_year_hum, paste0("distrib_", year_focus, "_hum.jpg"))
save_plot(hist_year_total, paste0("distrib_", year_focus, "_total.jpg"))


## 3.4 Distribution over time for a selected country----

country_focus <- "ZAF"

dist_country <- aid_shares_cty_year %>%
  filter(recipient_iso3 == country_focus)

plot_country <- ggplot(dist_country, aes(x = year)) +
  geom_line(aes(y = share_us_hum, color = "Humanitarian Aid")) +
  geom_line(aes(y = share_us_food, color = "Food Aid")) +
  geom_line(aes(y = share_us_total, color = "Both Aid")) +
  labs(
    title = paste("US aid share over time for", unique(dist_country$recipient)),
    x = "Year",
    y = "US aid share in total aid",
    color = "Aid type") +
  theme_minimal()

save_plot(plot_country, paste0("distrib_", country_focus, "_aid.jpg"))

## 3.5 Average variance comparisons----

within_variance_summary <- bind_rows(
  hum = summarise_variance(var_within_country_hum),
  food = summarise_variance(var_within_country_food),
  total = summarise_variance(var_within_country_total),
  .id = "measure"
)

across_variance_summary <- bind_rows(
  hum = summarise_variance(var_across_countries_by_year_hum),
  food = summarise_variance(var_across_countries_by_year_food),
  total = summarise_variance(var_across_countries_by_year_total),
  .id = "measure"
)

## 3.6 Plot variance ----
plot_variance_by_year <- function(data, share_var, aid_var, title_text) {
  compute_across_country_variation(data, share_var, aid_var) %>%
    ggplot(aes(x = year, y = var_share)) +
    geom_line() +
    labs(
      title = title_text,
      x = "Year",
      y = "Across-country variance of US aid share"
    ) +
    theme_minimal()
}

plot_var_year_hum <- plot_variance_by_year(
  aid_shares_cty_year, "share_us_hum", "aid_hum_all",
  "Across-country variance over time: humanitarian aid"
)

plot_var_year_food <- plot_variance_by_year(
  aid_shares_cty_year, "share_us_food", "aid_food_all",
  "Across-country variance over time: food aid"
)

plot_var_year_total <- plot_variance_by_year(
  aid_shares_cty_year, "share_us_total", "aid_total_all",
  "Across-country variance over time: humanitarian and food aid"
)

save_plot(plot_var_year_hum, "plot_hum_variance.jpg")
save_plot(plot_var_year_food, "plot_food_variance.jpg")
save_plot(plot_var_year_total, "plot_total_variance.jpg")




## 3.7 Latex tables----
table_within_country <- bind_rows(
  var_within_country_hum10,
  var_within_country_food10,
  var_within_country_total10
) %>%
  select(recipient, n_years, mean_share, sd_share, var_share, min_share, p25_share, median_share, p75_share, max_share) %>%
  rename(
    Country = recipient,
    `Years in sample` = n_years,
    Mean = mean_share,
    SD = sd_share,
    `Within-country variation` = var_share,
    Min = min_share,
    p25 = p25_share,
    Median = median_share,
    p75 = p75_share,
    Max = max_share)

table_within_country %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    linesep = "",
    caption = "Top 10 countries with the highest within-country variation in US aid shares",
    align = c("l","c","c","c","c","c","c", "c","c","c"),
    digits = 3,
    escape = FALSE
  ) %>%
  pack_rows("Panel A. Humanitarian aid", 1, 10) %>%
  pack_rows("Panel B. Food aid", 11, 20) %>%
  pack_rows("Panel C. Humanitarian and Food aid", 21, 30) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size = 10)



table_across <- bind_rows(var_across_countries_by_year_hum_selec, var_across_countries_by_year_food_selec, var_across_countries_by_year_total_selec) %>%
  select(Panel, year, n_countries, mean_share, sd_share,
         var_share, min, p25, median, p75, max)

table_across %>%
  select(-Panel) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    digits = 3,
    align = c("c","c","c","c","c","c","c","c"),
    col.names = c(
      "Year",
      "Number of countries",
      "Mean share",
      "SD",
      "Across-country variance",
      "Min",
      "p25",
      "Median",
      "p75",
      "Max"
    ),
    caption = "Distribution of aid shares across countries by selected years"
  ) %>%
  pack_rows("Panel A. Humanitarian aid", 1, 6) %>%
  pack_rows("Panel B. Food aid", 7, 13) %>%
  pack_rows("Panel C. Humanitarian and Food aid", 14, 20) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    position = "center",
    font_size = 10)


table_variance_summary <- bind_rows(
  within_variance_summary %>%
    mutate(Panel = "Panel A. Within-country variance"),
  across_variance_summary %>%
    mutate(Panel = "Panel B. Across-country variance")
) %>%
  mutate(
    measure = recode(
      measure,
      hum = "Humanitarian aid",
      food = "Food aid",
      total = "Humanitarian and Food aid"
    )
  ) %>%
  select(Panel, measure, mean_var, median_var) %>%
  rename(
    Measure = measure,
    `Mean variance` = mean_var,
    `Median variance` = median_var
  )

table_variance_summary %>%
  select(-Panel) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    linesep = "",
    caption = "Summary of within-country and across-country variation in US aid shares",
    align = c("l", "c", "c"),
    digits = 3,
    escape = FALSE
  ) %>%
  pack_rows("Panel A. Within-country variance", 1, 3) %>%
  pack_rows("Panel B. Across-country variance", 4, 6) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size = 10
  )


# 4. Descriptive statistics on aid amounts----

us_african_countries <- aid_africa %>%
  filter(recipient_type == "country") %>%
  filter(donor_iso3 == "USA") %>% 
  select(-c('recipient_type','is_usa','donor_iso3','donor')) 

us_africa_aggregates <- aid_africa %>%
  filter(recipient_type == "aggregate") %>%
  filter(donor_iso3 == "USA") %>% 
  select(-c('recipient_type','is_usa','donor_iso3','donor')) 


## 4.1 Descriptive statistics by aid measure----
# For each aid measure, we compute basic summary statistics:
# number of observations, number of recipient countries,
# time coverage, and distributional moments of aid amounts.

us_african_countries %>%
  group_by(measure) %>%
  summarise(
    n_obs       = n(),                             # Total number of observations
    n_countries = n_distinct(recipient),           # Number of distinct recipient countries
    start_year  = min(year),                       # First year of observation
    end_year    = max(year),                       # Last year of observation
    mean_aid    = mean(amount, na.rm = TRUE),      # Mean aid amount
    median_aid  = median(amount, na.rm = TRUE),    # Median aid amount
    sd_aid      = sd(amount, na.rm = TRUE),        # Standard deviation
    min_aid     = min(amount, na.rm = TRUE),       # Minimum observed aid
    max_aid     = max(amount, na.rm = TRUE),       # Maximum observed aid
    .groups = "drop"
  )

## 4.2 Country coverage over time----
# For each aid measure, we first compute the number of recipient
# countries per year, then summarise its distribution over time.

us_african_countries %>%
  group_by(measure, year) %>%
  summarise(
    n_countries = n_distinct(recipient_iso3),      # Number of countries receiving aid in a given year
    .groups = "drop"
  ) %>%
  group_by(measure) %>%
  summarise(
    mean_countries = mean(n_countries),            # Average yearly coverage
    min_countries  = min(n_countries),             # Minimum yearly coverage
    max_countries  = max(n_countries)              # Maximum yearly coverage
  )

## 4.3 Aid concentration across recipient countries----
# We compute total aid received by each country (by measure),
# then calculate each country's share of total aid and identify
# the top 5 recipients per measure.

us_african_countries %>%
  group_by(measure, recipient) %>%
  summarise(
    total_aid = sum(amount, na.rm = TRUE),         # Total aid received by a country
    .groups = "drop"
  ) %>%
  group_by(measure) %>%
  mutate(
    total_measure = sum(total_aid),                 # Total aid for the measure
    share_pct = 100 * total_aid / total_measure     # Country's share of total aid (%)
  ) %>%
  arrange(measure, desc(share_pct)) %>%
  slice_head(n = 5)                                 # Top 5 recipient countries

## 4.4 Aggregate aid trends: Africa----
# We aggregate total US aid over time at the continental level
# and plot trends separately by aid measure.

aid_trend <- us_africa_aggregates %>%
  filter(recipient == "Afrique") %>% 
  group_by(year, measure) %>%
  summarise(
    total_aid = sum(amount, na.rm = TRUE),          # Total aid to Africa per year and measure
    .groups = "drop")

plot_africa <- ggplot(aid_trend, aes(x = year, y = total_aid, linetype = measure)) +
  geom_line(linewidth = 1) +
  labs(
    x = NULL,
    y = "US aid (constant USD, millions)",
    linetype = "Aid type"
  ) +
  scale_y_continuous(labels = label_number()) +
  theme_minimal()
plot_africa

save_plot(plot_africa, "aid_trend_africa.jpg")



## 4.5 Aggregate aid trends: Sub-Saharan Africa----
# Same analysis as above, restricted to Sub-Saharan Africa.

aid_trend_ssa <- us_africa_aggregates %>%
  filter(recipient == "Afrique sub-saharienne") %>% 
  group_by(year, measure) %>%
  summarise(
    total_aid = sum(amount, na.rm = TRUE),
    .groups = "drop"
  )

plot_ssa <- ggplot(aid_trend_ssa, aes(x = year, y = total_aid, linetype = measure)) +
  geom_line(linewidth = 1) +
  labs(
    x = NULL,
    y = "US aid (constant USD, millions)",
    linetype = "Aid type"
  ) +
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

plot_ssa

save_plot(plot_ssa, "aid_trend_ssa.jpg")