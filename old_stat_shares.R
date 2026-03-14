
# GRAPHS

# Cross-Country Variation in share_pre
p_hist <- ggplot(pre_shares, aes(x = share_pre * 100, fill = measure)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    title = "Distribution of Pre-2017 US Aid Dependency (Share of Total Aid)",
    x = "US Aid Share (Pre-2017, %)",
    y = "Number of Countries",
    fill = "Aid Type"
  ) +
  facet_wrap(~measure) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA)
  )

# Map exposure by aid type (two panels)
# 1) Country shapes
africa_sf <- ne_countries(continent = "africa", scale = "medium", returnclass = "sf")

# 2) Duplicate the sf for each aid type (keeps sf class)
measures <- c("Food Aid Development", "Humanitarian Aid")

africa_by_measure <- dplyr::bind_rows(
  lapply(measures, function(m) dplyr::mutate(africa_sf, measure = m)))

# 3) Join exposure
pre_shares_map2 <- africa_by_measure %>%
  left_join(pre_shares, by = c("iso_a3" = "recipient_iso3", "measure")) %>%
  mutate(share_pre_pct = 100 * share_pre)

# 4) Plot
p_map2 <- ggplot(pre_shares_map2) +
  geom_sf(aes(fill = share_pre), color = "grey70", linewidth = 0.1) +
  facet_wrap(~measure) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    labels = scales::percent,
    na.value = "grey90"
  ) +
  labs(
    title = "Pre-2017 US Aid Dependency (share_pre), by Aid Type",
    fill = "US share (pre-2017)"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom")

# Load African country shapes
pre_shares_mean <- pre_shares %>%
  group_by(recipient_iso3) %>%
  summarise(share_pre = mean(share_pre, na.rm = TRUE))

pre_shares_map <- ne_countries(continent = "africa", scale = "medium", returnclass = "sf") %>%
  left_join(pre_shares_mean, by = c("iso_a3" = "recipient_iso3")) %>%
  mutate(share_pre_pct = share_pre * 100)

p_map <- ggplot(pre_shares_map) +
  geom_sf(aes(fill = share_pre)) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    labels = scales::percent
  ) +
  labs(
    title = "Pre-2017 US Aid Dependency (Share of Total Aid, %)",
    fill = "US Aid Share (%)")  +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom")

# Aggregate Shock Plot
p_shock <- ggplot(shocks, aes(x = year, y = shock)) +
  geom_line() +
  facet_wrap(~measure) +
  labs(
    title = "Aggregate US Aid Share Shock (Deviation from Pre-2017 Average)",
    y = "Shock (Deviation)",
    x = "Year") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom")

p_shock

ggsave("histogram_share_pre_percentage.png", plot = p_hist, width = 12, height = 8, dpi = 300)
ggsave("map_us_aid_dependency_percentage.png", plot = p_map, width = 12, height = 10, dpi = 300)
ggsave("shock_us_aid_share.png", plot = p_shock, width = 10, height = 6, dpi = 300)
ggsave(
  "map_us_aid_dependency_by_type.png",
  plot = p_map2, width = 12, height = 8, dpi = 300
)
# Summary Statistics Table
pre_share_stats <- pre_shares %>%
  group_by(measure) %>%
  summarise(
    mean = mean(share_pre * 100, na.rm = TRUE),
    median = median(share_pre * 100, na.rm = TRUE),
    sd = sd(share_pre * 100, na.rm = TRUE),
    min = min(share_pre * 100, na.rm = TRUE),
    max = max(share_pre * 100, na.rm = TRUE),
    n = n()
  )
colnames(pre_share_stats)[1:5] <- c("Mean (%)", "Median (%)", "SD (%)", "Min (%)", "Max (%)")

top_dependency <- pre_shares %>%
  arrange(desc(share_pre)) %>%
  group_by(measure) %>%
  slice_head(n = 10) %>%
  mutate(share_pre_pct = share_pre * 100)

# Print the table with percentages
print(pre_share_stats)
print(top_dependency[, c("recipient", "measure", "share_pre_pct")])



# 1) Pick relevant countries (edit if you prefer others)
# dev: Morocco + Egypt (high development food aid dependency)
# hum: Ethiopia + Sudan (major humanitarian recipients)
selected <- tibble::tibble(
  measure = c("Food Aid Development","Food Aid Development",
              "Humanitarian Aid","Humanitarian Aid"),
  recipient_iso3 = c("MAR","EGY","ETH","SDN")
)

# 2) Build country-year US share series
plot_df <- tot_aid_country_africa %>%
  inner_join(selected, by = c("measure","recipient_iso3")) %>%
  mutate(
    share_us = if_else(aid_all > 0, aid_us / aid_all, NA_real_),
    share_us_pct = 100 * share_us
  ) %>%
  filter(!is.na(share_us_pct))

# 3) Set y-axis limits per aid type for readability (optional)
y_limits <- plot_df %>%
  group_by(measure) %>%
  summarise(ymax = max(share_us_pct, na.rm = TRUE), .groups = "drop")

# 4) Plot
p_share_ts <- ggplot(plot_df, aes(x = year, y = share_us_pct, color = recipient)) +
  geom_line(linewidth = 1.0, na.rm = TRUE) +
  facet_wrap(~measure, scales = "free_y") +
  geom_vline(xintercept = 2016, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(
    breaks = seq(1975, 2025, by = 10),
    limits = c(1975, max(plot_df$year, na.rm = TRUE))
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Within-country Evolution of US Aid Share (aid_us / aid_all)",
    subtitle = "Two illustrative countries per aid type; dashed line marks 2016",
    x = "Year",
    y = "US share of total aid",
    color = "Country"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ---------------------------
# 5) Save
# ---------------------------
ggsave(
  "share_us_time_series_selected_countries.png",
  plot = p_share_ts, width = 12, height = 7, dpi = 300
)