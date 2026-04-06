###############################################
#UCDP Georeferenced Event Dataset (GED) Global#
###############################################

library(dplyr)
library(countrycode)
library(ggplot2)
library(knitr)

#Step 1: Download and filter the dataset
########################################
data_africa <- readRDS("original_data_conflict.rds") %>%
  filter(region == "Africa" | country == "Egypt") %>% 
  filter(year > 1994)


#Step 2: Check for patterns in conflict evolution
#################################################
datagood <- data_africa[, c("id", "year", "type_of_violence" , "conflict_name", "dyad_name",
                            "country", "country_id", "region",
                            "date_start", "date_end", "best")]

#Which variable to use as a proxy for conflict intensity

#Step 3: Built our nb of events per country per year variable
#############################################################

events_by_country_year <- datagood %>%
  group_by(country, year) %>%
  summarise(nb_event = n())

#Step 4: Built the relative nb of conflict per country per year
###############################################################

table_mediane_pays <- events_by_country_year %>%
  group_by(country) %>%
  summarise(mediane_nb_conflit = median(nb_event, na.rm = TRUE), .groups = "drop") %>%
  filter(mediane_nb_conflit >= 5) %>%
  arrange(country)

writeLines(
  kable(
    table_mediane_pays,
    format = "latex",
    booktabs = TRUE,
    col.names = c("Country", "Median number of events"),
    caption = "Median number of events by country"
  ),
  "table_mediane_pays.tex"
)

#Keep only countries for which mediane >5 
events_by_country_year <- events_by_country_year %>%
  left_join(table_mediane_pays, by = "country") %>%
  filter(mediane_nb_conflit >= 5)

#build dummy variable either each year nb of conflict is above or below the mediane
events_by_country_year <- events_by_country_year %>%
  mutate(dummy_sup_mediane = if_else(nb_event >= mediane_nb_conflit, 1, 0))

#build our relative nb of event per country per year (5 quintiles)

events_by_country_year <- events_by_country_year %>%
  group_by(country) %>%
  mutate(
    # quintile_nb_event = ntile(nb_event, 5)) %>%
    decile_nb_event = ntile(nb_event, 10)) %>%
    ungroup() %>% 
  mutate(iso3 = countrycode(country, "country.name", "iso3c")) 

write.csv(events_by_country_year,"processed_data_conflict.csv",row.names = FALSE)


#Step 5: Example for reprensentation using Algeria
##################################################

# library(dplyr)
# 
# #1st graph: Nb ov events per year in Algeria
# algeria_data <- events_by_country_year %>%
#   filter(country == "Algeria")
# med_algeria <- unique(algeria_data$mediane_nb_conflit)
# 
# ggplot(algeria_data, aes(x = year, y = nb_event)) +
#   geom_line(linewidth = 0.5) +
#   geom_point(size = 1) +
#   geom_hline(yintercept = med_algeria, linetype = "dashed", linewidth = 0.5, color = "red") +
#   labs(
#     title = "Number of events per year in Algeria",
#     x = "Year",
#     y = "Number of events"
#   ) +
#   theme_minimal()
# 
# #2nd graph: Nb of events quintiles by year in Algeria
# algeria_data <- events_by_country_year %>%
#   filter(country == "Algeria") %>%
#   mutate(quintile_calc = ntile(nb_event, 5))
# 
# quintile_med_algeria <- algeria_data %>%
#   filter(abs(nb_event - mediane_nb_conflit) == min(abs(nb_event - mediane_nb_conflit))) %>%
#   summarise(quintile_mediane = min(quintile_calc)) %>%
#   pull(quintile_mediane)
# 
# quintile_med_algeria
# 
# ggplot(algeria_data, aes(x = year, y = quintile_nb_event)) +
#   geom_line(linewidth = 0.5) +
#   geom_point(size = 1) +
#   geom_hline(
#     yintercept = quintile_med_algeria,
#     color = "red",
#     linetype = "dashed",
#     linewidth = 1
#   ) +
#   scale_y_continuous(breaks = 1:5) +
#   labs(
#     title = "Nb of events quintiles by year in Algeria",
#     x = "Year",
#     y = "Quintile of number of events"
#   ) +
#   theme_minimal()