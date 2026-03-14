###############################################
#UCDP Georeferenced Event Dataset (GED) Global#
###############################################

library(dplyr)
library(ggplot2)

#Step 1: Download and filter the dataset
########################################
data_africa <- readRDS("original_data_conflict.rds") %>%
  filter(region == "Africa" | country == "Egypt")

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

data_selected_countries <- events_by_country_year %>%
  filter(country %in% c("Ethiopia",
                        "Egypt",
                        "Sudan",
                        "Morocco",
                        "Mozambique",
                        "South Sudan",
                        "Somalia",
                        "DR Congo (Zaire)",
                        "Chad",
                        "Liberia"))


# data_selected_countries <- events_by_country_year %>%
#   filter(country %in% c("Ethiopia",
#                         "Egypt",
#                         "Sudan",
#                         "Morocco",
#                         "Mozambique",
#                         "South Sudan",
#                         "Somalia",
#                         "DR Congo (Zaire)", 
#                         "Chad",
#                         "Liberia",
#                         "Ghana",
#                         "Namibia",
#                         "Rwanda",
#                         "Madagascar",
#                         "Ouganda",
#                         "Cameroun",
#                         "Sierra Leone",
#                         "Zimbabwe",
#                         "Angola",
#                         "Mauritania"))


write.csv(data_selected_countries,
          "processed_data_conflict.csv",
          row.names = FALSE)


# 
# ggplot(data_selected_countries, aes(x = year, y = nb_event, group = country, color = country)) +
#   geom_line(linewidth = 0.5) +
#   geom_point(size = 1) +
#   labs(
#     x = "Years",
#     y = "Nb of events",
#     color = "Countries"
#   ) +
#   theme_minimal()
# 
# 
# 
# #Step 3: Built our nb of death per country per year variable
# ############################################################
# deaths_by_country_year <- datagood %>%
#   group_by(country, year) %>%
#   summarise(nb_deaths = sum(best, na.rm = TRUE))
# 
# death_selected_countries <- deaths_by_country_year %>%
#   filter(country %in% c("Ethiopia",
#                         "Egypt",
#                         "Sudan",
#                         "Morocco",
#                         "Mozambique",
#                         "South Sudan",
#                         "Somalia",
#                         "DR Congo (Zaire)"))
# 
# ggplot(death_selected_countries, aes(x = year, y = nb_deaths, group = country, color = country)) +
#   geom_line(linewidth = 0.5) +
#   geom_point(size = 1) +
#   labs(
#     x = "Years",
#     y = "Nb of deaths",
#     color = "Countries"
#   ) +
#   theme_minimal()
