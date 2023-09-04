# Packages laden
library(sf)
library(broom)
library(readr)
library(dplyr)
library(tidyverse)

#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

############################################# Prepare data for maps #####################################

# Get coordinates from countries
world <- map_data("world")

data$depressed <- as.numeric(data$depressed)
data$anxious <- as.numeric(data$anxious)

# Merge "world"-data and CTIS-data
data_shape <- data %>%
  select(country_region_numeric, country_agg, continent, continent_region, depression_flip, anxious_flip, A2_2_1, ISO_3) %>%
  group_by(ISO_3) %>%
  dplyr::summarize(mean_region_depressed = mean(depression_flip, na.rm = TRUE),
                   mean_region_anxious = mean(anxious_flip, na.rm = TRUE),
                   country_agg = first(country_agg),
                   continent = first(continent),
                   continent_region = first(continent_region))

map_data <- full_join(world, data_shape, by = c("region" = "country_agg"))
map_data <- map_data[!map_data$region == "Antarctica",]


############################################# Maps ############################
theme_set(theme_classic())

# UN-Geoscheme
ggplot(data = map_data) +
  geom_map(map = map_data, aes(long, lat, map_id = region, fill = continent),  
           colour = "black", size = 0.1) +
  xlab("Longitude") + 
  ylab("Latitude") +
  scale_fill_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666"),
                    labels = c("Africa", "America", "Asia", "Europe", "Oceania", "Not Included"), na.value = "gray86") +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))+
  guides(fill = guide_legend(ncol = 6))
ggsave("continent_geoscheme.png", width = 11, height = 6)

# Depression over continents
nrow(data %>% drop_na(continent, depressed))
# -> 46573464

ggplot(data = map_data) +
  geom_map(map = map_data, aes(long, lat, map_id = region, fill = mean_region_depressed),  
           colour = "black", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = "Depression") +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 2.5), na.value = "gray86") +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2.5, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
ggsave("map_depression.png", width = 11, height = 6)

# Anxiety over continents
nrow(data %>% drop_na(continent, anxious))
# -> 46461625

ggplot(data = map_data) +
  geom_map(map = map_data, aes(long, lat, map_id = region, fill = mean_region_anxious),  
           colour = "black", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = "Anxiety") +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 2.5), na.value = "gray86") +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2.5, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
ggsave("map_anxiety.png", width = 11, height = 6)

