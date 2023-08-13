# Load packages
library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(xtable)

theme_set(theme_bw())

################################## Line plot: Depression, Continent ################################

# Line Plot with continent and depression; weekly aggregated
nrow(data %>% drop_na(continent, depressed, week))

ggplot(subset(data_weekly, !is.na(continent)), aes(x = week, y = depressed_weekly, colour = continent)) +
  geom_line(size = 1.5) +
  theme(legend.position = "top") +
  labs(x = "Week", y = "Depression", caption = paste("n:", 46573464)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_y_continuous(limits = c(3.9, 4.8)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
ggsave("lineplot_week_dep.png", width = 11, height = 6)

# Line plotwith continent and anxiety; weekly aggregated
nrow(data %>% drop_na(continent, anxious, week))

ggplot(subset(data_weekly, !is.na(continent)), aes(x = week, y = anxious_weekly, colour = continent)) +
  geom_line(size = 1.5) +
  theme(legend.position = "top") +
  labs(x = "Week", y = "Anxiety", caption = paste("n:", 46461625)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_y_continuous(limits = c(4, 4.7)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
ggsave("lineplot_week_anx.png", width = 11, height = 6)

# Aggregate data to get percentage of anxious and depressed people
# Group 1 & 2 as depressed / anxious
data_freq_week_1 <- data %>%
  drop_na(continent, week, anxious, depressed) %>%
  group_by(week, continent) %>%
  dplyr::summarise(n_anx = count(anxious == 1 | anxious == 2),
                   n_dep = count(depressed == 1 | depressed == 2),
                   nall = n()) %>%
  mutate(freq_anx = n_anx$freq / nall,
         freq_dep = n_dep$freq / nall) %>%
  filter(n_anx$x == TRUE | n_dep$x == TRUE)

# group 1, 2 & 3 as depressed / anxious
data_freq_week_2 <- data %>%
  drop_na(continent, week, anxious, depressed) %>%
  group_by(week, continent) %>%
  dplyr::summarise(n_anx = count(anxious == 1 | anxious == 2 | anxious == 3),
                   n_dep = count(depressed == 1 | depressed == 2 | depressed == 3),
                   nall = n()) %>%
  mutate(freq_anx = n_anx$freq / nall,
         freq_dep = n_dep$freq / nall) %>%
  filter(n_anx$x == TRUE | n_dep$x == TRUE)
  
# Line plot with continent and anxiety; weekly aggregated; group 1 & 2
nrow(data %>% drop_na(continent, anxious, week) %>% filter(anxious == 1 | anxious == 2))

ggplot(data_freq_week_1, aes(x = week, y = freq_anx, colour = continent)) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0.03, 0.115), breaks = c(0.03, 0.05, 0.07, 0.09, 0.11)) +
  theme(legend.position = "top") +
  labs(x = "Month", y = "Relative frequency of anxious people", caption = paste("n:", 2859968))  +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
ggsave("lineplot_week_anxgroup1.png", width = 11, height = 6)

# Line plot with continent and depression; weekly aggregated; groups 1 & 2
nrow(data %>% drop_na(continent, depressed, week) %>% filter(depressed == 1 | depressed == 2))

ggplot(data_freq_week_1, aes(x = week, y = freq_dep, colour = continent)) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0.04, 0.15)) +
  theme(legend.position = "top") +
  labs(x = "Month", y = "Relative frequency of depressed people", caption = paste("n:", 3750596))  +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
ggsave("lineplot_week_depgroup1.png", width = 11, height = 6)  

# Line plot with continent and anxiety; weekly aggregated; groups 1, 2 & 3
nrow(data %>% drop_na(continent, anxious, week) %>% filter(anxious == 1 | anxious == 2 | anxious == 3))

ggplot(data_freq_week_2, aes(x = week, y = freq_anx, colour = continent)) +
  geom_line(size = 01.5) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0.1, 0.3), breaks = c(0.1, 0.15, 0.2, 0.25, 0.3)) +
  theme(legend.position = "top") +
  labs(x = "Month", y = "Relative frequency of anxious people", caption = paste("n:", 9138280))  +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
ggsave("lineplot_week_anxgroup2.png", width = 11, height = 6)

# Line plot with continent and depression; weekly aggregated; group 1, 2 & 3
nrow(data %>% drop_na(continent, depressed, week) %>% filter(depressed == 1 | depressed == 2 | depressed == 3))

ggplot(data_freq_week_2, aes(x = week, y = freq_dep, colour = continent)) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0.125, 0.35)) +
  theme(legend.position = "top") +
  labs(x = "Month", y = "Relative frequency of depressed people", caption = paste("n:", 10468064))  +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
ggsave("lineplot_week_depgroup2.png", width = 11, height = 6)  


#################################################################### Line plot: Age groups #################################################################### 

# Aggregate data by week and agegroup
data_week_age <- data %>%
  group_by(week, continent, age_grouped) %>%
  dplyr::summarize(depressed = mean(depressed, na.rm = TRUE),
                   anxious = mean(anxious, na.rm = TRUE)) %>%
  as.data.frame()

# Line plot for each age group with continent and anxiety; weekly aggregated
nrow(data %>% drop_na(age, continent, anxious))
nrow(data %>% drop_na(age, continent, anxious) %>% filter(age_grouped == "1"))
nrow(data %>% drop_na(age, continent, anxious) %>% filter(age_grouped == "2"))
nrow(data %>% drop_na(age, continent, anxious) %>% filter(age_grouped == "3"))

ggplot(subset(data_week_age, !is.na(age_grouped) & !is.na(continent)), aes(x = week, y = anxious, colour = continent)) +
  geom_line(size = 0.8) +
  theme(legend.position = "top") +
  labs(x = "Week", y = "Anxiety", caption = paste("n:", 43786804))  +
  facet_wrap(~ age_grouped, labeller = as_labeller(c("1" = "18-34 (n: 16278364)", 
                                                     "2" = "35-64 (n: 23427751)", 
                                                     "3" = "\u2265 65 (n: 4080689)"))) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(3.5, 5)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%m-%y", expand = c(0, 0))
ggsave("lineplot_agegroup_anx.png", width = 14, height = 6)  

# Line plot for each agegroup with continent and depression; weekly aggregated
nrow(data %>% drop_na(age, continent, depressed))
nrow(data %>% drop_na(age, continent, depressed) %>% filter(age_grouped == "1"))
nrow(data %>% drop_na(age, continent, depressed) %>% filter(age_grouped == "2"))
nrow(data %>% drop_na(age, continent, depressed) %>% filter(age_grouped == "3"))

ggplot(subset(data_week_age, !is.na(age_grouped) & !is.na(continent)), aes(x = week, y = depressed, colour = continent)) +
  geom_line(size = 0.8) +
  theme(legend.position = "top") +
  labs(x = "Week", y = "Depression", caption = paste("n:", 43894177)) +
  facet_wrap(~ age_grouped, labeller = as_labeller(c("1" = "18-34 (n: 16325077)", 
                                                     "2" = "35-64 (n: 23479031)", 
                                                     "3" = "\u2265 65 (n: 4090069)"))) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(3.5, 5)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%m-%y", expand = c(0, 0)) 
ggsave("lineplot_agegroup_dep.png", width = 14, height = 6)  


################################################################## Heat Maps ####################################################

#### Heatmaps for Continents

# Flip scale for heatmaps: 1 - none of the time; 5 - all the time
data <- data %>%
  mutate(depressed_flipped = depressed*(-1),
         anxious_flipped = anxious*(-1))

data_monthly <- data_monthly %>%
  mutate(depressed_monthly_flip = depressed_monthly*(-1),
         anxious_monthly_flip = anxious_monthly*(-1))

data_weekly <- data_weekly %>%
  mutate(depressed_weekly_flip = depressed_weekly*(-1),
         anxious_weekly_flip = anxious_weekly*(-1))

# Heatmap for continent and depression; monthly aggregated
nrow(data %>% drop_na(month, continent, depressed))

ggplot(data_monthly, aes(x = month, y = continent)) +
  geom_tile(aes(fill = depressed_monthly_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -3.5), labels = c(1, 1.5, 2, 2.5)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Continent", caption = paste("n:", 46573464))
ggsave("heatmap_continents_month_dep.png", width = 8, height = 7.5)

# Heatmap for continent and anxiety; monthly aggregated
nrow(data %>% drop_na(month, continent, anxious))

ggplot(data_monthly, aes(x = month, y = continent)) +
  geom_tile(aes(fill = anxious_monthly_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -3.5), labels = c(1, 1.5, 2, 2.5)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Continent", caption = paste("n:", 46461625))
ggsave("heatmap_continents_month_anx.png", width = 8, height = 7.5)

# Heatmap for continent and depression; weekly aggregated
nrow(data %>% drop_na(week, depressed, continent))

ggplot(data_weekly, aes(x = week, y = continent)) +
  geom_tile(aes(fill = depressed_weekly_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -3.5), labels = c(1, 1.5, 2, 2.5)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Continent", caption = paste("n:", 46573464))
ggsave("heatmap_continents_week_dep.png", width = 8, height = 7.5)

# Heatmap for continent and anxiety; weekly aggregated
nrow(data %>% drop_na(week, anxious, continent))

ggplot(data_weekly, aes(x = week, y = continent)) +
  geom_tile(aes(fill = anxious_weekly_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -3.5), labels = c(1, 1.5, 2, 2.5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Continent", caption = paste("n:", 46461625))
ggsave("heatmap_continents_week_anx.png", width = 8, height = 7.5)

### Heatmaps for each country

## Africa
data_week_africa <- data %>%
  filter(continent == "Africa") %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(week, country_agg) %>%
  dplyr::summarize(depressed_week = mean(depressed, na.rm = TRUE),
                   anxious_week = mean(anxious, na.rm = TRUE)) %>%
  mutate(depressed_week_flip = depressed_week*(-1),
         anxious_week_flip = anxious_week*(-1)) %>%
  as.data.frame()

# Heatmap for countries of Africa and depression; weekly aggregated
nrow(data %>% drop_na(country_agg, week, continent, depressed) %>% filter(continent == "Africa"))

ggplot(data_week_africa, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = depressed_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -1), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 2576735))
ggsave("heatmap_africa_week_dep.png", width = 13, height = 15)

# Heatmap for countries of Africa and anxiety; weekly aggregated
nrow(data %>% drop_na(country_agg, month, continent, anxious) %>% filter(continent == "Africa"))

ggplot(data_week_africa, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = anxious_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -1), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 2572931))
ggsave("heatmap_africa_month_anx.png", width = 13, height = 15)


## America
data_week_america <- data %>%
  filter(continent == "America") %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(week, country_agg) %>%
  dplyr::summarize(depressed_week = mean(depressed, na.rm = TRUE),
                   anxious_week = mean(anxious, na.rm = TRUE)) %>%
  mutate(depressed_week_flip = depressed_week*(-1),
         anxious_week_flip = anxious_week*(-1))

# Heatmap for countries of America and depression; weekly aggregated
nrow(data %>% filter(continent == "America") %>% drop_na(country_agg, month, continent, depressed))

ggplot(data_week_america, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = depressed_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -1), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 17145966))
ggsave("heatmap_america_week_dep.png", width = 13, height = 14)

# Heatmap for countries of America and anxiety; weekly aggregated
nrow(data %>% drop_na(country_agg, month, continent, anxious) %>% filter(continent == "America"))

ggplot(data_week_america, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = anxious_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -1), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 17080937))
ggsave("heatmap_america_week_anx.png", width = 13, height = 14)

## Europe

data_week_europe <- data %>%
  filter(continent == "Europe") %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(week, country_agg) %>%
  dplyr::summarize(depressed_week = mean(depressed, na.rm = TRUE),
                   anxious_week = mean(anxious, na.rm = TRUE)) %>%
  mutate(depressed_week_flip = depressed_week*(-1),
         anxious_week_flip = anxious_week*(-1))

# Heatmap for countries of Europe and depression; weekly aggregated
nrow(data %>% filter(continent == "Europe") %>% drop_na(country_agg, week, continent, depressed))

ggplot(data_week_europe, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = depressed_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -1), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 10811270))
ggsave("heatmap_europe_week_dep.png", width = 13, height = 13)

# Heatmap for countries of Europe and anxiety; weekly aggregated
nrow(data %>% filter(continent == "Europe") %>% drop_na(country_agg, week, continent, anxious))

ggplot(data_week_europe, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = anxious_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -1), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-03-01" ,"2022-07-01")))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 10798309))
ggsave("heatmap_europe_week_anx.png", width = 13, height = 13)

## Oceania
data_week_oceania <- data %>%
  filter(continent == "Oceania") %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(week, country_agg) %>%
  dplyr::summarize(depressed_week = mean(depressed, na.rm = TRUE),
                   anxious_week = mean(anxious, na.rm = TRUE)) %>%
  mutate(depressed_week_flip = depressed_week*(-1),
         anxious_week_flip = anxious_week*(-1))

# Heatmap for countries of Oceania and depression; weekly aggregated
nrow(data %>% drop_na(country_agg, month, continent, depressed) %>% filter(continent == "Oceania"))

ggplot(data_week_oceania, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = depressed_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -1), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 1067921))
ggsave("heatmap_australia_week_dep.png", width = 13, height = 10)

# Heatmap for countries of Oceania and anxiety; weekly aggregated
nrow(data %>% drop_na(country_agg, month, continent, anxious) %>% filter(continent == "Oceania"))

ggplot(data_week_oceania, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = anxious_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -1), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 1064644))
ggsave("heatmap_australia_week_anx.png", width = 13, height = 10)

## Asia
data_week_asia <- data %>%
  filter(continent == "Asia") %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(week, country_agg) %>%
  dplyr::summarize(depressed_week = mean(depressed, na.rm = TRUE),
                   anxious_week = mean(anxious, na.rm = TRUE)) %>%
  mutate(depressed_week_flip = depressed_week*(-1),
         anxious_week_flip = anxious_week*(-1))

# Heatmap for countries of Asia and depression; weekly aggregated
nrow(data %>% drop_na(country_agg, week, continent, depressed) %>% filter(continent == "Asia"))

ggplot(data_week_asia, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = depressed_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -1), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 14971572))
ggsave("heatmap_asia_week_dep.png", width = 13, height = 10)

# Heatmap for countries of Asia and anxiety; weekly aggregated
nrow(data %>% drop_na(country_agg, week, continent, anxious) %>% filter(continent == "Asia"))

ggplot(data_week_asia, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = anxious_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("lightgreen", "white", "red"), limits = c(-5, -1), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-03-01" ,"2022-07-01"))) +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 14944804))
ggsave("heatmap_asia_week_anx.png", width = 13, height = 10)


################################## find smart way to select countries to look at in detail ############################

## Quantiles
summary(data$depressed)
summary(data$anxious)
# -> would end up in using all quantiles -> doesn't make any sense

## look at countries that have depression lower than 3.8 or higher than 4.5 -> 35 countries
dt_month_dep_min <- data %>%
  drop_na(month, country_agg, anxious, depressed) %>%
  group_by(country_agg, month) %>%
  dplyr::summarize(depressed_month = mean(depressed, na.rm = TRUE),
                   anxious_month = mean(anxious, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(country_agg) %>%
  filter(mean(depressed_month) <= 3.8 | mean(depressed_month) >= 4.5)

length(unique(dt_month_dep_min$country_agg))

ggplot(dt_month_dep_min, aes(x = month, y = country_agg)) +
  geom_tile(aes(fill = depressed_month), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("red", "white", "lightgreen"), limits = c(1, 5), breaks = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 1078869))
  labs(x = "Month", y = "Country")

## look at countries that have anxiety levels lower than 2 or higher than 4.5 -> 15 countries
dt_month_anx_min <- data %>%
  drop_na(month, country_agg, anxious, depressed) %>%
  group_by(country_agg, month) %>%
  dplyr::summarize(depressed_month = mean(depressed, na.rm = TRUE),
                   anxious_month = mean(anxious, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(country_agg) %>%
  filter(mean(anxious_month) <= 3.8 | mean(anxious_month) >= 4.5)

length(unique(dt_month_anx_min$country_agg))

ggplot(dt_month_anx_min, aes(x = month, y = country_agg)) +
  geom_tile(aes(fill = anxious_month), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("red", "white", "lightgreen"), limits = c(1, 5), breaks = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 1078869))
  labs(x = "Month", y = "Country")

## countries with highest increase in anxiety & depression levels
dt_decrease_dep <- data %>%
  drop_na(month, country_agg, anxious, depressed) %>%
  group_by(country_agg, month) %>%
  dplyr::summarize(depressed_month = mean(depressed, na.rm = TRUE),
                   anxious_month = mean(anxious, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(country_agg) %>%
  mutate(diff_dep = max(depressed_month) - min(depressed_month),
         diff_anx = max(anxious_month) - min(anxious_month)) %>%
  filter(diff_dep >= 2) 
  
ggplot(dt_decrease_dep, aes(x = month, y = country_agg)) +
  geom_tile(aes(fill = depressed_month), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("red", "white", "lightgreen"), limits = c(1, 5), breaks = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 1078869))
  labs(x = "Month", y = "Country")

dt_decrease_anx <- data %>%
  drop_na(month, country_agg, anxious, depressed) %>%
  group_by(country_agg, month) %>%
  dplyr::summarize(depressed_month = mean(depressed, na.rm = TRUE),
                   anxious_month = mean(anxious, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(country_agg) %>%
  mutate(diff_dep = max(depressed_month) - min(depressed_month),
         diff_anx = max(anxious_month) - min(anxious_month)) %>%
  filter(diff_anx >= 2) 

ggplot(dt_decrease_anx, aes(x = month, y = country_agg)) +
  geom_tile(aes(fill = anxious_month), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("red", "white", "lightgreen"), limits = c(1, 5), breaks = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 1078869))
  labs(x = "Month", y = "Country")


## get countries that have observations in whole time period
# countries that have observations from april 2020 to april 2022

dt_country_time <- data %>%
  group_by(month) %>%
  dplyr::mutate(month_id = cur_group_id()) %>%
  ungroup() %>%
  group_by(country_agg) %>%
  filter(all(1:25 %in% month_id)) %>%
  drop_na(anxious, depressed, month, country_agg) %>%
  group_by(country_agg, month_id, month) %>%
  dplyr::summarize(depressed_month = mean(depressed, na.rm = TRUE),
                   anxious_month = mean(anxious, na.rm = TRUE)) %>%
  filter(month_id %in% 1:25)

length(unique(dt_country_time$country_agg)) # -> 104

ggplot(dt_country_time, aes(x = month, y = country_agg)) +
  geom_tile(aes(fill = depressed_month), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("red", "white", "lightgreen"), breaks = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  labs(x = "Month", y = "Country", caption = paste("n:", 1078869))
  labs(x = "Month", y = "Depression")

#################################################################### Country analysis #################################################################### 

dt_decrease <- data %>%
    drop_na(week, country_agg, anxious, depressed) %>%
    group_by(country_agg, week) %>%
    dplyr::summarise(mean_dep = mean(depressed, na.rm = TRUE),
                     mean_anx = mean(anxious, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(country_agg) %>%
    dplyr::summarise(diff_dep = max(mean_dep) - min(mean_dep),
           diff_anx = max(mean_anx) - min(mean_anx),
           depressed_var = var(mean_dep, na.rm = TRUE),
           anxious_var = var(mean_anx, na.rm = TRUE),
           depressed_iqr = IQR(mean_dep, na.rm = TRUE),
           anxious_iqr = IQR(mean_anx, na.rm = TRUE)) %>%
    ungroup()
  
dt_increase_dep_diff <- dt_decrease %>%
    arrange(desc(diff_dep)) %>%
    slice(1:10)

dt_decrease_dep_diff <- dt_decrease %>%
  arrange(-(desc(diff_dep))) %>%
  slice(1:10)
    
xtable(dt_increase_dep_diff)
xtable(dt_decrease_dep_diff)

dt_increase_dep_var <- dt_decrease %>%
    arrange(desc(depressed_var)) %>%
      slice(1:10)

dt_decrease_dep_var <- dt_decrease %>%
  arrange(-desc(depressed_var)) %>%
  slice(1:10)

xtable(dt_increase_dep_var)
xtable(dt_decrease_dep_var)

dt_increase_dep_iqr <- dt_decrease %>%
    arrange(desc(depressed_iqr)) %>%
      slice(1:10)

dt_decrease_dep_iqr <- dt_decrease %>%
  arrange(-desc(depressed_iqr)) %>%
  slice(1:10)

xtable(dt_increase_dep_iqr)
xtable(dt_decrease_dep_iqr)
  
  
  
  