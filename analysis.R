# Load packages
library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(xtable)

theme_set(theme_bw())

################################## LINE-PLOTS FOR CONTINENTS ################################

## Line Plot with continent and depression; weekly aggregated
nrow(data %>% drop_na(continent, depressed, week))
# -> 46573464

ggplot(subset(data_weekly_cont, !is.na(continent)), aes(x = week, y = depressed_weekly_flip, colour = continent)) +
  geom_line(size = 1.5) +
  theme(legend.position = "top") +
  labs(x = "Week", y = "Depression", caption = paste("n:", 46573464)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_y_continuous(limits = c(1.5, 2.1), breaks = c(1.5, 1.7, 1.9, 2.1)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
# ggsave("lineplot_week_depmean.png", width = 11, height = 6)

## Line plotwith continent and anxiety; weekly aggregated
nrow(data %>% drop_na(continent, anxious, week))
# -> 46461625

ggplot(subset(data_weekly_cont, !is.na(continent)), aes(x = week, y = anxious_weekly_flip, colour = continent)) +
  geom_line(size = 1.5) +
  theme(legend.position = "top") +
  labs(x = "Week", y = "Anxiety", caption = paste("n:", 46461625)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_y_continuous(limits = c(1.4, 2)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
# ggsave("lineplot_week_anxmean.png", width = 11, height = 6)

### Aggregate data to get percentage of anxious and depressed people
# Group 1 & 2 as depressed / anxious
data_freq_week_group1 <- data %>%
  drop_na(continent, week, anxious, depressed) %>%
  group_by(week, continent) %>%
  dplyr::summarise(n_anx = count(anxious == 1 | anxious == 2),
                   n_dep = count(depressed == 1 | depressed == 2),
                   nall = n()) %>%
  mutate(freq_anx = n_anx$freq / nall,
         freq_dep = n_dep$freq / nall) %>%
  filter(n_anx$x == TRUE | n_dep$x == TRUE)

# group 1, 2 & 3 as depressed / anxious
data_freq_week_group2 <- data %>%
  drop_na(continent, week, anxious, depressed) %>%
  group_by(week, continent) %>%
  dplyr::summarise(n_anx = count(anxious == 1 | anxious == 2 | anxious == 3),
                   n_dep = count(depressed == 1 | depressed == 2 | depressed == 3),
                   nall = n()) %>%
  mutate(freq_anx = n_anx$freq / nall,
         freq_dep = n_dep$freq / nall) %>%
  filter(n_anx$x == TRUE | n_dep$x == TRUE)
  
## Line plot with continent and anxiety; weekly aggregated; group 1 & 2
nrow(data %>% drop_na(continent, anxious, week) %>% filter(anxious == 1 | anxious == 2))
# -> 2859968

ggplot(data_freq_week_group1, aes(x = week, y = freq_anx, colour = continent)) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0, 0.115), breaks = c(0, 0.015, 0.03, 0.05, 0.07, 0.09, 0.11)) +
  theme(legend.position = "top") +
  labs(x = "Month", y = "Relative frequency of anxious people")  +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
# ggsave("lineplot_week_anxgroup1.png", width = 11, height = 6)

## Line plot with continent and depression; weekly aggregated; groups 1 & 2
nrow(data %>% drop_na(continent, depressed, week) %>% filter(depressed == 1 | depressed == 2))
# -> 3750596

ggplot(data_freq_week_group1, aes(x = week, y = freq_dep, colour = continent)) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0, 0.15)) +
  theme(legend.position = "top") +
  labs(x = "Month", y = "Relative frequency of depressed people")  +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
# ggsave("lineplot_week_depgroup1.png", width = 11, height = 6)  

## Line plot with continent and anxiety; weekly aggregated; groups 1, 2 & 3
nrow(data %>% drop_na(continent, anxious, week) %>% filter(anxious == 1 | anxious == 2 | anxious == 3))
# -> 9138280

ggplot(data_freq_week_group2, aes(x = week, y = freq_anx, colour = continent)) +
  geom_line(size = 01.5) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0, 0.3), breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) +
  theme(legend.position = "top") +
  labs(x = "Month", y = "Relative frequency of anxious people")  +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
# ggsave("lineplot_week_anxgroup2.png", width = 11, height = 6)

## Line plot with continent and depression; weekly aggregated; group 1, 2 & 3
nrow(data %>% drop_na(continent, depressed, week) %>% filter(depressed == 1 | depressed == 2 | depressed == 3))
# -> 10468064

ggplot(data_freq_week_group2, aes(x = week, y = freq_dep, colour = continent)) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0, 0.35), breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35)) +
  theme(legend.position = "top") +
  labs(x = "Month", y = "Relative frequency of depressed people")  +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0), limits = as.Date(c("2020-04-01" ,"2022-06-01")))
# ggsave("lineplot_week_depgroup2.png", width = 11, height = 6)  


#################################################################### LINE-PLOTS: AGE GROUPS #################################################################### 

## Aggregate data by week and agegroup
data_week_age <- data %>%
  group_by(week, continent, age_grouped) %>%
  dplyr::summarize(depressed = mean(depressed, na.rm = TRUE),
                   anxious = mean(anxious, na.rm = TRUE),
                   depressed_mean_flip = mean(depression_flip, na.rm = TRUE),
                   anxious_mean_flip = mean(anxious_flip, na.rm = TRUE)) %>%
  as.data.frame()

## Line plot for each age group with continent and anxiety; weekly aggregated
nrow(data %>% drop_na(age, continent, anxious)) # -> 43786804
nrow(data %>% drop_na(age, continent, anxious) %>% filter(age_grouped == "1")) # -> 16278364
nrow(data %>% drop_na(age, continent, anxious) %>% filter(age_grouped == "2")) # -> 23427751
nrow(data %>% drop_na(age, continent, anxious) %>% filter(age_grouped == "3")) # -> 4080689

ggplot(subset(data_week_age, !is.na(age_grouped) & !is.na(continent)), aes(x = week, y = anxious_mean_flip, colour = continent)) +
  geom_line(size = 0.8) +
  theme(legend.position = "top") +
  labs(x = "Week", y = "Anxiety")  +
  facet_wrap(~ age_grouped, labeller = as_labeller(c("1" = "18-34 (n: 16278364)", 
                                                     "2" = "35-64 (n: 23427751)", 
                                                     "3" = "\u2265 65 (n: 4080689)"))) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        strip.text = element_text(size = 14.5),
        strip.background = element_rect(fill = "white")) +
  scale_y_continuous(limits = c(1, 2.2)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%m-%y", expand = c(0, 0))
# ggsave("lineplot_agegroup_anx.png", width = 14, height = 6)  

## Line plot for each agegroup with continent and depression; weekly aggregated
nrow(data %>% drop_na(age, continent, depressed)) # -> 43894177
nrow(data %>% drop_na(age, continent, depressed) %>% filter(age_grouped == "1")) # -> 16325077
nrow(data %>% drop_na(age, continent, depressed) %>% filter(age_grouped == "2")) # -> 23479031
nrow(data %>% drop_na(age, continent, depressed) %>% filter(age_grouped == "3")) # -> 4090069

ggplot(subset(data_week_age, !is.na(age_grouped) & !is.na(continent)), aes(x = week, y = depressed_mean_flip, colour = continent)) +
  geom_line(size = 0.8) +
  theme(legend.position = "top") +
  labs(x = "Week", y = "Depression") +
  facet_wrap(~ age_grouped, labeller = as_labeller(c("1" = "18-34 (n: 16325077)", 
                                                     "2" = "35-64 (n: 23479031)", 
                                                     "3" = "\u2265 65 (n: 4090069)"))) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        strip.text = element_text(size = 14.5),
        strip.background = element_rect(fill = "white")) +
  scale_y_continuous(limits = c(1.2, 2.4), breaks = c(1.2, 1.5, 1.8, 2.1, 2.4)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%m-%y", expand = c(0, 0)) 
# ggsave("lineplot_agegroup_dep.png", width = 14, height = 6)  

################################################################## LINE-PLOTS: GENDER ##################################################################

## Aggregate data by week and gender
data_week_gender <- data %>%
  drop_na(gender_grouped, continent, depressed, anxious, week) %>%
  group_by(gender_grouped, continent, week) %>%
  dplyr::summarise(depressed_mean = mean(depressed, na.rm = TRUE),
                   anxious_mean = mean(anxious, na.rm = TRUE), 
                   depressed_mean_flip = mean(depression_flip, na.rm = TRUE),
                   anxious_mean_flip = mean(anxious_flip, na.rm = TRUE))

## Line plot for each gender with continent and depression; weekly aggregated
nrow(data %>% drop_na(gender_grouped, continent, depressed) %>% filter(gender_grouped == 1)) # -> 22063709
nrow(data %>% drop_na(gender_grouped, continent, depressed) %>% filter(gender_grouped == 2)) # -> 21130993
nrow(data %>% drop_na(gender_grouped, continent, depressed)) # -> 43194702

ggplot(subset(data_week_gender, !is.na(gender_grouped) & !is.na(continent)), aes(x = week, y = depressed_mean_flip, colour = continent)) +
  geom_line(size = 0.8) +
  theme(legend.position = "top") +
  labs(x = "Week", y = "Depression") +
  facet_wrap(~ gender_grouped, labeller = as_labeller(c("1" = "Male (n: 22063709)", "2" = "Female (n: 21130993)"))) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        strip.text = element_text(size = 14.5),
        strip.background = element_rect(fill = "white")) +
  scale_y_continuous(limits = c(1, 2.5)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%m-%y", expand = c(0, 0)) 
# ggsave("lineplot_gendergroup_dep.png", width = 14, height = 6)  

## Line plot for each gender with continent and anxiety; weekly aggregated
nrow(data %>% drop_na(gender_grouped, continent, anxious) %>% filter(gender_grouped == 1)) # -> 22031059
nrow(data %>% drop_na(gender_grouped, continent, anxious) %>% filter(gender_grouped == 2)) # -> 21065424
nrow(data %>% drop_na(gender_grouped, continent, anxious)) # -> 43096483

ggplot(subset(data_week_gender, !is.na(gender_grouped) & !is.na(continent)), aes(x = week, y = anxious_mean_flip, colour = continent)) +
  geom_line(size = 0.8) +
  theme(legend.position = "top") +
  labs(x = "Week", y = "Anxiety") +
  facet_wrap(~ gender_grouped, labeller = as_labeller(c("1" = "Male (n: 22031059)", "2" = "Female (n: 21065424)"))) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        strip.text = element_text(size = 14.5),
        strip.background = element_rect(fill = "white")) +
  scale_y_continuous(limits = c(1, 2.5)) +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%m-%y", expand = c(0, 0)) 
# ggsave("lineplot_gendergroup_anx.png", width = 14, height = 6)  

############################################ HEATMAPS FOR CONTINENTS AND COUNTRIES ####################################################

############################################## Heatmaps for Continents

## Heatmap for continent and depression; monthly aggregated
nrow(data %>% drop_na(month, continent, depressed))
# -> 46573464

ggplot(data_monthly_cont, aes(x = month, y = continent)) +
  geom_tile(aes(fill = depressed_monthly_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 2.5), labels = c(1, 1.5, 2, 2.5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Continent")
# ggsave("heatmap_continents_month_dep.png", width = 11, height = 6)

## Heatmap for continent and anxiety; monthly aggregated
nrow(data %>% drop_na(month, continent, anxious))
# -> 46461625

ggplot(data_monthly_cont, aes(x = month, y = continent)) +
  geom_tile(aes(fill = anxious_monthly_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 2.5), labels = c(1, 1.5, 2, 2.5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Continent")
# ggsave("heatmap_continents_month_anx.png", width = 11, height = 6)

## Heatmap for continent and depression; weekly aggregated
nrow(data %>% drop_na(week, depressed, continent))
# -> 46573464

ggplot(data_weekly_cont, aes(x = week, y = continent)) +
  geom_tile(aes(fill = depressed_weekly_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 2.5), labels = c(1, 1.5, 2, 2.5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month")
# ggsave("heatmap_continents_week_dep.png", width = 11, height = 6)

## Heatmap for continent and anxiety; weekly aggregated
nrow(data %>% drop_na(week, anxious, continent))
# -> 46461625

ggplot(data_weekly_cont, aes(x = week, y = continent)) +
  geom_tile(aes(fill = anxious_weekly_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 2.5), labels = c(1, 1.5, 2, 2.5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Continent")
# ggsave("heatmap_continents_week_anx.png", width = 11, height = 6)

############################################## Heatmaps for each country

############ Africa
data_week_africa <- data %>%
  filter(continent == "Africa") %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(week, country_agg) %>%
  dplyr::summarize(depressed_week = mean(depressed, na.rm = TRUE),
                   anxious_week = mean(anxious, na.rm = TRUE),
                   depressed_week_flip = mean(depression_flip, na.rm = TRUE),
                   anxious_week_flip = mean(anxious_flip, na.rm = TRUE)) %>%
  as.data.frame()

## Heatmap for countries of Africa and depression; weekly aggregated
nrow(data %>% drop_na(country_agg, week, continent, depressed) %>% filter(continent == "Africa"))
# -> 2576735

ggplot(data_week_africa, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = depressed_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 5), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Country")
# ggsave("heatmap_africa_week_dep.png", width = 13, height = 15)

## Heatmap for countries of Africa and anxiety; weekly aggregated
nrow(data %>% drop_na(country_agg, month, continent, anxious) %>% filter(continent == "Africa"))
# -> 2572931

ggplot(data_week_africa, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = anxious_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 5), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Country")
# ggsave("heatmap_africa_week_anx.png", width = 13, height = 15)


############ America
data_week_america <- data %>%
  filter(continent == "America") %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(week, country_agg) %>%
  dplyr::summarize(depressed_week = mean(depressed, na.rm = TRUE),
                   anxious_week = mean(anxious, na.rm = TRUE),
                   depressed_week_flip = mean(depression_flip, na.rm = TRUE),
                   anxious_week_flip = mean(anxious_flip, na.rm = TRUE))

## Heatmap for countries of America and depression; weekly aggregated
nrow(data %>% filter(continent == "America") %>% drop_na(country_agg, month, continent, depressed))
# -> 17145966

ggplot(data_week_america, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = depressed_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 5), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Country")
# ggsave("heatmap_america_week_dep.png", width = 13, height = 14)

## Heatmap for countries of America and anxiety; weekly aggregated
nrow(data %>% drop_na(country_agg, month, continent, anxious) %>% filter(continent == "America"))
# -> 17080937

ggplot(data_week_america, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = anxious_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 5), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Country")
# ggsave("heatmap_america_week_anx.png", width = 13, height = 14)

############ Europe

data_week_europe <- data %>%
  filter(continent == "Europe") %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(week, country_agg) %>%
  dplyr::summarize(depressed_week = mean(depressed, na.rm = TRUE),
                   anxious_week = mean(anxious, na.rm = TRUE),
                   depressed_week_flip = mean(depression_flip, na.rm = TRUE),
                   anxious_week_flip = mean(anxious_flip, na.rm = TRUE))

## Heatmap for countries of Europe and depression; weekly aggregated
nrow(data %>% filter(continent == "Europe") %>% drop_na(country_agg, week, continent, depressed))
# -> 10811270

ggplot(data_week_europe, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = depressed_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 5), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Country")
# ggsave("heatmap_europe_week_dep.png", width = 13, height = 13)

## Heatmap for countries of Europe and anxiety; weekly aggregated
nrow(data %>% filter(continent == "Europe") %>% drop_na(country_agg, week, continent, anxious))
# -> 10798309

ggplot(data_week_europe, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = anxious_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 5), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0))  +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Country")
# ggsave("heatmap_europe_week_anx.png", width = 13, height = 13)

############ Oceania
data_week_oceania <- data %>%
  filter(continent == "Oceania") %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(week, country_agg) %>%
  dplyr::summarize(depressed_week = mean(depressed, na.rm = TRUE),
                   anxious_week = mean(anxious, na.rm = TRUE),
                   depressed_week_flip = mean(depression_flip, na.rm = TRUE),
                   anxious_week_flip = mean(anxious_flip, na.rm = TRUE))

## Heatmap for countries of Oceania and depression; weekly aggregated
nrow(data %>% drop_na(country_agg, month, continent, depressed) %>% filter(continent == "Oceania"))
# -> 1067921

ggplot(data_week_oceania, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = depressed_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 5), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Country")
# ggsave("heatmap_australia_week_dep.png", width = 13, height = 10)

## Heatmap for countries of Oceania and anxiety; weekly aggregated
nrow(data %>% drop_na(country_agg, month, continent, anxious) %>% filter(continent == "Oceania"))
# -> 1064644

ggplot(data_week_oceania, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = anxious_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 5), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Country")
# ggsave("heatmap_australia_week_anx.png", width = 13, height = 10)

############ Asia
data_week_asia <- data %>%
  filter(continent == "Asia") %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(week, country_agg) %>%
  dplyr::summarize(depressed_week = mean(depressed, na.rm = TRUE),
                   anxious_week = mean(anxious, na.rm = TRUE),
                   depressed_week_flip = mean(depression_flip, na.rm = TRUE),
                   anxious_week_flip = mean(anxious_flip, na.rm = TRUE))

## Heatmap for countries of Asia and depression; weekly aggregated
nrow(data %>% drop_na(country_agg, week, continent, depressed) %>% filter(continent == "Asia"))
# -> 14971572

ggplot(data_week_asia, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = depressed_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 5), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Country")
# ggsave("heatmap_asia_week_dep.png", width = 13, height = 10)

## Heatmap for countries of Asia and anxiety; weekly aggregated
nrow(data %>% drop_na(country_agg, week, continent, anxious) %>% filter(continent == "Asia"))
# -> 14944804

ggplot(data_week_asia, aes(x = week, y = country_agg)) +
  geom_tile(aes(fill = anxious_week_flip), color = "white", lwd = 0.4, linetype = 1) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(1, 5), labels = c(1, 2, 3, 4, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", limits = as.Date(c("2020-04-01" ,"2022-06-01")), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))  +
  guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
  theme(axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 14.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Month", y = "Country")
# ggsave("heatmap_asia_week_anx.png", width = 13, height = 10)


#################################################################### COUNTRY ANALYSIS #################################################################### 

#### Calculate the mean value, variance and interquantilsrange (IQR) of weekly aggregated data for each country
## 1) With depression and anxiety levels
dt_country <- data %>%
  drop_na(week, country_agg, anxious, depressed) %>%
  group_by(country_agg, week) %>%
  dplyr::mutate(mean_weekly_dep_flip = mean(depression_flip, na.rm = TRUE),
                mean_weekly_anx_flip = mean(anxious_flip, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(country_agg) %>%
  dplyr::summarize(n = n(),
                   mean_dep = mean(mean_weekly_dep_flip),
                   depressed_var = var(mean_weekly_dep_flip, na.rm = TRUE),
                   depressed_iqr = IQR(mean_weekly_dep_flip, na.rm = TRUE),
                   mean_anx = mean(mean_weekly_anx_flip),
                   anxiety_var = var(mean_weekly_anx_flip, na.rm = TRUE),
                   anxiety_iqr = IQR(mean_weekly_anx_flip, na.rm = TRUE),
                   continent = first(continent),
                   iso = first(ISO_3)) %>%
  ungroup() %>%
  arrange(desc(n))
  
# 2) With relative frequency of depressed and anxious people, where someone is depressed or anxious if all, most or some of the time
dt_country_freq <- data %>%
    drop_na(week, country_agg, anxious, depressed) %>%
    group_by(country_agg, week) %>%
    dplyr::summarise(n_anx = sum(anxious == 1 | anxious == 2 | anxious == 3),
                  n_dep = sum(depressed == 1 | depressed == 2 | depressed == 3),
                  n_all = n(),
                  freq_anx = n_anx / n_all,
                  freq_dep = n_dep / n_all) %>%
    ungroup() %>%
    group_by(country_agg) %>%
    dplyr::summarize(mean_dep = mean(freq_dep),
                     depressed_var = var(freq_dep, na.rm = TRUE),
                     depressed_iqr = IQR(freq_dep, na.rm = TRUE),
                     mean_anx = mean(freq_anx),
                     anxiety_var = var(freq_anx, na.rm = TRUE),
                     anxiety_iqr = IQR(freq_anx, na.rm = TRUE),
                     n = sum(n_all)) %>%
    ungroup() %>%
    arrange(desc(n))

xtable(dt_country)
xtable(dt_country_freq)

#### Subset countries that have more than 5000 observations
dt_country_5000 <- dt_country %>%
  filter(n >= 5000)

dt_country_freq_5000 <- dt_country_freq %>%
  filter(n >= 5000)

nrow(dt_country_5000)
# -> 113 countries are left

######################## Depression

## Find countries that have highest and lowest increase of variance
dt_country_dep_var <- dt_country_5000 %>%
  arrange(desc(depressed_var)) %>%
  select(country_agg, n, mean_dep, depressed_var, depressed_iqr)

dt_country_dep_var_freq <- dt_country_freq_5000 %>%
  arrange(desc(depressed_var)) %>%
  select(country_agg, n, mean_dep, depressed_var, depressed_iqr)

xtable(dt_country_dep_var)
xtable(dt_country_dep_var_freq)

## Find countries that have highest and lowest increase in Interquartilsrange
dt_country_dep_iqr <- dt_country_5000 %>%
    arrange(desc(depressed_iqr)) %>%
  select(country_agg, n, mean_dep, depressed_var, depressed_iqr)

dt_country_dep_iqr_freq <- dt_country_freq_5000 %>%
  arrange(desc(depressed_iqr)) %>%
  select(country_agg, n, mean_dep, depressed_var, depressed_iqr)

xtable(dt_country_dep_iqr)
xtable(dt_country_dep_iqr_freq)

######################## Anxiety

## Find countries that have highest and lowest increase of variance
dt_country_anx_var <- dt_country_5000 %>%
  arrange(desc(anxiety_var))  %>%
  select(country_agg, n, mean_anx, anxiety_var, anxiety_iqr)

dt_country_anx_var_freq <- dt_country_freq_5000 %>%
  arrange(desc(anxiety_var))  %>%
  select(country_agg, n, mean_anx, anxiety_var, anxiety_iqr)

xtable(dt_country_anx_var)
xtable(dt_country_anx_var_freq)

## Find countries that have highest and lowest increase in Interquartilsrange
dt_country_anx_iqr <- dt_country_5000 %>%
  arrange(desc(anxiety_iqr)) %>%
  select(country_agg, n, mean_anx, anxiety_var, anxiety_iqr)

dt_country_anx_iqr_freq <- dt_country_freq_5000 %>%
  arrange(desc(anxiety_iqr)) %>%
  select(country_agg, n, mean_anx, anxiety_var, anxiety_iqr)

xtable(dt_country_anx_iqr)
xtable(dt_country_anx_iqr_freq)

## Correlation between mean and variance
cor(dt_country_5000$anxiety_var, dt_country_5000$mean_anx)
cor(dt_country_5000$depressed_var, dt_country_5000$mean_dep)



#################################################################### NOTES ####################################################################
################################## find smart way to select countries to look at in detail ############################

# ## Quantiles
# summary(data$depressed)
# summary(data$anxious)
# # -> would end up in using all quantiles -> doesn't make any sense
# 
# ## look at countries that have depression lower than 3.8 or higher than 4.5 -> 35 countries
# dt_month_dep_min <- data %>%
#   drop_na(month, country_agg, anxious, depressed) %>%
#   group_by(country_agg, month) %>%
#   dplyr::summarize(depressed_month = mean(depressed, na.rm = TRUE),
#                    anxious_month = mean(anxious, na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(country_agg) %>%
#   filter(mean(depressed_month) <= 3.8 | mean(depressed_month) >= 4.5)
# 
# length(unique(dt_month_dep_min$country_agg))
# 
# ggplot(dt_month_dep_min, aes(x = month, y = country_agg)) +
#   geom_tile(aes(fill = depressed_month), color = "white", lwd = 0.4, linetype = 1) +
#   scale_fill_gradientn(colors = c("red", "white", "lightgreen"), limits = c(1, 5), breaks = c(1, 2, 3, 4, 5)) +
#   scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
#   scale_y_discrete(expand = c(0, 0))  +
#   guides(fill = guide_colourbar(title = "Depression", barwidth = 15)) +
#   theme(axis.text = element_text(size = 12),
#         axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
#         legend.position = "top",
#         legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
#         legend.text = element_text(size = 12),
#         legend.direction = "horizontal",
#         legend.key.width = unit(2, "cm"),
#         plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
#         plot.caption = element_text(size = 12)) +
#   labs(x = "Month", y = "Country", caption = paste("n:", 1078869))
# labs(x = "Month", y = "Country")
# 
# ## look at countries that have anxiety levels lower than 2 or higher than 4.5 -> 15 countries
# dt_month_anx_min <- data %>%
#   drop_na(month, country_agg, anxious, depressed) %>%
#   group_by(country_agg, month) %>%
#   dplyr::summarize(depressed_month = mean(depressed, na.rm = TRUE),
#                    anxious_month = mean(anxious, na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(country_agg) %>%
#   filter(mean(anxious_month) <= 3.8 | mean(anxious_month) >= 4.5)
# 
# length(unique(dt_month_anx_min$country_agg))
# 
# ggplot(dt_month_anx_min, aes(x = month, y = country_agg)) +
#   geom_tile(aes(fill = anxious_month), color = "white", lwd = 0.4, linetype = 1) +
#   scale_fill_gradientn(colors = c("red", "white", "lightgreen"), limits = c(1, 5), breaks = c(1, 2, 3, 4, 5)) +
#   scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
#   scale_y_discrete(expand = c(0, 0))  +
#   guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
#   theme(axis.text = element_text(size = 12),
#         axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
#         legend.position = "top",
#         legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
#         legend.text = element_text(size = 12),
#         legend.direction = "horizontal",
#         legend.key.width = unit(2, "cm"),
#         plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
#         plot.caption = element_text(size = 12)) +
#   labs(x = "Month", y = "Country", caption = paste("n:", 1078869))
# labs(x = "Month", y = "Country")
# 
# ## countries with highest increase in anxiety & depression levels
# dt_decrease_dep <- data %>%
#   drop_na(month, country_agg, anxious, depressed) %>%
#   group_by(country_agg, month) %>%
#   dplyr::summarize(depressed_month = mean(depressed, na.rm = TRUE),
#                    anxious_month = mean(anxious, na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(country_agg) %>%
#   mutate(diff_dep = max(depressed_month) - min(depressed_month),
#          diff_anx = max(anxious_month) - min(anxious_month)) %>%
#   filter(diff_dep >= 2) 
# 
# ggplot(dt_decrease_dep, aes(x = month, y = country_agg)) +
#   geom_tile(aes(fill = depressed_month), color = "white", lwd = 0.4, linetype = 1) +
#   scale_fill_gradientn(colors = c("red", "white", "lightgreen"), limits = c(1, 5), breaks = c(1, 2, 3, 4, 5)) +
#   scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
#   scale_y_discrete(expand = c(0, 0))  +
#   guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
#   theme(axis.text = element_text(size = 12),
#         axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
#         legend.position = "top",
#         legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
#         legend.text = element_text(size = 12),
#         legend.direction = "horizontal",
#         legend.key.width = unit(2, "cm"),
#         plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
#         plot.caption = element_text(size = 12)) +
#   labs(x = "Month", y = "Country", caption = paste("n:", 1078869))
# labs(x = "Month", y = "Country")
# 
# dt_decrease_anx <- data %>%
#   drop_na(month, country_agg, anxious, depressed) %>%
#   group_by(country_agg, month) %>%
#   dplyr::summarize(depressed_month = mean(depressed, na.rm = TRUE),
#                    anxious_month = mean(anxious, na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(country_agg) %>%
#   mutate(diff_dep = max(depressed_month) - min(depressed_month),
#          diff_anx = max(anxious_month) - min(anxious_month)) %>%
#   filter(diff_anx >= 2) 
# 
# ggplot(dt_decrease_anx, aes(x = month, y = country_agg)) +
#   geom_tile(aes(fill = anxious_month), color = "white", lwd = 0.4, linetype = 1) +
#   scale_fill_gradientn(colors = c("red", "white", "lightgreen"), limits = c(1, 5), breaks = c(1, 2, 3, 4, 5)) +
#   scale_x_date(date_breaks = "2 months", date_labels = "%m-%y", expand = c(0,0))  +
#   scale_y_discrete(expand = c(0, 0))  +
#   guides(fill = guide_colourbar(title = "Anxiety", barwidth = 15)) +
#   theme(axis.text = element_text(size = 12),
#         axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
#         legend.position = "top",
#         legend.title = element_text(size = 14, face = "bold", margin = margin(r = 15)),
#         legend.text = element_text(size = 12),
#         legend.direction = "horizontal",
#         legend.key.width = unit(2, "cm"),
#         plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
#         plot.caption = element_text(size = 12)) +
#   labs(x = "Month", y = "Country", caption = paste("n:", 1078869))
# labs(x = "Month", y = "Country")
# 
#   