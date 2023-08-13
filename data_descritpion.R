# Load packages
library(readr)
library(dplyr)
library(tidyverse)
library(scales)
library(xtable)
library(forcats)

theme_set(theme_bw())

### Read data
data <- read_csv("data_prepared.csv")
data[, c("anxious", "depressed", "worry_nutrition", "gender", "age", "area", "worry_finance", "had_covid",
         "infected", "depression_group")] <- lapply(data[, c("anxious", "depressed", "worry_nutrition", "gender", 
                                                             "age", "area", "worry_finance", "had_covid", "infected", 
                                                             "depression_group")] , factor)

########## Gender
tab_per_gender <- table(subset(data, !is.na(gender))$gender)/length(subset(data, !is.na(gender))$gender)
tab_gender <- table(data$gender)
xtable(tab_per_gender*100, digits = 4)
xtable(tab_gender)

########## Age
tab_age <- table(data$age)
tab_per_age <- table(subset(data, !is.na(age))$age)/length(subset(data, !is.na(age))$age)
tab_per_agegroup <- table(subset(data, !is.na(age_grouped))$age_grouped)/length(subset(data, !is.na(age_grouped))$age_grouped)
xtable(tab_per_age)
xtable(tab_per_agegroup)

# get n:
nrow(data %>% drop_na(age))

ggplot(subset(data, !is.na(age))) +
  geom_bar(aes(x = age, y = after_stat(prop), group = 1)) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", expression("">=75))) +
  scale_y_continuous(limits = c(0, 0.25), labels = function(x) paste0(x*100, "%")) +
  labs(x = "Age", y = "Relative frequency", caption = paste("n:", 57508597)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12))
ggsave("plots_description/age.png", width = 8, height = 7.5)

# get n:
nrow(data %>% drop_na(age, gender_grouped))

ggplot(subset(data, !is.na(age) & !is.na(gender_grouped))) +
  geom_bar(aes(x = age, y = after_stat(count)/sum(after_stat(count)), fill = gender_grouped), position = position_dodge2()) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", expression("">=75))) + 
  scale_y_continuous(limits = c(0, 0.15), labels = function(x) paste0(x*100, "%")) +
  labs(x = "Age", y = "Relative frequency", caption = paste("n: ", 56397875)) +
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
  scale_fill_manual("Gender", values = c("lightblue", "darkred", "darkgreen"), labels = c("Male", "Female", "Other"))
ggsave("plots_description/age_gendergrouped.png", width = 8, height = 7.5)

# get n:
nrow(data %>% drop_na(age_grouped, gender_grouped))

ggplot(subset(data, !is.na(age_grouped) & !is.na(gender_grouped))) +
  geom_bar(aes(x = age_grouped, y = after_stat(count)/sum(after_stat(count)), fill = gender_grouped), position = position_dodge()) +
  scale_y_continuous(limits = c(0, 0.15), labels = function(x) paste0(x*100, "%")) +
  scale_x_discrete(breaks = c("1", "2", "3"),
                   labels = c("18-34", "35-64", expression("">=65))) + 
  labs(x = "Age", y = "Relative frequency", caption = paste("n: ", 56397875)) +
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
  scale_fill_manual("Gender", values = c("lightblue", "darkred", "darkgreen"), labels = c("Male", "Female", "Other")) +
  ylim(0, 0.3)
ggsave("agegrouped_gendergrouped.png", width = 8, height = 7.5)


######### Depressed

# get n:
nrow(data %>% drop_na(depressed))

tab_depressed <- table(data$depressed)
tab_per_depressed <- table(subset(data, !is.na(depressed))$depressed)/length(subset(data, !is.na(depressed))$depressed)
xtable(tab_depressed)
xtable(tab_per_depressed)

ggplot(subset(data, !is.na(depressed))) +
  geom_bar(aes(x = as.factor(depressed), y = after_stat(count)/sum(after_stat(count)))) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels = c("All the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
                   guide = guide_axis(angle = 40)) + 
  labs(x = "Feeling of being depressed", y = "Relative frequency", caption = paste("n: ", 47474752)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 0.6), labels = function(x) paste0(x*100, "%"))
ggsave("bar_depressed.png", width = 8, height = 7.5)

# get n:
nrow(data %>% drop_na(depressed, gender_grouped))

data %>%
  drop_na(depressed, gender_grouped) %>%
  group_by(depressed) %>%
  mutate(freq = n / sum(n)) %>%
ggplot() +
  geom_bar(aes(x = depressed, fill = gender_grouped, y = freq), position = "dodge", stat = "identity") +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels = c("All the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
                   guide = guide_axis(angle = 40)) + 
  labs(x = "Feeling of being depressed", y = "Relative frequency", caption = paste("n: ", 44057833)) +
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
  scale_fill_manual("Gender", values = c("lightblue", "darkred", "darkgreen"), labels = c("Male", "Female", "Other")) +
  scale_y_continuous(limits = c(0, 0.7), labels = function(x) paste0(x*100, "%"))
ggsave("depressed_gendergrouped.png", width = 8, height = 7.5)

# get n:
nrow(data %>% drop_na(depressed, age))

ggplot(subset(data, !is.na(depressed) & !is.na(age))) +
  geom_bar(aes(x = depressed, fill = age, y = after_stat(count)/sum(after_stat(count))), position = "dodge") +
  scale_y_continuous(labels = label_comma()) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels = c("All the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
                   guide = guide_axis(angle = 40)) + 
  labs(x = "Feeling of being depressed", y = "Relative frequency") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  scale_fill_manual("Age", 
                    values = c("darkorange", "deeppink3", "lightgreen", "darkviolet", "darksalmon", "deepskyblue4", "aquamarine3"), 
                    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", ">75")) +
  scale_y_continuous(limits = c(0, 0.15), labels = function(x) paste0(x*100, "%"))
ggsave("depressed_age.png", width = 8, height = 7.5)

# get n:
nrow(data %>% drop_na(depressed, age_grouped))

data %>%
  drop_na(depressed, age_grouped) %>%
  dplyr::count(depressed, age_grouped) %>%
  group_by(age_grouped) %>%
  mutate(freq = n / sum(n)) %>%
ggplot() +
  geom_bar(aes(x = depressed, fill = age_grouped, y = freq), position = position_dodge(), stat = "identity") +
  scale_y_continuous(labels = label_comma()) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels = c("All the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
                   guide = guide_axis(angle = 40)) + 
  labs(x = "Feeling of being depressed", y = "Relative frequency") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "top",
        legend.text = element_text(size = 10)) +
  scale_fill_manual("Age", 
                    values = c("darkblue", "cornflowerblue", "deepskyblue"), 
                    labels = c("18-34", "35-64", expression("">=65))) +
  scale_y_continuous(limits = c(0, 0.8), labels = function(x) paste0(x*100, "%"))
ggsave("depressed_agegrouped.png", width = 8, height = 7.5)


df_age_dep <- data %>%
  drop_na(depression_group, age_grouped) %>%
  group_by(age_grouped) %>%
  dplyr::summarise(n_dep = count(depression_group),
                   n = n()) %>%
  mutate(freq = n_dep$freq / n)

# get n:
nrow(data %>% drop_na(depression_group, age_grouped))

ggplot(df_age_dep) +
  geom_bar(aes(x = as.factor(n_dep$x), fill = age_grouped, y = freq), position = position_dodge(), stat = "identity") +
  scale_x_discrete(breaks = c("1", "2", "3"),
                   labels = c("Most of the time", "Some of the time", "None of the time")) + 
  labs(x = "Feeling of being depressed", y = "Relative frequency", caption = "n: 44766693") +
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
  scale_fill_manual("Age", 
                    values = c("darkblue", "cornflowerblue", "deepskyblue"), 
                    labels = c("18-34", "35-64", expression("">=65))) +
  scale_y_continuous(limits = c(0, 0.8), labels = function(x) paste0(x*100, "%"))
ggsave("depressedgrouped_agegrouped.png", width = 8, height = 7.5)

df_continent_dep <- data %>%
  drop_na(depression_group, continent) %>%
  group_by(continent) %>%
  dplyr::summarise(n_dep = count(depression_group),
                   n = n()) %>%
  mutate(freq = n_dep$freq / n)

# get n:
nrow(data %>% drop_na(depression_group, continent))

ggplot(df_continent_dep) +
  geom_bar(aes(x = as.factor(n_dep$x), fill = continent, y = freq), position = "dodge", stat = "identity") +
  labs(x = "Feeling of being depressed", y = "Relative frequency", caption = "n: 47259805") +
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
  scale_fill_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_discrete(breaks = c("1", "2", "3"),
                   labels = c("Most of the time", "Some of the time", "None of the time")) +
  scale_y_continuous(limits = c(0, 0.7), labels = function(x) paste0(x*100, "%"))
ggsave("depressed_continent.png", width = 10, height = 7.5)

######### Anxiety
tab_anxiety <- table(data$anxious)
tab_per_anxiety <- table(subset(data, !is.na(anxious))$anxious)/length(subset(data, !is.na(anxious))$anxious)

# get n:
nrow(data %>% drop_na(anxious))

ggplot(subset(data, !is.na(anxious))) +
  geom_bar(aes(x = as.factor(anxious), y = after_stat(count)/sum(after_stat(count)))) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels = c("All the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
                   guide = guide_axis(angle = 40)) + 
  labs(x = "Feeling of being anxious", y = "Relative frequency", caption = "n: 47361703") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 0.6), labels = function(x) paste0(x*100, "%"))
ggsave("anxious.png", width = 8, height = 7.5)

data %>%
  drop_na(anxious, gender_grouped) %>%
  count(anxious, gender_grouped) %>%
  group_by(gender_grouped) %>%
  mutate(freq = n / sum(n)) %>%
ggplot() +
  geom_bar(aes(x = anxious, fill = gender_grouped, y = freq), position = position_dodge(), stat = "identity") +
  scale_y_continuous(labels = label_comma()) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels = c("All the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
                   guide = guide_axis(angle = 40)) + 
  labs(x = "Feeling of being anxious", y = "Relative frequency") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  scale_fill_manual("Gender", values = c("lightblue", "darkred", "darkgreen"), labels = c("Male", "Female", "Other")) +
  scale_y_continuous(limits = c(0, 0.7), labels = function(x) paste0(x*100, "%"))
ggsave("anxious_gendergrouped.png", width = 8, height = 7.5)

ggplot(subset(data, !is.na(anxious) & !is.na(age))) +
  geom_bar(aes(x = anxious, fill = age, y = after_stat(count)/sum(after_stat(count))), position = "dodge") +
  scale_y_continuous(labels = label_comma()) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels = c("All the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
                   guide = guide_axis(angle = 40)) + 
  labs(x = "Feeling of being anxious", y = "Relative frequency") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  scale_fill_manual("Age", 
                    values = c("darkorange", "deeppink3", "lightgreen", "darkviolet", "darksalmon", "deepskyblue4", "aquamarine3"), 
                    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", ">75")) +
  scale_y_continuous(limits = c(0, 0.15), labels = function(x) paste0(x*100, "%"))
ggsave("anxious_age.png", width = 8, height = 7.5)


df_age <- data %>%
  drop_na(anxious_group, age_grouped) %>%
  group_by(age_grouped) %>%
  dplyr::summarise(n_anx = count(anxious_group),
                   n = n()) %>%
  mutate(freq = n_anx$freq / n)

# get n:
nrow(data %>% drop_na(anxious_group, age_grouped))

ggplot(df_age) +
    geom_bar(aes(x = as.factor(n_anx$x), fill = age_grouped, y = freq), position = "dodge", stat = "identity") +
    scale_x_discrete(breaks = c("1", "2", "3"),
                     labels = c("Most of the time", "Some of the time", "None of the time")) + 
    labs(x = "Feeling of being anxious", y = "Relative frequency", caption = "n: 44658264") +
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
    scale_fill_manual("Age", values = c("darkblue", "cornflowerblue", "deepskyblue"), 
                      labels = c("18-34", "35-64", expression("">=65))) +
    scale_y_continuous(limits = c(0, 0.75), labels = function(x) paste0(x*100, "%"))
ggsave("anxious_agegrouped.png", width = 8, height = 7.5)

df_continent <- data %>%
  drop_na(anxious_group, continent) %>%
  group_by(continent) %>%
  dplyr::summarise(n_anx = count(anxious_group),
                   n = n()) %>%
  mutate(freq = n_anx$freq / n)

# get n:
nrow(data %>% drop_na(anxious_group, continent))

ggplot(df_continent) +
  geom_bar(aes(x = as.factor(n_anx$x), fill = continent, y = freq), position = "dodge", stat = "identity") +
  labs(x = "Feeling of being anxious", y = "Relative frequency", caption = "n: 47147353") +
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
  scale_fill_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_discrete(breaks = c("1", "2", "3"),
                   labels = c("Most of the time", "Some of the time", "None of the time")) +
  scale_y_continuous(limits = c(0, 0.8), labels = function(x) paste0(x*100, "%"))
ggsave("anxious_continent.png", width = 10, height = 7.5)

######### Continent
tab_per_cont <- table(subset(data, !is.na(continent))$continent)/length(subset(data, !is.na(continent))$continent)

# get n: 
nrow(data %>% drop_na(continent))

ggplot(subset(data, !is.na(continent))) +
  geom_bar(aes(x = fct_infreq(continent), y = after_stat(count)/sum(after_stat(count)), fill = continent)) +
  labs(x = "Continents", y = "Relative frequency", caption = paste("n: 68918710")) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "none",
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12)) +
  scale_fill_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_y_continuous(limits = c(0, 0.4), labels = function(x) paste0(x*100, "%"))
ggsave("continents.png", width = 8, height = 7.5)


