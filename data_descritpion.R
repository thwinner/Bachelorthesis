# Load packages
library(readr)
library(dplyr)
library(tidyverse)
library(scales)
library(xtable)
library(forcats)

theme_set(theme_bw())

############################################################ DATA DESCRIPTION AGE AND GENDER ##############################

## Tables for gender
tab_per_gender <- table(subset(data, !is.na(gender))$gender)/length(subset(data, !is.na(gender))$gender)
tab_gender <- table(data$gender)
xtable(tab_per_gender*100, digits = 4)
xtable(tab_gender)

## Tables for age
tab_age <- table(data$age)
tab_per_age <- table(subset(data, !is.na(age))$age)/length(subset(data, !is.na(age))$age)
tab_per_agegroup <- table(subset(data, !is.na(age_grouped))$age_grouped)/length(subset(data, !is.na(age_grouped))$age_grouped)
xtable(tab_per_age)
xtable(tab_per_agegroup)


## Plot with age distribution
nrow(data %>% drop_na(age)) 
# -> 56034307

ggplot(subset(data, !is.na(age))) +
  geom_bar(aes(x = age, y = after_stat(prop), group = 1)) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", expression("">=75))) +
  scale_y_continuous(limits = c(0, 0.25), labels = function(x) paste0(x*100, "%")) +
  labs(x = "Age", y = "Relative frequency", caption = paste("n:", 56034307)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
        plot.caption = element_text(size = 12))
ggsave("age.png", width = 8, height = 7.5)


## Plot with age distribution conditioned on gender
nrow(data %>% drop_na(age, gender_grouped))
# -> 54943766

ggplot(subset(data, !is.na(age) & !is.na(gender_grouped))) +
  geom_bar(aes(x = age, y = after_stat(count)/sum(after_stat(count)), fill = gender_grouped), position = position_dodge2()) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", expression("">=75))) + 
  scale_y_continuous(limits = c(0, 0.15), labels = function(x) paste0(x*100, "%")) +
  labs(x = "Age", y = "Relative frequency", caption = paste("n: ", 54943766)) +
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
ggsave("age_gendergrouped.png", width = 8, height = 7.5)

## Plot with age distribution with age groups conditioned on gender 
nrow(data %>% drop_na(age_grouped, gender_grouped))
# -> 54943766

ggplot(subset(data, !is.na(age_grouped) & !is.na(gender_grouped))) +
  geom_bar(aes(x = age_grouped, y = after_stat(count)/sum(after_stat(count)), fill = gender_grouped), position = position_dodge()) +
  scale_y_continuous(limits = c(0, 0.15), labels = function(x) paste0(x*100, "%")) +
  scale_x_discrete(breaks = c("1", "2", "3"),
                   labels = c("18-34", "35-64", expression("">=65))) + 
  labs(x = "Age", y = "Relative frequency") +
  theme(axis.text = element_text(size = 18.5),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 20, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 18.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_fill_manual("Gender", values = c("lightblue", "darkred", "darkgreen"), labels = c("Male", "Female", "Other")) +
  ylim(0, 0.3)
ggsave("agegrouped_gendergrouped.png", width = 8, height = 7.5)


###################################################### PLOTS FOR DEPRESSION #############################################

## Tables for depression
tab_depressed <- table(data$depressed)
tab_per_depressed <- table(subset(data, !is.na(depressed))$depressed)/length(subset(data, !is.na(depressed))$depressed)
xtable(tab_depressed)
xtable(tab_per_depressed)

## Plot with depression distribution
nrow(data %>% drop_na(depressed))
# -> 46573464

ggplot(subset(data, !is.na(depression_flip))) +
  geom_bar(aes(x = as.factor(depression_flip), y = after_stat(count)/sum(after_stat(count)))) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels = c("None of the time", "A little of the time", "Some of the time", "Most of the time", "All the time"),
                   guide = guide_axis(angle = 40)) + 
  labs(x = "Feeling of being depressed", y = "Relative frequency") +
  theme(axis.text = element_text(size = 18.5),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_y_continuous(limits = c(0, 0.6), labels = function(x) paste0(x*100, "%"))
ggsave("bar_depressed.png", width = 8, height = 7.5)

## Plot with grouped depression conditioned on gender

nrow(data %>% drop_na(depressed, gender_grouped))
# -> 43194702

df_gender_dep <- data %>%
  drop_na(depression_group, gender_grouped) %>%
  group_by(gender_grouped) %>%
  dplyr::summarise(n_dep = count(depression_group),
                   n = n()) %>%
  mutate(freq = n_dep$freq / n)

ggplot(df_gender_dep) +
  geom_bar(aes(x = n_dep$x, fill = gender_grouped, y = freq), position = "dodge", stat = "identity") +
  scale_x_discrete(labels = c("None of the time", "Some of the time", "Most of the time"),
                   limits = c("3", "2", "1"),
                   guide = guide_axis(angle = 20)) + 
  labs(x = "Feeling of being depressed", y = "Relative frequency") +
  theme(axis.text = element_text(size = 18.5),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 20, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 18.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_fill_manual("Gender", values = c("lightblue", "darkred", "darkgreen"), labels = c("Male", "Female", "Other")) +
  scale_y_continuous(limits = c(0, 0.7), labels = function(x) paste0(x*100, "%"))
ggsave("depressed_gendergrouped.png", width = 8, height = 7.5)


# Plot with depression conditioned on age groups -- MUSS NOCH GEMACHT WERDEN
nrow(data %>% drop_na(depressed, age_grouped))
# -> 43894177

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
  labs(x = "Feeling of being depressed", y = "Relative frequency", caption = paste("n: ", 43894177)) +
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


## Plot with depression groups conditioned on age groups
df_age_dep <- data %>%
  drop_na(depression_group, age_grouped) %>%
  group_by(age_grouped) %>%
  dplyr::summarise(n_dep = count(depression_group),
                   n = n()) %>%
  mutate(freq = n_dep$freq / n)

# get n:
nrow(data %>% drop_na(depression_group, age_grouped))
# -> 43894177

ggplot(df_age_dep) +
  geom_bar(aes(x = as.factor(n_dep$x), fill = age_grouped, y = freq), position = position_dodge(), stat = "identity") +
  scale_x_discrete(breaks = c("3", "2", "1"),
                   labels = c("None of the time", "Some of the time", "Most of the time"),
                   limits = c("3", "2", "1"),
                   guide = guide_axis(angle = 20)) + 
  labs(x = "Feeling of being depressed", y = "Relative frequency") +
  theme(axis.text = element_text(size = 18.5),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 20, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 18.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_fill_manual("Age", 
                    values = c("darkblue", "cornflowerblue", "deepskyblue"), 
                    labels = c("18-34", "35-64", expression("">=65))) +
  scale_y_continuous(limits = c(0, 0.8), labels = function(x) paste0(x*100, "%"))
ggsave("depressedgrouped_agegrouped.png", width = 8, height = 7.5)


## Plot with depression groups conditioned on continents
df_continent_dep <- data %>%
  drop_na(depression_group, continent) %>%
  group_by(continent) %>%
  dplyr::summarise(n_dep = count(depression_group),
                   n = n()) %>%
  mutate(freq = n_dep$freq / n)

# get n:
nrow(data %>% drop_na(depression_group, continent))
# -> 46573464

ggplot(df_continent_dep) +
  geom_bar(aes(x = as.factor(n_dep$x), fill = continent, y = freq), position = "dodge", stat = "identity") +
  labs(x = "Feeling of being depressed", y = "Relative frequency") +
  theme(axis.text = element_text(size = 18.5),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 20, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 18.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_fill_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_discrete(breaks = c("3", "2", "1"),
                   labels = c("None of the time", "Some of the time", "Most of the time"),
                   limits = c("3", "2", "1")) +
  scale_y_continuous(limits = c(0, 0.8), labels = function(x) paste0(x*100, "%")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
ggsave("depressedgroup_continent.png", width = 10, height = 9)

############################################ PLOTS FOR ANXIETY ######################################################
tab_anxiety <- table(data$anxious)
tab_per_anxiety <- table(subset(data, !is.na(anxious))$anxious)/length(subset(data, !is.na(anxious))$anxious)
xtable(tab_per_anxiety, digits = 2)

## Plot with anxiety distribution
nrow(data %>% drop_na(anxious))
# -> 46461625

ggplot(subset(data, !is.na(anxious_flip))) +
  geom_bar(aes(x = as.factor(anxious_flip), y = after_stat(count)/sum(after_stat(count)))) +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels = c("None of the time", "A little of the time", "Some of the time", "Most of the time", "All the time"),
                   guide = guide_axis(angle = 40)) + 
  labs(x = "Feeling of being anxious", y = "Relative frequency") +
  theme(axis.text = element_text(size = 18.5),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_y_continuous(limits = c(0, 0.6), labels = function(x) paste0(x*100, "%"))
ggsave("bar_anxious.png", width = 8, height = 7.5)

## Plot with anxiety conditioned on gender groups

nrow(data %>% drop_na(anxious, gender_grouped))
# -> 43096483

df_gender_anx <- data %>%
  drop_na(anxious_group, gender_grouped) %>%
  group_by(gender_grouped) %>%
  dplyr::summarise(n_anx = count(anxious_group),
                   n = n()) %>%
  mutate(freq = n_anx$freq / n)

ggplot(df_gender_anx) +
  geom_bar(aes(x = n_anx$x, fill = gender_grouped, y = freq), position = position_dodge(), stat = "identity") +
  scale_y_continuous(labels = label_comma()) +
  scale_x_discrete(labels = c("None of the time", "Some of the time", "Most of the time"),
                   limits = c("3", "2", "1"),
                   guide = guide_axis(angle = 20)) + 
  labs(x = "Feeling of being anxious", y = "Relative frequency") +
  theme(axis.text = element_text(size = 18.5),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 18.5, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 20),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_fill_manual("Gender", values = c("lightblue", "darkred", "darkgreen"), labels = c("Male", "Female", "Other")) +
  scale_y_continuous(limits = c(0, 0.7), labels = function(x) paste0(x*100, "%"))
ggsave("anxious_gendergrouped.png", width = 8, height = 7.5)


## Plot with anxiety groups conditioned on age groups
df_age <- data %>%
  drop_na(anxious_group, age_grouped) %>%
  group_by(age_grouped) %>%
  dplyr::summarise(n_anx = count(anxious_group),
                   n = n()) %>%
  mutate(freq = n_anx$freq / n)

nrow(data %>% drop_na(anxious_group, age_grouped))
# -> 43786804

ggplot(df_age) +
    geom_bar(aes(x = as.factor(n_anx$x), fill = age_grouped, y = freq), position = "dodge", stat = "identity") +
  scale_x_discrete(breaks = c("3", "2", "1"),
                   labels = c("None of the time", "Some of the time", "Most of the time"),
                   limits = c("3", "2", "1"),
                   guide = guide_axis(angle = 20)) + 
    labs(x = "Feeling of being anxious", y = "Relative frequency") +
    theme(axis.text = element_text(size = 18.5),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 20, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 18.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
    scale_fill_manual("Age", values = c("darkblue", "cornflowerblue", "deepskyblue"), 
                      labels = c("18-34", "35-64", expression("">=65))) +
    scale_y_continuous(limits = c(0, 0.8), labels = function(x) paste0(x*100, "%"))
ggsave("anxious_agegrouped.png", width = 8, height = 7.5)


## Plot with anxiety groups conditioned on continents
df_continent <- data %>%
  drop_na(anxious_group, continent) %>%
  group_by(continent) %>%
  dplyr::summarise(n_anx = count(anxious_group),
                   n = n()) %>%
  mutate(freq = n_anx$freq / n)

nrow(data %>% drop_na(anxious_group, continent))
# -> 46461625

ggplot(df_continent) +
  geom_bar(aes(x = as.factor(n_anx$x), fill = continent, y = freq), position = "dodge", stat = "identity") +
  labs(x = "Feeling of being anxious", y = "Relative frequency") +
  theme(axis.text = element_text(size = 18.5),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "top",
        legend.title = element_text(size = 20, face = "bold", margin = margin(r = 15)),
        legend.text = element_text(size = 18.5),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_fill_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_x_discrete(breaks = c("3", "2", "1"),
                   labels = c("None of the time", "Some of the time", "Most of the time"),
                   limits = c("3", "2", "1")) + 
  scale_y_continuous(limits = c(0, 0.8), labels = function(x) paste0(x*100, "%")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
ggsave("anxious_continent.png", width = 10, height = 9)

###################################################### PLOTS FOR CONTINENTS #############################################
tab_per_cont <- table(subset(data, !is.na(continent))$continent)/length(subset(data, !is.na(continent))$continent)
xtable(tab_per_cont, digits = 2)

# Plot with continent distribution
nrow(data %>% drop_na(continent))
# -> 67571490

ggplot(subset(data, !is.na(continent))) +
  geom_bar(aes(x = fct_infreq(continent), y = after_stat(count)/sum(after_stat(count)), fill = continent)) +
  labs(x = "Continents", y = "Relative frequency") +
  theme(axis.text = element_text(size = 18.5),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.position = "none",
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_fill_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666")) +
  scale_y_continuous(limits = c(0, 0.4), labels = function(x) paste0(x*100, "%"))
ggsave("continents.png", width = 8, height = 7.5)

###################################################### COUNTRIES #############################################
df_country <- data %>%
  drop_na(anxious_group, country_agg, depression_group) %>%
  group_by(ISO_3, country_agg) %>%
  dplyr::summarise(anx = count(anxious_group == 1),
                   dep = count(depression_group == 1),
                   n = n(),
                   continent = first(continent)) %>%
  filter(anx$x == TRUE | dep$x == TRUE) %>%
  mutate(freq_anx = anx$freq / n,
         freq_dep = dep$freq / n)

nrow(data %>% drop_na(anxious_group, depression_group, country_agg, continent))
# ->

ggplot(df_country, aes(x = freq_anx, y = freq_dep)) +
  geom_text(aes(label = ISO_3, color = continent), size = 4) +
  geom_abline(intercept = 0, slope = 1) +
  ylim(c(0, 0.3)) +
  xlim(c(0, 0.3)) +
  geom_vline(xintercept = mean(df_country$freq_anx), colour = "darkgrey", linetype = "dashed") +
  annotate("text", x = mean(df_country$freq_anx) + 0.004, y = 0.28, label = "mean anxiety", angle = 90, size = 5, color = "darkgrey") +
  geom_hline(yintercept = mean(df_country$freq_dep), colour = "darkgrey", linetype = "dashed") +
  annotate("text", y = mean(df_country$freq_dep) - 0.006, x = 0.29, label = "mean depression", angle = 0, size = 5, color = "darkgrey") +
  scale_colour_manual("Continent", values = c("#FFCC00", "darkmagenta", "#009933", "#FF9933", "#CC6666"))
  theme(legend.position = "none",
        axis.text = element_text(size = 14.5),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 0, r = 25, b = 0, l = 0)),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  labs(x = "Relative frequency of anxiety", y = "Relative frequency of depression")
ggsave("scatter_country.png", width = 12, height = 8)





###################################################### CORRELATION #############################################
cor(data$anxious, data$depressed, use = "pairwise.complete.obs")
# -> 0.6672727












