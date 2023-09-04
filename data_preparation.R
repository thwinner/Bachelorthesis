# Load packages
library(readr)
library(tidyverse)
library(data.table)
library(plyr)
library(dplyr)

########################################## READ DATA AND PREPARE DATAFRAME ##########################################
## Put single files into one big dataframe and select only variables of interest
#files <- list.files(path = "data", recursive = TRUE, pattern = ".csv*", full.names = TRUE)

# data <- rbind.fill(lapply(files, function(x) {
#   fread(x, select = c("survey_region", "A2_2_1",  "A2_2_2", "B0", "B8a", "C14a", "D1", "D2", "D4", "D5", "V10_1",
#                       "V10_2", "V10_3", "V10_4", "V10_5", "V10_6", "V10_7", "V10_8", "V10_9", "V10_10",
#                       "E3", "E4", "E2", "E5", "country_region_numeric", "region_agg", "country_agg"),
#         stringsAsFactors = TRUE, fill = TRUE, data.table = FALSE)}))
# 
# write.csv(data, "data.csv", quote = FALSE, row.names = FALSE)


######### Read data

# selected_cols <- c("survey_region", "A2_2_1",  "A2_2_2", "B0", "B8a", "C14a", "D1", "D2", "D4", "D5", "V10_1",
#                    "V10_2", "V10_3", "V10_4", "V10_5", "V10_6", "V10_7", "V10_8", "V10_9", "V10_10",
#                    "E3", "E4", "E2", "E5", "country_region_numeric", "region_agg", "country_agg", "ISO_3", 
#                    "RecordedDate", "GID_0")
# dt_all <- data.frame()
# 
# for (file in files) {
#   print(file)
#   dt <- readr::read_csv(file = file, col_names = TRUE,
#                         col_select = any_of(selected_cols),
#                         col_types = cols(survey_version = "c"))
#   dt$region_agg <- as.character(dt$region_agg)
#   dt_all <- bind_rows(dt_all, dt)
# }
# 
# write.csv(dt_all, "data.csv", quote = FALSE, row.names = FALSE)


# Read data
data <- read_csv("data.csv") %>%
  mutate_all(na_if, -99) %>%
  mutate_all(na_if, -88) %>%
  mutate_all(na_if, -77)

########################################## ASSIGN GEOSCHEME ##########################################

# Remove observations from country Antarctica
data <- data[!data$country_agg == "Antarctica",]

# Assign continent to every country
data <- data %>%
  mutate(continent_region = case_when(country_agg %in% c("Australia", "New Zealand", "Norfolk Island", "Heard Island and McDonald Islands",
                                                     "Christmas Island", "Cocos (Keeling) Islands") ~ "Austrlia and New Zealand",
                                      country_agg %in% c("Fiji", "New Caledonia", "Papua New Guinea", "Solomon Islands", "Vanuatu") ~ "Melanesia",
                                      country_agg %in% c("Guam", "Kiribati", "Marshall Islands", "Federated States of Micronesia", "Nauru", 
                                                     "Northern Mariana Islands", "Palau") ~ "Micronesia",
                                      country_agg %in% c("American Samoa", "Cook Islands", "French Polynesia", "Niue", "Pitcairn", "Samoa",
                                                     "Tokelau", "Tonga", "Tuvalu", "Wallis and Futuna") ~ "Polynesia",
                                      country_agg %in% c("Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan") ~ "Central Asia",
                                      country_agg %in% c("China", "Hong Kong", "North Korea", "Japan", "Mongolia", "South Korea", "Taiwan", "Macau") ~ "Eastern Asia",
                                      country_agg %in% c("Russia") ~ "Northern Asia",
                                      country_agg %in% c("Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines",
                                                     "Singapore", "Thailand", "Timor-Leste", "Vietnam") ~ "South-eastern Asia",
                                      country_agg %in% c("Afghanistan", "Bangladesh", "Bhutan", "India", "Iran", "Maldives", "Nepal", "Pakistan",
                                                     "Sri Lanka") ~ "Southern Asia",
                                      country_agg %in% c("Armenia", "Azerbaijan", "Bahrain", "Cyprus", "Georgia", "Iraq", "Israel", "Jordan",
                                                     "Kuwait", "Lebanon", "Oman", "Qatar", "Saudi Arabia", "Palestine", "Syria", "Turkey",
                                                     "United Arab Emirates", "Yemen") ~ "Western Asia",
                                      country_agg %in% c("Algeria", "Egypt", "Libya", "Morocco", "Sudan", "Tunisia", "Western Sahara") ~ "Northern Africa",
                                      country_agg %in% c("British Indian Ocean Territory", "Burundi", "Comoros", "Djibouti", "Eritrea",
                                                     "Ethiopia", "French Southern Territories", "Kenya", "Madagascar", "Malawi", "Mauritius",
                                                     "Mayotte", "Mozambique", "Réunion", "Rwanda", "Seychelles", "Somalia", "South Sudan", "Uganda",
                                                     "Tanzania", "Zambia", "Zimbabwe") ~ "Eastern Africa",
                                      country_agg %in% c("Angola", "Cameroon", "Central African Republic", "Chad", "Democratic Republic of the Congo", "Equatorial Guinea",
                                                     "Gabon", "Sao Tome and Principe", "Republic of the Congo") ~ "Middle Africa",
                                      country_agg %in% c("Botswana", "Eswatini", "Lesotho", "Namibia", "South Africa") ~ "Southern Africa",
                                      country_agg %in% c("Benin", "Burkina Faso", "Cape Verde", "Côte d'Ivoire", "Gambia", "Ghana", "Guinea",
                                                     "Guinea-Bissau", "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", "Saint Helena",
                                                     "Senegal", "Sierra Leone", "Togo") ~ "Western Africa",
                                      country_agg %in% c("Antigua", "Aruba", "Bahamas", "Barbados", "Bonaire", "British Virgin Islands",
                                                     "Cayman Islands", "Cuba", "Curaçao", "Dominica", "Dominican Republic", "Grenada", "Guadeloupe", "Haiti",
                                                     "Jamaica", "Martinique", "Montserrat", "Puerto Rico", "Saint Barthélemy", "Saint Kitts and Nevis",
                                                     "Saint Lucia", "Saint Martin", "Saint Vincent and the Grenadines", "Sint Maarten", "Trinidad and Tobago",
                                                     "Turks and Caicos Islands", "United Kingdom ~ British Virgin Islands", "Virgin Islands",
                                                     "Anguilla") ~ "Caribbean",
                                      country_agg %in% c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua",
                                                     "Panama") ~ "Central America",
                                      country_agg %in% c("Argentina", "Bolivia", "Bouvet Island", "Brazil", "Chile", "Colombia", "Ecuador", "Falkland Islands",
                                                     "French Guiana", "Guyana", "Paraguay", "Peru", "South Georgia and the South Sandwich Islands", "Suriname",
                                                     "Uruguay", "Venezuela") ~ "South America",
                                      country_agg %in% c("Bermuda", "Canada", "Greenland", "Saint Pierre and Miquelon", "United States", 
                                                     "United States Minor Outlying Islands") ~ "Northern America",
                                      country_agg %in% c("Belarus", "Bulgaria", "Czech Republic", "Hungary", "Poland", "Moldova", "Romania",
                                                     "Slovakia", "Ukraine") ~ "Eastern Europe", 
                                      country_agg %in% c("Aland Islands", "Denmark", "Estonia", "Faroe Islands", "Finland", "Iceland", "Ireland", "Isle Of Man", "Latvia", "Lithuania", 
                                                     "Norway", "Svalbard and Jan Mayen", "Sweden", "United Kingdom", "Jersey", "Guernsey") ~ "Northern Europe",
                                      country_agg %in% c("Albania", "Andorra", "Bosnia and Herzegovina", "Croatia", "Gibraltar", "Greece", "Italy", "Vatican City",
                                                     "Malta", "Montenegro", "Republic of North Macedonia", "Portugal", "San Marino", "Serbia", "Slovenia", "Spain",
                                                     "Kosovo") ~ "Southern Europe",
                                      country_agg %in% c("Austria", "Belgium", "France", "Germany", "Liechtenstein", "Luxembourg", "Monaco", "Netherlands", "Switzerland") ~ "Western Europe",
                                      country_agg %in% c("Antarctica") ~ "Antarctica"),
         continent = case_when(continent_region %in% c("Austrlia and New Zealand", "Micronesia", "Polynesia", "Melanesia") ~ "Oceania",
                   continent_region %in% c("Central Asia", "Eastern Asia", "Western Asia", "Southern Asia", "Northern Asia", "South-eastern Asia") ~ "Asia",
                   continent_region %in% c("Middle Africa", "Northern Africa", "Eastern Africa", "Southern Africa", "Western Africa") ~ "Africa",
                   continent_region %in% c("Caribbean", "Central America", "South America", "Northern America") ~ "America",
                   continent_region %in% c("Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe") ~ "Europe"))

########################################## PREPARE DATA ##########################################

## Selected Variables and change groups of some factor variables (age, gender, area, had_covid) :
## - A2_2_1: What is the country or region where you are currently staying?
## - D1: During the past 7 days, how often did you feel so nervous that nothing could calm you down?
## - D2: During the past 7 days, how often did you feel so depressed that nothing could cheer you up?
## - D4: How worried are you about having enough to eat in the next week?
## - D5: How worried are you about your household’s finances in the next month?
## - E3: Gender (1 = Male, 2 = Female, 3 = Other)
## - E4: Age
## - E2: Which of these best describes the area where you are currently staying? (1 = City, 2 = Town, 3 = Village/rural area)

data <- data %>%
  dplyr::rename("anxious" = D1, "depressed" = D2, "gender" = E3, "age" = E4, "area" = E2) %>%
  mutate(age_grouped = case_when(age %in% c(1, 2) ~ "1", age %in% c(3, 4, 5) ~ "2", age %in% c(6, 7) ~ "3")) %>%
  mutate(gender = case_when(gender %in% c(1) ~ "1", gender %in% c(2) ~ "2", gender %in% c(3) ~ "3", gender %in% c(4) ~ NA_character_)) %>%
  mutate(gender_grouped = case_when(gender %in% c(1) ~ "1", gender %in% c(2) ~ "2", gender %in% c(3) ~ NA_character_, gender %in% c(4) ~ NA_character_)) %>%
  mutate(area_grouped = case_when(area %in% c(1, 2) ~ "0", area %in% c(3) ~ "1")) %>%
  mutate(depression_group = case_when(depressed %in% c(1, 2) ~ "1", depressed %in% c(3, 4) ~ "2", depressed %in% c(5) ~ "3")) %>%
  mutate(anxious_group = case_when(anxious %in% c(1, 2) ~ "1", anxious %in% c(3, 4) ~ "2", anxious %in% c(5) ~ "3")) %>%
  mutate(anxious_flip = case_when(anxious == 1 ~ 5, anxious == 2 ~ 4, anxious == 4 ~ 2, anxious == 5 ~ 1, anxious == 3 ~ 3)) %>%
  mutate(depression_flip = case_when(depressed == 1 ~ 5, depressed == 2 ~ 4, depressed == 3 ~ 3, depressed == 4 ~ 2, depressed == 5 ~ 1))

data[, c("anxious", "depressed", "gender", "age", "area")] <- lapply(data[, c("anxious", "depressed", "gender", "age", "area")] , factor)



########################################## AGGREGATE DATA BY WEEK & MONTH ##########################################

# Remove last month from data
data$date <- as.Date(ymd_hms(data$RecordedDate))
data <- data %>%
  filter(date < "2022-06-01")

# Add week, month and year as columns
data$day <- floor_date(data$date, "day")
data$week <- floor_date(data$date, "week")
data$month <- floor_date(data$date, "month")
data$year <- floor_date(data$date, "year")

data$depressed <- as.numeric(data$depressed)
data$anxious <- as.numeric(data$anxious)

# Aggregate depressed and anxious by month and week
data_monthly <- data %>%
  drop_na(month, continent) %>%
  group_by(month, continent) %>%
  dplyr::summarize(depressed_monthly = mean(depressed, na.rm = TRUE),
                   anxious_monthly = mean(anxious, na.rm = TRUE),
                   depressed_monthly_flip = mean(depression_flip, na.rm = TRUE),
                   anxious_monthly_flip = mean(anxious_flip, na.rm = TRUE)) %>%
  as.data.frame()

data_weekly <- data %>%
  drop_na(month, continent) %>%
  group_by(week, continent) %>%
  dplyr::summarize(depressed_weekly = mean(depressed, na.rm = TRUE),
                   anxious_weekly = mean(anxious, na.rm = TRUE),
                   depressed_weekly_flip = mean(depression_flip, na.rm = TRUE),
                   anxious_weekly_flip = mean(anxious_flip, na.rm = TRUE)) %>%
  as.data.frame()

data_daily <- data %>%
  drop_na(day, country_agg) %>%
  group_by(day, country_agg) %>%
  dplyr::summarize(depressed_daily = mean(depressed, na.rm = TRUE),
                   anxious_daily = mean(anxious, na.rm = TRUE),
                   depressed_daily_flip = mean(depression_flip, na.rm = TRUE),
                   anxious_daily_flip = mean(anxious_flip, na.rm = TRUE),
                   iso = first(ISO_3),
                   continent = first(continent)) %>%
  mutate(week = floor_date(day, "week"),
         month = floor_date(day, "month"))





#write.csv(data, "data_prepared.csv", quote = FALSE, row.names = FALSE)




