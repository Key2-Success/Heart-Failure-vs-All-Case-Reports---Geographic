# steps:
# 1. read in the data and clean it
# 2. extract geographic locations
# 3. compare proportions of locations using statistical test
# 4. graph significant locations
# 5. r shiny app to visualize any subset of case report to all (for future: not in this script)

library(readr)
library(data.table)
library(reshape)
library(dplyr)
library(tidyr)
library(xlsx)
library(stringr)
library(stringi)
library(readr)


##### 1: read in the data and clean it #####

# load heart failure case reports
load(file = "Kitu/College/Senior Year/Extracurriculars/Data Science Research Internship/David's Data/HFpEF vs HFrEF/all heart failure case reports clean.Rdata")

location <- read_csv(file = "Kitu/College/Senior Year/Extracurriculars/Data Science Research Internship/Python Extract/location.csv")

location <- location[ , c(3, 2)]

##### 2: extract geographic locations #####

# load supplementary data found online
US_cities <- read_csv("most populous US cities.csv", col_names = FALSE)
countries <- read.csv("countries.csv", stringsAsFactors = FALSE)
usstates <- read.csv("US_States.csv", stringsAsFactors = FALSE, header = FALSE)
major_global_cities <- read_csv("major global cities.csv", col_names = TRUE)
country_capitals <- read_csv("country-capitals.csv", col_names = TRUE)
state_abs <- read.csv("state_abbreviations.csv")

# ----- cleaning data ----- #

# clean most populous US cities dataframe
US_cities$X1 <- str_replace_all(US_cities$X1, "<.*>", "") # remove strange format in beginning
US_cities$X1 <- substring(US_cities$X1, 2, nchar(US_cities$X1)) # continue to remove weird image in beginning
US_cities$X7 <- sub(pattern = " *\\(.*?\\) *", replacement = "", x = US_cities$X7)
US_cities$X7 <- substring(US_cities$X7, 1, nchar(US_cities$X7) - 2)
US_cities$X1[7] <- substr(US_cities$X1[7], 2, nchar(US_cities$X1[7])) # weird North Dakota exception
US_cities <- t(US_cities) # transpose
colnames(US_cities) <- US_cities[1, ] # rename headers
US_cities <- US_cities[-1, ]
US_cities <- melt(data = US_cities) # reshape from wide to long format
US_cities <- US_cities[ , -1]
names(US_cities) <- c("State", "City")
US_cities <- US_cities[complete.cases(US_cities), c(2, 1)]
US_cities$City <- as.character(US_cities$City)
US_cities$State <- as.character(US_cities$State)

# clean major global cities dataframe
major_global_cities <- major_global_cities[-1, c(2:3)]
major_global_cities$City <- stri_trans_tolower(major_global_cities$City)
major_global_cities$Country <- tolower(major_global_cities$Country)

# replace usa and uk with full form 
major_global_cities$Country <- stri_replace_all_regex(str = major_global_cities$Country, pattern = "\\busa\\b", replacement = "united states")
major_global_cities$Country <- stri_replace_all_regex(str = major_global_cities$Country, pattern = "\\buk\\b", replacement = "united kingdom")

# remove those with ( in the dataframe
a <- which(grepl("\\(", major_global_cities$City))
major_global_cities <- major_global_cities[-a, ]

# add in those with ( from dataframe
parenthesis <- data.frame("City" = c("pretoria", "bombay", "jiangxi", "halab", "tshwane", "mumbai", "fuzhou", "aleppo"), 
                          "Country" = c("south africa", "india", "china", "syria", "south africa", "india", "china", "syria"), 
                          stringsAsFactors = FALSE)
major_global_cities <- rbind(major_global_cities, parenthesis)


# clean country capitals dataframe
country_capitals <- country_capitals[, 1:2]

# start to string match location by making all strings lowercase to be consistent
data$AD <- tolower(data$AD)
countries$Name <- tolower(countries$Name)
countries$Code <- tolower(countries$Code)
usstates$V1 <- tolower(usstates$V1)
usstates$V2 <- tolower(usstates$V2)
US_cities$City <- tolower(US_cities$City)
US_cities$State <- tolower(US_cities$State)
country_capitals$CountryName <- tolower(country_capitals$CountryName)
country_capitals$CapitalName <- tolower(country_capitals$CapitalName)
state_abs$State <- tolower(state_abs$State)
state_abs$Abbreviation <- tolower(state_abs$Abbreviation)


# ----- string matching ----- #

data <- test
data$country <- ""
data$us_state <- ""

# string match the countries
data$country <- str_extract(data$AD, paste0("\\b", countries$Name, "\\b", collapse = "|"))

# string match the US states
data$us_state <- str_extract(data$AD, paste0("\\b", usstates$V1, "\\b", collapse = "|"))

# string match the top US cities
data$us_cities <- ifelse(is.na(data$us_state) , str_extract(data$AD, paste0("\\b", US_cities$City, "\\b", collapse = "|")), "")
data <- left_join(data, US_cities, by = c("us_cities" = "City"))
data$us_state <- ifelse(is.na(data$us_state), data$State, data$us_state)
data$State <- NULL

# string match country capitals
data$capitals <- ifelse(is.na(data$country) , str_extract(data$AD, paste0("\\b", country_capitals$CapitalName, "\\b", collapse = "|")), "")
data <- left_join(data, country_capitals, by = c("capitals" = "CapitalName"))
data$country <- ifelse(is.na(data$country), data$CountryName, data$country)

# string match the major global cities
data$world_cities <- ifelse(is.na(data$country) , str_extract(data$AD, paste0("\\b", major_global_cities$City, "\\b", collapse = "|")), "")
data <- left_join(data, major_global_cities, by = c("world_cities" = "City"))
data$country <- ifelse(is.na(data$country), data$Country, data$country)

# string match usa state abbreviations
cond <- is.na(data$us_state) & (is.na(data$country) | data$country == "United States")
data$abs <- data$us_state
data$abs[cond] <- str_extract(data$AD[cond], paste0("\\b", state_abs$Abbreviation, "\\b", collapse = "|"))
data <- left_join(data, state_abs, by = c("abs" = "Abbreviation"))
data$us_state <- ifelse(is.na(data$us_state), data$State, data$us_state)

# hardcode uk and usa abbreviations as well as mapping peking to China and others
data$country <- ifelse(str_detect(string = data$AD, pattern = "\\buk\\b"), "United Kingdom", data$country)
data$country <- ifelse(grepl("\\busa\\b", data$AD), "United States", data$country)
data$country <- ifelse(grepl("peking", data$AD), "China", data$country)
data$us_state <- ifelse(grepl("stanford", data$AD), "california", data$us_state)
data$us_state <- ifelse(grepl("palo alto", data$AD), "california", data$us_state)
data$country <- ifelse(grepl("england", data$AD), "United Kingdom", data$country)

# vector of all US states
states <- usstates$V1[1:50]

# if state is in the USA, change country name to USA
data$country <- ifelse(data$us_state %in% states, "United States", data$country)

# make all countries and states title case
data$country <- str_to_title(data$country)
data$us_state <- str_to_title(data$us_state)

# remove all unnecessary variables
data <- data[ , -which(names(data) %in% c("us_cities", "State", "capitals", "CountryName", "world_cities", "Country", "abs"))]


##### 3: compare proportions of locations using statistical test #####

##### 4: graph significant locations #####

