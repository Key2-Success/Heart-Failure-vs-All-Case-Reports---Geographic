# to-do:
# 1. change color scheme using Harry's suggestions to make color above and below 1 different
# 2. add in dark grey scheme for countries that do produce heart failure case reports (and light grey for those who do not)
# 3. find incidence and prevalence statistics and find correlation to those numbers to case report publications
# 4. look into cancer case reports
# 5. look into malaria case reports
# 6. create R shiny app for ACCR
# 7. create just general map of ACCR
# 8. statistically compare ACCR locations to PMID locations


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
library(leaflet)
library(ggmap)
library(rgdal)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(plotly)
library(grDevices)


##### 1: read in the data and clean it #####

# load heart failure case reports
load("all heart failure case reports clean.Rdata")

location <- read_csv("location.csv")

location <- location[ , c(3, 2)]
location$col <- str_replace_all(location$col,"[^[:graph:]]", " ")

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
location$col <- tolower(location$col)
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
save(location, file = "location.Rdata")
load("location.Rdata")

location$country <- ""
location$us_state <- ""

# string match the countries
location$country <- str_extract(location$col, paste0("\\b", countries$Name, "\\b", collapse = "|"))

# string match the US states
location$us_state <- str_extract(location$col, paste0("\\b", usstates$V1, "\\b", collapse = "|"))

# string match the top US cities
location$us_cities <- ifelse(is.na(location$us_state) , str_extract(location$col, paste0("\\b", US_cities$City, "\\b", collapse = "|")), "")
location <- left_join(location, US_cities, by = c("us_cities" = "City"))
location$us_state <- ifelse(is.na(location$us_state), location$State, location$us_state)
location$State <- NULL

# string match country capitals
location$capitals <- ifelse(is.na(location$country) , str_extract(location$col, paste0("\\b", country_capitals$CapitalName, "\\b", collapse = "|")), "")
location <- left_join(location, country_capitals, by = c("capitals" = "CapitalName"))
location$country <- ifelse(is.na(location$country), location$CountryName, location$country)

# string match the major global cities
location$world_cities <- ifelse(is.na(location$country) , str_extract(location$col, paste0("\\b", major_global_cities$City, "\\b", collapse = "|")), "")
location <- left_join(location, major_global_cities, by = c("world_cities" = "City"))
location$country <- ifelse(is.na(location$country), location$Country, location$country)

# string match usa state abbreviations
cond <- is.na(location$us_state) & (is.na(location$country) | location$country == "United States")
location$abs <- location$us_state
location$abs[cond] <- str_extract(location$col[cond], paste0("\\b", state_abs$Abbreviation, "\\b", collapse = "|"))
location <- left_join(location, state_abs, by = c("abs" = "Abbreviation"))
location$us_state <- ifelse(is.na(location$us_state), location$State, location$us_state)

# hardcode uk and usa abbreviations as well as mapping peking to China and others
location$country <- ifelse(str_detect(string = location$col, pattern = "\\buk\\b"), "United Kingdom", location$country)
location$country <- ifelse(grepl("\\busa\\b", location$col), "United States", location$country)
location$country <- ifelse(grepl("peking", location$col), "China", location$country)
location$us_state <- ifelse(grepl("stanford", location$col), "california", location$us_state)
location$us_state <- ifelse(grepl("palo alto", location$col), "california", location$us_state)
location$country <- ifelse(grepl("england", location$col), "United Kingdom", location$country)

# vector of all US states
states <- usstates$V1[1:50]

# if state is in the USA, change country name to USA
location$country <- ifelse(location$us_state %in% states, "United States", location$country)

# make all countries and states title case
location$country <- str_to_title(location$country)
location$us_state <- str_to_title(location$us_state)

# remove all unnecessary variables
location <- location[ , -which(names(location) %in% c("us_cities", "State", "capitals", "CountryName", "world_cities", "Country", "abs"))]


##### 3: compare proportions of locations using statistical test #####

# create dataframe for country proportions
loc_country <- data.frame(table(location$country))
names(loc_country) <- c("loc_country", "loc_country_freq")

data_country <- data.frame(table(data$country))
names(data_country) <- c("data_country", "data_country_freq")

# join dataframes
all <- left_join(loc_country, data_country, by = c("loc_country" = "data_country"))

# create proportion variables
all$loc_prop <- all$loc_country_freq/sum(all$loc_country_freq) # sum is 1442889
all$data_prop <- all$data_country_freq/sum(all$data_country_freq, na.rm = T) # sum is 18047

# value of proportion is 0 if NA
all$data_prop <- ifelse(is.na(all$data_prop), 0, all$data_prop)
all$data_country_freq <- ifelse(is.na(all$data_country_freq), 0, all$data_country_freq)

# find and remove those countries that don't satisfy chi square conditions
count <- numeric(221) # dim of loc_country

for (i in 1:nrow(all))
{
  a <- c(all$loc_country_freq[i], all$data_country_freq[i])
  b <- c(sum(all$loc_country_freq), sum(all$data_country_freq, na.rm = T)) # dim of each variables
  m <- matrix(c(a, b-a), ncol = 2)
  if (sum(chisq.test(m)$expected > 5) != 4)
  {
    count <- append(count, i)
  }
}

all <- all[-count, ]

# find p-values
for (i in 1:nrow(all))
{
  # calculate raw occurrences
  prop_1 <- all[i, 2]
  prop_2 <- all[i, 3]
  
  # calculate p-value
  p <- prop.test(x = c(prop_1, prop_2), n = c(1442889, 18047), correct = TRUE)
  p <- p$p.value
  all$pvalue[i] <- p
}

# order by significant and increasing p-values
all <- all[all$pvalue <= 0.05, ]
all <- all[order(all$pvalue), ]

# find difference between proportions
all$diffs <- all$loc_prop - all$data_prop

# create factor differences
for (i in 1:nrow(all))
{
  if (all$diffs[i] < 0)
  {
    all$factor[i] <- -all$data_prop[i]/all$loc_prop[i]
  }
  if (all$diffs[i] > 0)
  {
    all$factor[i] <- all$loc_prop[i]/all$data_prop[i]
  }
}

# substitute infinite values
for (i in 1:nrow(all))
{
  if (all$factor[i] == Inf)
  {
    all$factor[i] <- all$loc_prop[i]
  }
  if (all$factor[i] == -Inf)
  {
    all$factor[i] <- all$data_prop[i]
  }
}

save(all, file = "all.Rdata")

# remember loc contains all case reports and data contains heart failure

# create dataframe for state proportions
loc_state <- data.frame(table(location$us_state))
names(loc_state) <- c("loc_state", "loc_state_freq")

data_state <- data.frame(table(data$us_state))
names(data_state) <- c("data_state", "data_state_freq")

# join dataframes
all2 <- left_join(loc_state, data_state, by = c("loc_state" = "data_state"))

# create proportion variables
all2$loc_prop_state <- all2$loc_state_freq/sum(all2$loc_state_freq) # sum is 366053
all2$data_prop_state <- all2$data_state_freq/sum(all2$data_state_freq, na.rm = T) # sum is 4686

# value of proportion is 0 if NA
all2$data_prop_state <- ifelse(is.na(all2$data_prop_state), 0, all2$data_prop_state)
all2$data_state_freq <- ifelse(is.na(all2$data_state_freq), 0, all2$data_state_freq)

# find and remove those countries that don't satisfy chi square conditions
count <- numeric(54) # dim of all2

for (i in 1:nrow(all2))
{
  a <- c(all2$loc_state_freq[i], all2$data_state_freq[i])
  b <- c(sum(all2$loc_state_freq), sum(all2$data_state_freq, na.rm = T)) # dim of each variables
  m <- matrix(c(a, b-a), ncol = 2)
  if (sum(chisq.test(m)$expected > 5) != 4)
  {
    count <- append(count, i)
  }
}

all2 <- all2[-count, ]

# find p-values
for (i in 1:nrow(all2))
{
  # calculate raw occurrences
  prop_1 <- all2[i, 2]
  prop_2 <- all2[i, 3]
  
  # calculate p-value
  p <- prop.test(x = c(prop_1, prop_2), n = c(1442889, 18047), correct = TRUE)
  p <- p$p.value
  all2$pvalue[i] <- p
}

# order by significant and increasing p-values
all2 <- all2[all2$pvalue <= 0.05, ]
all2 <- all2[order(all2$pvalue), ]

# find difference between proportions
all2$diffs <- all2$loc_prop_state - all2$data_prop_state

# create factor differences
for (i in 1:nrow(all2))
{
  if (all2$diffs[i] < 0)
  {
    all2$factor[i] <- -all2$data_prop_state[i]/all2$loc_prop_state[i]
  }
  if (all2$diffs[i] > 0)
  {
    all2$factor[i] <- all2$loc_prop_state[i]/all2$data_prop_state[i]
  }
}

# substitute infinite values
for (i in 1:nrow(all2))
{
  if (all2$factor[i] == Inf)
  {
    all2$factor[i] <- all2$loc_prop_state[i]
  }
  if (all2$factor[i] == -Inf)
  {
    all2$factor[i] <- all2$data_prop_state[i]
  }
}

save(all2, file = "all2.Rdata")

##### 4: graph significant locations #####
# world and state map data
world <- map_data(map = "world")
state <- map_data(map = "state")

# add alaska and hawaii
alaska <- world[which(world$subregion == "Alaska"), ]
alaska[ , 6] <- NA
alaska$region <- "alaska"
state <- rbind(alaska, state)
alaska$group <- 100

hawaii <- world[which(world$subregion == "Hawaii"), ]
hawaii[ , 6] <- NA
hawaii$region <- "hawaii"
hawaii$group <- 99

state <- rbind(hawaii, state)
restWorld <- world[which(world$region != "USA"),]

# save state and world dataframes
save(state, file = "state.Rdata")
save(world, file = "world.Rdata")

# merge state with US data
state_factor <- all2[ , c("loc_state", "factor")]
names(state_factor) <- c("region", "factor") # rename just like in state df
state_factor$region <- tolower(state_factor$region)
state <- left_join(state, state_factor)

# merge world with countries data
world_factor <- all[ , c("loc_country", "factor")]
names(world_factor) <- c("region", "factor") # rename just like in world df
world <- left_join(world, world_factor)

# round the factors
state$factor <- round(state$factor, 2)
world$factor <- round(world$factor, 2)

# clean names
state$region <- str_to_title(state$region)

# change the factor scaling so that there is only one point of reference: compared to expected 1 heart failure case 
# report, this country published x heart failure case reports (so range will vary from 0 to beyond)

# make 1/factor prop for those "less than 1" and reverse scale so they are all positive
state$factor <- ifelse(state$factor > 0, 1/state$factor, -state$factor)
world$factor <- ifelse(world$factor > 0, 1/world$factor, -world$factor)

# save state and world dataframes
save(state, file = "state.Rdata")
save(world, file = "world.Rdata")

# load in dataframes
load("state.Rdata")
load("world.Rdata")

# make map of world
p1 <- ggplot(data = world, aes(x = long, y = lat, fill = factor, Region = region, frequency = factor, group = group)) + geom_polygon(color = "white", show.legend = FALSE)

# combine map of world with US states map
p2 <- p1 + geom_polygon(data = state, color = "white" , aes(fill = factor)) + guides(fill = guide_legend()) + 
  ggtitle(label = "Significant Regions that Publish Heart Failure Case Reports vs All Case Reports") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "") +
  scale_color_brewer(palette = "PuOr") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "white"), 
        axis.line = element_line(colour = "white"),
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank())

# plot it!
ggplotly(p2, tooltip = c("Region", "frequency"))

