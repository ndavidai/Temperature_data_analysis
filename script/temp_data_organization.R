
library(dplyr)
library(stringr)
library(xfun)
library(janitor)


#identify all data files

# Combine all temp data sets ----------------------------------------------

data_files <- dir(path = "input/", recursive = TRUE, pattern = ".csv", 
                  full.names = TRUE)

#remove all but site name from each row
list_of_files <- lapply(data_files, FUN = function(x){
  read.csv(x) %>% 
    mutate(site = str_extract(basename(sans_ext(x)),"^[^_]+(?=_)"))
})

#combine all lists into one data set
#add data columns
temp_all_data <- do.call(rbind,list_of_files) %>% 
  mutate(snow_position = case_when(str_detect(site,"D")==TRUE~"below", 
                                   str_detect(site,"U")==TRUE~"above"),
         park_name = case_when(str_detect(site,"S")==TRUE~"Saint Sulpice",
                               str_detect(site,"K")==TRUE~"Kenauk",
                               str_detect(site,"O")==TRUE~"Oka",
                               str_detect(site,"P")==TRUE~"Pointe aux Prairie",
                               str_detect(site,"Z")==TRUE~"Boise Jean Milot",
                               str_detect(site,"M")==TRUE~"Mont Orford",
                               str_detect(site,"H")==TRUE~"Mont Saint Hilaire",
                               str_detect(site,"A")==TRUE~"Angrignon",
                               str_detect(site,"BD")==TRUE~"Mont Saint Bruno",
                               str_detect(site,"BU")==TRUE~"Mont Saint Bruno",
                               str_detect(site,"BGD")==TRUE~"Botanical Gardens",
                               str_detect(site,"BGU")==TRUE~"Botanical Gardens"))
                               
                                
check_sites <- temp_all_data %>% group_by(snow_position, park_name) %>% tally()

# quick visualizations of data set
summary(temp_all_data)
str(temp_all_data)
unique(temp_all_data)

# Clean and organize data --------------------------------------------------------------

#remove all unique characters and replaces spaces with _.
temp_data_clean <- temp_all_data %>%
  clean_names()

summary(temp_data_clean)
str(temp_data_clean)

# change "snow position to 'factor'
temp_data_clean$snow_position <- as.factor(temp_data_clean$snow_position)

str(temp_data_clean)

# create the urban/non-urban column
temp_data_clean <- temp_data_clean %>%
  mutate(urban = case_when(
    grepl("Oka", park_name) ~ "No",
    grepl("Kenauk", park_name) ~ "No",
    grepl("Mont Orford", park_name) ~ "No",
    grepl("Mont Saint Bruno", park_name) ~ "No",
    grepl("Mont Saint Hilaire", park_name) ~ "No",
    grepl("Angrignon", park_name) ~ "Yes",
    grepl("Boise Jean Milot", park_name) ~ "Yes",
    grepl("Botanical Gardens", park_name) ~ "Yes",
    grepl("Pointe aux Prairie", park_name) ~ "Yes",
    grepl("Saint Sulpice", park_name) ~ "Yes",
    TRUE ~ NA_character_
  ))

check_sites <- temp_data_clean %>% group_by(snow_position, park_name, urban) %>% tally()

#separate and format date_time column
library(lubridate)

temp_data_clean <- temp_data_clean %>%
  mutate(date_time = ymd_hms(date_time)) %>%
  mutate(
    date = as.Date(date_time),
    time = format(date_time, "%H:%M:%S")
  )

#extract the hour into a separate column (for analysis purposes)
temp_data_clean <- temp_data_clean %>%
  mutate(hour = hour(date_time))

str(temp_data_clean)

write.csv(temp_data_clean, "input/Temp_data_clean.csv")
