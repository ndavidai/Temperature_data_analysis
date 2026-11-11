
library(dplyr)
library(stringr)
library(xfun)


#identify all data files

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
                               str_detect(site,"JMD")==TRUE~"Boise Jean Milot",
                               str_detect(site,"JMU")==TRUE~"Boise Jean Milot",
                               str_detect(site,"MD")==TRUE~"Mont Orford",
                               str_detect(site,"MU")==TRUE~"Mont Orford",
                               str_detect(site,"H")==TRUE~"Mont Saint Hilaire",
                               str_detect(site,"A")==TRUE~"Angrignon",
                               str_detect(site,"BD")==TRUE~"Mont Saint Bruno",
                               str_detect(site,"BU")==TRUE~"Mont Saint Bruno",
                               str_detect(site,"BGD")==TRUE~"Botanical Gardens",
                               str_detect(site,"BGU")==TRUE~"Botanical Gardens"))
                               
                                
check_sites <- temp_all_data %>% group_by(snow_position, park_name) %>% tally()
