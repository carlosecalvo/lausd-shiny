# Importing and geocoding data

library(tidyverse)
library(ggmap)
library(readxl)
library(glue)
library(janitor)
library(here)

##### Data cleaning and wrangling for Pepperdine #####
wdir <- glue(here(),"/")

file <- "LAUSD_COVID19_Family_Resource_Centers_TNL_copy.xlsx"

family <- read_excel(glue("{wdir}data/raw/{file}"), sheet = 'FamilySource')

geocoded <- family %>% 
  mutate_geocode(`Full Address`)

geocoded <- geocoded %>% 
  mutate(x = lon, y = lat) %>% 
  select(-c(lon, lat))

write_tsv(geocoded, path = glue("{wdir}data/familySource.tsv"))

youth <- read_excel(glue("{wdir}/{file}"), sheet = 3)

# Google's API doesn't handle pound signs (#) in the address fields so we need to remove them. Since it's only one in this dataset I'll do it manually.

youth$`Full Address`[10] <- "222 W. 6th St., San Pedro, 90731"

youth_gc <- youth %>% 
  mutate_geocode(`Full Address`)

youth_gc <- youth_gc %>% 
  mutate(x = lon, y = lat) %>% 
  select(-c(lon, lat)) 

youth_gc <- mutate(youth_gc, Hours = "Online", Description = "YouthSource is a city wide program open to young people ages 16-24.  All services are free.  Opportunites include:  work readiness, career exploration, job skills training, tutoring and computer training, college preparation, mentoring and counseling.", Phone_Num = str_squish(Phone_Num))

write_tsv(youth_gc, path = glue("{wdir}data/youthSource.tsv"))

# Rewrite as a function to parse the different sheets on an Excel file

mapping <- function(df, filename) {
  data_gc <- df %>% 
    mutate_geocode(`Full Address`)
  
  data_gc <- data_gc %>% 
    mutate(x = lon, y = lat) %>% 
    select(-c(lon, lat))
  
  return(data_gc)
  
  write_tsv(data_gc, path = glue("{wdir}data/{filename}.tsv"))
  
}

# Mental Health Open Orgs

mHealth_o <- read_excel(glue("{wdir}data/{file}"), sheet = 4)

mapping(mHealth_o, "MentalHealth_open")

# Mental Health Telehealth Orgs

mHealth_tele <- read_excel(glue("{wdir}data/{file}"), sheet = 5)

mHealth_tele <- mHealth_tele %>%  
  mutate(`Full Address` = str_replace(mHealth_tele$`Full Address`, "[#]", ". "))

mapping(mHealth_tele, "MentalHealth_telehealth")

#mHealth_info <- read_excel(glue("{wdir}/{file}"), sheet = 6)

foodRes <- read_excel(glue("{wdir}data/{file}"), sheet = 7)

foodRes <- foodRes %>% 
  mutate(`Full Address` = str_replace(foodRes$`Full Address`, "[#]", ". "))

mapping(foodRes, "FoodResources")


# Aggregate all data into the same file 

# Create a list of the file paths
fileList <- list.files(path = glue("{wdir}data/raw/"), full.names = T, pattern = "*.tsv$")

name_source <- function(x, tmp){
  read_tsv(x) %>% 
    mutate(source = str_extract(tmp, "(?<=data/)(.*)(?=\\.tsv)")) %>% 
    clean_names()
}

name_tsv <- function(x){
  tmp <- x
  assign(glue(str_extract(x, "(?<=data/raw/)(.*)(?=\\.tsv)"), "_df"), name_source(x, tmp))
}

for (i in 1:length(fileList)) {
  assign(glue(str_extract(fileList[i], "(?<=data/raw/)(.*)(?=\\.tsv)"), "_df"), read_tsv(fileList[i]))
}

df <- map_df(fileList, name_tsv) 

df_clean <- df %>% 
  mutate(zip = coalesce(zip, zip_code), phone = coalesce(phone_num, phone_number)) %>% 
  select(-c(zip_code, phone_num, phone_number))

write_tsv(df_clean, path = glue("{wdir}data/aggregated_data.tsv"))

write_tsv(df_clean, path = glue("{wdir}", "test-map/data/aggregated_data.tsv"))