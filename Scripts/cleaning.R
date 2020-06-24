######## New Data from LAUSD (2020/05/28) ##########

library(tidyverse)
library(ggmap)
library(readxl)
library(glue)
library(janitor)
library(here)

# Import the raw data=
sources <- read_excel(list.files(path = here("/data/raw"), pattern = "*.xlsx$"))

# The data is not clean enough for us to pass to the geocoder so we need to wrangle it for a bit

# First create unique identifier to keep track of observations across dataframes. I'll use the row number since it's the easieast and the data is small. To scale, LAUSD staff should think of a unique identifier going forward. Using dataMaid::check, we see that "Local District" has "Various" and "various", we need to clean that to only "Various". Recommend being careful and consistent with the syntax of the Local District categories

sources <- sources %>% 
  mutate(id = row_number()) %>% 
  mutate_if(is.character, str_squish) %>% 
  mutate(`Local District` = if_else(`Local District` == "various", "Various", `Local District`))

# Since the most important part of the geocoding process is passing a complete address to the Google API we need to filter the provided data accordingly. We are gonna end up with two (probably three) datasets. one for data with geolocations, one for online resources, (and whatever is left).



online <- sources %>% 
  filter(is.na(`Zip code`))

index <- which(sources$id %in% online$id)

# Subset to those left after extracting the online observations

physical <- sources %>% 
  slice(-index)


# Google Maps has trouble with parsing # signs when geolocating, So I'll remove them when creating the "Full Address" variable. Unite "pastes" two character columns into one with a given separator in this case a comma and a space ", ". Type ?unite in the console for more info.


physical_full <- physical %>% 
  mutate(`Full Address` = str_remove(Address, "[#].*$")) %>% 
  unite("Full Address", c(`Full Address`, City), sep = ", ", remove = F, na.rm = T) %>% 
  unite("Full Address", c(`Full Address`, `Zip code`), sep = ", ", remove = F, na.rm = T)

# Now we can pass this dataframe to Google's API to geocode it.

# In order to be able to use Google Maps API you have to register at developers.google.com and go to the Google Maps Platform. Go to Console and generate a key and use the function register_google in an R session with the key generated in Google's website and the type of account. Type ?register_google in R's console for more info. #

#### IMPORTANT NOTE: YOU SHOULD REGISTER YOUR KEY EVERYTIME YOU START A NEW R SESSION AND RUN THIS SCRIPT

physical_gc <- physical_full %>% 
  mutate_geocode(`Full Address`)

# Looking at the "online" dataframe,id 479 has an improperly formatted "Address" field that's why it ends up in the "online" data. Cleaning it, geocoding it, and merging it back into the physical data.

odd_entry <- online %>% 
  filter(id == 479) %>% 
  mutate(`Full Address` = Address) %>% 
  separate(`Full Address`, into = c("Address", "drop"), sep = "\\.", remove = F) %>% 
  separate(drop, into = c("City", "Zip code"), sep = ", ", remove = T, convert = T) %>% 
  mutate_geocode(`Full Address`)

# Reformatting variables into character and Zip code into numeric fromat. This is important to be able to merge the 'odd_entry' back into the 'physical' data.

odd_entry <- type_convert(odd_entry, col_types = cols(
  `Partner Type` = col_character(),
  `Local District` = col_character(),
  `Organization/ Agency` = col_character(),
  `Contact Link` = col_character(),
  `Phone number` = col_character(),
  Description = col_character(),
  `Full Address` = col_character(),
  Address = col_character(),
  City = col_character(),
  `Zip code` = col_number()
))

physical_gc <- bind_rows(physical_gc, odd_entry)

# Reclean physical_gc since I went back to clean resources and dont want to rerun the geocoding. THIS DOESN'T NEED TO BE RUN AGAIN. IT WAS A ONE TIME THING BECAUSE I CAUGHT ID 479 AFTER GEOCODING. IF THE DATA IS FORMATTED PROPERLY, THEN YOU GEOCODE AFTER SPLITTING DATA.

# physical_gc <- physical_gc %>% 
#   mutate_if(is.character, str_squish) %>% 
#   mutate(`Local District` = if_else(`Local District` == "various", "Various", `Local District`))



# Saving this to its own file so no geocoding has to be done multiple times and thus saving API calls.

write_csv(physical_gc, here("/raw/physical_sources.csv"))

### Let's look at the online only. Removing id number 479. And saving to disk

online <- online %>% 
  filter(!(id == 479))

write_csv(online, here("/raw/online_resources.csv"))

# We want to merge both datasets before importing into the Shiny app.
# First we need to add an "online" identifier column to both datasets and then a lat and lon empty columns to the online dataset so that the columns match when we merge them.

online_df <- online %>% 
  mutate(online = "yes", lat = NA, lon = NA, `Full Address` = NA)

physical_df <- physical_gc %>% 
  mutate(online = "no")

all_resources <- bind_rows(physical_df, online_df)

# Also need to make variable names easier to handle (i.e. no spaces, lowercase, unique)

all_resources <- all_resources %>% 
  rename(partner_type = `Partner Type`, district = `Local District`, name = `Organization/ Agency`, contact_link = `Contact Link`, address = Address, city = City, zip = `Zip code`, phone = `Phone number`, description = Description) 

# This is the file the Shiny app is going to use. As long as you have a csv with the name "resources_full.csv" formatted the same way as this file in the data folder of the app the map should run with no problems.

write_csv(all_resources, here("/data/resources_full.csv"))

# Comment: LAUSD should consider if they want to differentiate from the observations with comments guiding somewhere else (i.e. "Check website for locations") from those observations with no message.