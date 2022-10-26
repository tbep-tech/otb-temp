library(tidyverse)
library(googlesheets4)
library(googledrive)
library(janitor)
library(lubridate)
library(sf)
library(here)

drive_deauth()
gs4_deauth()

# gdrive info ---------------------------------------------------------------------------------

gdrive_pth <- 'https://drive.google.com/drive/folders/1MauA87CetbHCl1IcSolrv2Q8HRXoM9NL?usp=sharing'

fls <- drive_ls(gdrive_pth) %>% 
  filter(!grepl('\\.xlsx$', name))


# site metadata -------------------------------------------------------------------------------

metadat <- fls %>% 
  filter(grepl('OTB_TEMP_LOGGER_DATA', name)) %>% 
  pull(id) %>% 
  read_sheet(na = c('none', 'NA'), col_types = 'ttnccnnnccccccc') %>% 
  clean_names %>% 
  filter(site_id %in% c(1:6)) %>% 
  select(
    logger = logger_id,
    stratum, 
    lat, 
    long, 
    depthm = depth_m, 
    seagrass_percent, 
    seagrass_species, 
    macro_percent, 
    macro_species
  ) %>% 
  filter(!is.na(logger)) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

save(metadat, file = here('data/metadat.RData'))

# temp data -----------------------------------------------------------------------------------

datfls <- fls %>% 
  .[!grepl('OTB_TEMP_LOGGER_DATA|DATASHEETS', .$name),] 

tempdat <- NULL
for(i in 1:nrow(datfls)){
  
  id <- datfls[i, ] %>% 
    pull(id)
  
  logger <- datfls[i, ] %>% 
    pull(name) %>% 
    gsub(' .*', '', .) %>% 
    gsub('2145', '', .)
  
  out <- read_sheet(id) %>% 
    mutate(
      logger = logger, 
      elapsed = `Date-Time (EDT)` - min(`Date-Time (EDT)`)
    ) %>% 
    filter(elapsed > 3600)
  
  tempdat <- bind_rows(tempdat, out)
  
}

tempdat <- tempdat %>% 
  clean_names %>% 
  select(
    logger, 
    datetime = date_time_edt, 
    tempc = ch_1_temperature_c
  ) %>% 
  mutate(
    datetime = force_tz(datetime, tzone = 'America/New_York')
  )

save(tempdat, file = here('data/tempdat.RData'))