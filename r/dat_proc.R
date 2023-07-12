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

gdrive_pth <- 'https://drive.google.com/drive/folders/1MauA87CetbHCl1IcSolrv2Q8HRXoM9NL'

fls <- drive_ls(gdrive_pth) %>% 
  filter(!grepl('\\.xlsx$', name))

# site metadata -------------------------------------------------------------------------------

metadat <- fls %>% 
  filter(grepl('OTB_TEMP_LOGGER_DATA', name)) %>% 
  pull(id) %>% 
  read_sheet(na = c('', 'NA'), col_types = 'DcDcccnnnnnnccccccc') %>% 
  clean_names %>% 
  select(
    deploy_date,
    logger = logger_id,
    site = site_id,
    stratum, 
    lat, 
    long, 
    depthm = depth_m, 
    seagrass_percent, 
    seagrass_species, 
    macro_percent, 
    macro_species
  ) %>% 
  mutate(
    yr = year(deploy_date), 
    logger = sprintf('%04d', logger)
  ) %>% 
  filter(
    (yr == 2022 & site %in% c(1, 2, 4, 5, 6)) |
    yr == 2023
    ) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

save(metadat, file = here('data/metadat.RData'))

# temp data -----------------------------------------------------------------------------------

load(file = here('data/metadat.RData'))

# logger, site lookup
lkup <- metadat %>% 
  st_set_geometry(NULL) %>% 
  select(yr, site, logger) %>% 
  unique()

datfls <- fls %>% 
  .[!grepl('OTB_TEMP_LOGGER_DATA|DATASHEETS|^Avg', .$name),] 

tempdat <- NULL
for(i in 1:nrow(datfls)){
  
  cat(i,'\n')
  
  id <- datfls[i, ] %>% 
    pull(id)
  
  logger <- datfls[i, ] %>% 
    pull(name)
  
  out <- read_sheet(id) %>% 
    mutate(
      logger = logger, 
      elapsed = `Date-Time (EDT)` - min(`Date-Time (EDT)`)
    ) %>% 
    filter(elapsed > 3600) # remove first hour
  
  names(out)[grepl('Temp', names(out))] <- 'tempc'
  tempdat <- bind_rows(tempdat, out)
  
}

tempdat <- tempdat %>% 
  clean_names %>% 
  select(
    logger, 
    datetime = date_time_edt, 
    tempc
  ) %>% 
  mutate(
    datetime = force_tz(datetime, tzone = 'America/New_York'),
    yr = year(datetime), 
    logger = gsub(' .*', '', logger), 
    logger = gsub('2145', '', logger), 
    logger = gsub('^.*_.*_(.*$)', '\\1', logger)
  ) %>% 
  left_join(lkup, by = c('yr', 'logger'))

save(tempdat, file = here('data/tempdat.RData'))
