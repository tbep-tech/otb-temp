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
    depthm = depth_m
  ) %>%
  filter(!is.na(logger)) %>% 
  mutate(
    yr = year(deploy_date), 
    logger = sprintf('%04d', logger)
  ) %>%
  unite('yr_site_logger', yr, site, logger, sep = '_', remove = F) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

save(metadat, file = here('data/metadat.RData'))

# temp data -----------------------------------------------------------------------------------

load(file = here('data/metadat.RData'))

# logger, site lookup
lkup <- metadat %>% 
  st_set_geometry(NULL) %>% 
  select(yr_site_logger, yr, site, logger) %>% 
  unique()

datfls <- fls %>% 
  .[!grepl('OTB_TEMP_LOGGER_DATA|DATASHEETS|^Avg', .$name),] 

tempdatrw <- NULL
for(i in 1:nrow(datfls)){
  
  cat(i,'\n')
  
  id <- datfls[i, ] %>% 
    pull(id)
  
  yr_site_logger <- datfls[i, ] %>% 
    pull(name)
  
  out <- read_sheet(id) %>% 
    mutate(
      yr_site_logger = yr_site_logger, 
      elapsed = `Date-Time (EDT)` - min(`Date-Time (EDT)`)
    ) %>% 
    filter(elapsed > 3600) # remove first hour
  
  names(out)[grepl('Temp', names(out))] <- 'tempc'
  tempdatrw <- bind_rows(tempdatrw, out)
  
}

tempdat <- tempdatrw %>% 
  clean_names %>% 
  select(
    yr_site_logger, 
    datetime = date_time_edt, 
    tempc
  ) %>% 
  mutate(
    datetime = force_tz(datetime, tzone = 'America/New_York'),
    yr = year(datetime), 
    logger = gsub('^.*_.*_(.*$)', '\\1', yr_site_logger),
    site = gsub('^.*_(.*)_.*$', '\\1', yr_site_logger)
  ) %>% 
  filter(yr_site_logger %in% lkup$yr_site_logger)

save(tempdat, file = here('data/tempdat.RData'))
