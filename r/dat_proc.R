library(tidyverse)
library(googlesheets4)
library(googledrive)
library(janitor)
library(lubridate)
library(sf)
library(here)

drive_deauth()
gs4_deauth()

source(here('R/funcs.R'))

# gdrive info ---------------------------------------------------------------------------------

gdrive_pth <- 'https://drive.google.com/drive/folders/1MauA87CetbHCl1IcSolrv2Q8HRXoM9NL'

fls <- drive_ls(gdrive_pth) %>% 
  filter(!grepl('\\.xlsx$', name))

# site metadata -------------------------------------------------------------------------------

metadat <- fls %>% 
  filter(grepl('OTB_TEMP_LOGGER_DATA', name)) %>% 
  pull(id) %>% 
  read_sheet(na = c('', 'NA'), col_types = 'ccDcDcnnnnnncnc') %>% 
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

tempdat <- dltempdat_fun(fls, metadat)

save(tempdat, file = here('data/tempdat.RData'))
