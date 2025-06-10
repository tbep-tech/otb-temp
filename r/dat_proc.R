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

# may need to step through function
metadat <- dlmetadat_fun(fls)

save(metadat, file = here('data/metadat.RData'))

# temp data -----------------------------------------------------------------------------------

load(file = here('data/metadat.RData'))

# may need to step through function
tempdat <- dltempdat_fun(fls, metadat)

# remove non-deployment data
tempdat <- tempdat %>% 
  filter(!(yr_site_logger %in% c('2024_4NWB3_7775') & datetime > ymd_hms('2024-07-15 11:11:00', tz = 'America/New_York')))

save(tempdat, file = here('data/tempdat.RData'))

