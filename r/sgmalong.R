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

# seagrass data -------------------------------------------------------------------------------

meta <- fls %>% 
  filter(grepl('OTB_TEMP_LOGGER_DATA', name)) %>% 
  pull(id) %>% 
  read_sheet(na = c('', 'NA'), col_types = 'DcDcccnnnnnnccccccc') %>% 
  clean_names %>% 
  select(
    deploy_date,
    site = site_id,
    seagrass_percent, 
    seagrass_species, 
    macro_percent, 
    macro_species
  ) 

long_fun <- function(dat, group){

  nmspe <- paste0(group, '_species')
  nmpct <- paste0(group, '_percent')

  out <- dat %>% 
    rename(
      species = !!nmspe,
      percent = !!nmpct
    ) %>% 
    separate_wider_delim(species, delim = '; ', names_sep = '_', too_few = 'align_start') %>% 
    separate_wider_delim(percent, delim = '; ', names_sep = '_', too_few = 'align_start') %>% 
    pivot_longer(matches('species'), names_to = 'grp', values_to = 'grpvalue') %>% 
    pivot_longer(matches('percent'), names_to = 'pct', values_to = 'pctvalue') %>% 
    mutate(
      grp = gsub('^species_', '', grp),
      pct = gsub('^percent_', '', pct),
      group = !!group
    ) %>% 
    filter(grp == pct) %>% 
    filter(!is.na(pctvalue)) %>% 
    select(
      deploy_date, 
      site, 
      group,
      percent = pctvalue,
      species = grpvalue
    )
  
  return(out)
  
}

sgmeta <- meta %>% 
  select(-macro_percent, -macro_species) %>% 
  long_fun(group = 'seagrass')

mameta <- meta %>% 
  select(-seagrass_percent, -seagrass_species) %>% 
  long_fun(group = 'macro')

out <- bind_rows(sgmeta, mameta)

write.csv(out, here('data/sgmalong.csv'), row.names = F)
