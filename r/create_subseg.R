# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(mapview)
library(mapedit)
library(tbeptools)
library(lwgeom)

otbseg <- tbseg %>% 
  dplyr::filter(bay_segment == 'OTB')

# draw lines, then intersect ------------------------------------------------------------------

# draw lines manually, the rest won't work unless drawn
m <- mapview(otbseg)
x <- drawFeatures(m, record = T)

otbsub <- st_split(otbseg, x) %>% 
  st_collection_extract('POLYGON') %>% 
  mutate(
    subseg = c('NW', 'NE', 'CE', 'SE', 'SW', 'CW')
  ) %>% 
  select(subseg)

st_write(otbsub, here('data/otbsub.shp'))
save(otbsub, file = here('data/otbsub.RData'))
