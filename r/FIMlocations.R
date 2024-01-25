##Comparing FIM sites to seagrass loss/gain layers and OTB temp logger stations

# Install packages

options(repos = c(
  tbeptech = 'https://tbep-tech.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

install.packages('tbeptools')
install.packages("remotes")
install.packages("rgdal")

# install the most recent development version of EPA spsurvey from GitHub
remotes::install_github("USEPA/spsurvey", ref = "main")

# Load packages
library(sf)
library(tidyverse)
library(mapview)
library(leaflet)
library(units)
library(stars)
library(raster)
library(here)
library(spatstat)
library(remotes)
library(tbeptools)
library(spsurvey)
library(rgdal)
library(here)

####Seagrass Changes for loss sites
#Load [seagrass change data from 2020 to 2022](https://tbep-tech.github.io/seagrass-analysis/seagrasschange20202022.html) and Bay segment boundaries
load(file = 'data/chgdat20202022.RData')

#load seagrass management areas and Tampa Bay Segments
sgmanagement
tbseg

## colors
Loss_cols <- c('green4', 'tomato1')
names(cols) <- c('gained', 'lost')

cols <- c('green', 'green4')
names(cols) <- c('9113', '9116')

# reproject layers
prj4 <- '+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs'
sgmanagement <- sgmanagement %>% 
  st_transform(crs = prj4)
tbseg<- tbseg %>% 
  st_transform(crs = prj4)
chgdat20202022 <- chgdat20202022 %>% 
  st_transform(crs = prj4)
sgdat2022 <- sgdat2022 %>% 
  st_transform(crs = prj4)

##Filter boundaries to only include OTB
filt_dat <- tbseg %>% 
  filter(bay_segment=='OTB')


#clip sg change data to only Old Tampa Bay segment
sg_clip= st_intersection(chgdat20202022,filt_dat)
sg22_clip= st_intersection(sgdat2022,filt_dat)
sg22_clip

mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = Loss_cols)
st_write(sg_clip, "OTBchgdat20202022.shp")

mapview(sg22_clip, zcol = 'FLUCCSCODE', col.regions = cols)
st_write(sg22_clip, "OTBsg22.shp")

###Load bathy file, filter out all depths >2
load(file = 'data/dem (1).RData')
dem[dem[] < -2] <- NA

mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = cols)+
  mapview(dem, layer.name = 'Depth (m)')

#Bring in FIM selected sites
FIMsites <- read.csv(here("data/TB_FIM_2024_sites.csv"), header=TRUE, stringsAsFactors=FALSE)
#Load FIM historical stations(https://github.com/tbep-tech/nekton-dash/tree/master/data/tbniscr.RData)
load(file = 'data/tbniscr.RData')

FIMsites_filter <- FIMsites %>%
  filter(
    Zone %in% c("A","B"),
    Gear == "20",
    SamplingMonth %in% c(6,7,8,9))

FIMsites_geo <- st_as_sf(FIM_clip,coords = c("Longitude","Latitude"), crs=4326)
FIMsites_geo <- FIM_clip %>% st_transform(crs = prj4)

#Generate a map of FIM sites
mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = Loss_cols)+
  mapview(FIMsites_geo, layer.name = 'FIM sites, 2024', zcol="Stratum")

