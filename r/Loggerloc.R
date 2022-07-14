library(usethis)
use_git_config(user.name="sscolaro", user.email="sscolaro@tbep.org")

##Generating random temp logger sites

#Load MBeck previous sg analysis change data file
chgdat
load("C:/Users/sscol/OneDrive/Desktop/Logger/dem.RData")

#only include OTB segment
sgbound=st_read('Seagrass_Segment_Boundaries.shp')

#load seagrass management areas
sgmanagement

# Install and load packages
library(sf)
library(tidyverse)
library(mapview)
library(leaflet)
library(units)
library(stars)
library(raster)
library(here)
library(spatstat)
library(tbeptools)

# Installing from GitHub requires you first install the remotes package
install.packages("remotes")

# install the most recent development version from GitHub
remotes::install_github("USEPA/spsurvey", ref = "main")
# load the most recent development version from GitHub
library(spsurvey)



##Filter boundaries to only include OTB
filt_dat <- sgbound %>% 
  filter(SEAGRASSSE=='OLD TAMPA BAY')
filt_dat

# reproject layers
prj4 <- '+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs'
sgmanagement <- sgmanagement %>% 
  st_transform(crs = prj4)

# reproject dem, convert to stars 
demstr <- dem %>% 
  projectRaster(crs = prj4) %>% 
  st_as_stars()

# colors
cols <- c('green4', 'tomato1')
names(cols) <- c('gained', 'lost')

mapview(sgbound, layer.name= 'OTB')
mapview(chgdat, zcol = 'var', layer.name = 'Seagrass', col.regions = cols)+
  mapview(filt_dat, layer.name= 'OTB',col.regions='black', alpha.regions=0)+
  mapview(sgmanagement, layer.name='SG Mngment Areas', color = 'blue', alpha.regions=0)






















##maybe need to compare virtual coordinates to chngdat, then filter by "true"
## overlapping locations?

Seqlat <- seq(from=28.03320, to=28.83604, by=.0001)
Seqlong <- seq(from=-82.76856, to=-82.5087, by=.0001)
Latitude <- sample(Seqlat, size=100, replace=TRUE)
Longitude <- sample(Seqlong, size=100, replace=TRUE)
VirtualCoordinates <- data.frame(x= Longitude, y = Latitude)
st_crs(VirtualCoordinates)



