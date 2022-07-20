##Generating random temp logger sites

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

options(repos = c(
  tbeptech = 'https://tbep-tech.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# install tbeptools
install.packages('tbeptools')
library(tbeptools)

# Installing from GitHub requires you first install the remotes package
install.packages("remotes")

# install the most recent development version of EPA spsurvey from GitHub
remotes::install_github("USEPA/spsurvey", ref = "main")
# load the most recent development version from GitHub
library(spsurvey)

#Load MBeck previous [sg analysis change data file](https://github.com/tbep-tech/seagrass-analysis)
chgdat
sgbound=st_read('Seagrass_Segment_Boundaries.shp')

#load seagrass management areas
sgmanagement

# reproject layers
prj4 <- '+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs'
sgmanagement <- sgmanagement %>% 
  st_transform(crs = prj4)
sgbound<- sgbound %>% 
  st_transform(crs = prj4)

##Filter boundaries to only include OTB
filt_dat <- sgbound %>% 
  filter(SEAGRASSSE=='OLD TAMPA BAY')
filt_dat

#clip sg change data to only Old Tampa Bay segment
sg_clip= st_intersection(chgdat,filt_dat)

## colors
cols <- c('green4', 'tomato1')
names(cols) <- c('gained', 'lost')

##Mapping the sampling frame
mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = cols)+
  mapview(filt_dat, layer.name= 'OTB',col.regions='black', alpha.regions=0)+
  mapview(sgmanagement, layer.name='SG Mngment Areas', color = 'blue', alpha.regions=0)

##creating list of sites for areas of seagrass loss

loss_n=c(lost=50,gained=0)
uneqprob=grts(sg_clip,n_base=50, caty_var = 'var', caty_n = loss_n )
sp_plot(uneqprob)

gain_n=c(lost=0, gained=50)
uneqprob_gain=grts(sg_clip,n_base=50, caty_var = 'var', caty_n = gain_n )
