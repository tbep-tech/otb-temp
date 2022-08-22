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
library(remotes)

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

#Load MBeck previous [sg analysis change data file](https://github.com/tbep-tech/seagrass-analysis) and Bay segment boundaries
chgdat

#load seagrass management areas and Tampa Bay Segments
sgmanagement
tbseg

# reproject layers
prj4 <- '+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs'
sgmanagement <- sgmanagement %>% 
  st_transform(crs = prj4)
tbseg<- tbseg %>% 
  st_transform(crs = prj4)

##Filter boundaries to only include OTB
filt_dat <- tbseg %>% 
  filter(bay_segment=='OTB')
filt_dat

#clip sg change data to only Old Tampa Bay segment
sg_clip= st_intersection(chgdat,filt_dat)

## colors
cols <- c('green4', 'tomato1')
names(cols) <- c('gained', 'lost')

##Select a sample stratified by seagrass change area (https://cran.r-project.org/web/packages/spsurvey/vignettes/sampling.html#3_Stratified_sampling)
stata_n<- c(gained = 3, lost=3)
strata_eqprob<- grts(sg_clip,n_base = stata_n, stratum_var = "var")
sp_plot(strata_eqprob)

#transform list of sites into a dataframe and project to prj4 for consistency
sites<- as.data.frame(strata_eqprob$sites_base)
write.csv(sites, "C:\\Users\\sscol\\OneDrive\\Desktop\\Logger\\otb-temp\\data\\sites.csv")

sites_geo<-st_as_sf(sites,coords = c("lon_WGS84","lat_WGS84"), crs=4326)
sites_geo<- sites_geo %>% st_transform(crs = prj4)


#Generate a map selected sites
mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = cols)+
  mapview(filt_dat, layer.name= 'OTB',col.regions='black', alpha.regions=0)+
  mapview(sites_geo, layer.name='statum', color = 'blue', alpha.regions=0)
