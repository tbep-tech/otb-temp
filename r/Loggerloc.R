##Generating random temp logger sites:Summer 2023

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

####Seagrass Changes for loss sites
#Load [seagrass change data from 2020 to 2022](https://tbep-tech.github.io/seagrass-analysis/seagrasschange20202022.html) and Bay segment boundaries
load(file = 'data/chgdat20202022.RData')

#load seagrass management areas and Tampa Bay Segments
sgmanagement
tbseg

## colors
Loss_cols <- c('green4', 'tomato1')
names(cols) <- c('gained', 'lost')

cols <- c('green4', 'tomato1')
names(cols) <- c('9113', '9116')

# reproject layers
prj4 <- '+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs'
sgmanagement <- sgmanagement %>% 
  st_transform(crs = prj4)
tbseg<- tbseg %>% 
  st_transform(crs = prj4)
chgdat20202022 <- chgdat20202022 %>% 
  st_transform(crs = prj4)
sgdat2022 <- sgdat2022%>% 
  st_transform(crs = prj4)

##Filter boundaries to only include OTB
filt_dat <- tbseg %>% 
  filter(bay_segment=='OTB')
filt_dat

#clip sg change data to only Old Tampa Bay segment
sg_clip= st_intersection(chgdat20202022,filt_dat)
sg22_clip= st_intersection(sgdat2022,filt_dat)

mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = Loss_cols)
st_write(sg_clip, "OTBchgdat20202022.shp")

mapview(sg22_clip, zcol = 'FLUCCSCODE', col.regions = cols)
st_write(sg22_clip, "OTBsg22.shp")

###Load bathy file, filter out all depths >2
load(file = 'data/dem (1).RData')
dem[dem[] < -2] <- NA

mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = cols)+
  mapview(dem, layer.name = 'Depth (m)')

##Select a sample stratified by seagrass change area (https://cran.r-project.org/web/packages/spsurvey/vignettes/sampling.html#3_Stratified_sampling)
stata_n<- c(lost=4)
strata_eqprob<- grts(sg_clip,n_base = stata_n, stratum_var = "var")
sp_plot(strata_eqprob)

stata_n_sg<- c("9116"=4)
strata_eqprob_sg<- grts(sg22_clip,n_base = stata_n_sg, stratum_var = "FLUCCSCODE")
sp_plot(strata_eqprob_sg)


#transform list of sites into a dataframe and project to prj4 for consistency
Loss_sites<- as.data.frame(strata_eqprob$sites_base)
write.csv(Loss_sites, "C:\\Users\\sscol\\OneDrive\\Desktop\\Logger\\otb-temp\\data\\loss_sites_2023.csv")
sg_sites<- as.data.frame(strata_eqprob_sg$sites_base)
write.csv(sg_sites, "C:\\Users\\sscol\\OneDrive\\Desktop\\Logger\\otb-temp\\data\\sg_sites_2023.csv")


Loss_sites_geo<-st_as_sf(sites,coords = c("lon_WGS84","lat_WGS84"), crs=4326)
Loss_sites_geo<- sites_geo %>% st_transform(crs = prj4)


#Generate a map selected sites
mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = cols)+
  mapview(dem, layer.name = 'Depth (m)')+
  mapview(Loss_sites_geo, layer.name='statum', color = 'yellow', alpha.regions=0)
