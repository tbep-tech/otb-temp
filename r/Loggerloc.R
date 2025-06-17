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

#### Seagrass Changes for loss sites####
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

#### Processing the change file for OTB ####

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
sg22_clip

mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = Loss_cols)
mapview(sg22_clip, zcol = 'FLUCCSCODE', col.regions = cols)


st_write(sg22_clip, "OTBsg22.shp")
st_write(sg_clip, "OTBchgdat20202022.shp")

#### Selecting new sites ####


###Load bathy file, filter out all depths >2
load(file = 'data/dem (1).RData')
dem[dem[] < -2] <- NA

mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = cols)+
  mapview(dem, layer.name = 'Depth (m)')

##Select a sample stratified by seagrass change area (https://cran.r-project.org/web/packages/spsurvey/vignettes/sampling.html#3_Stratified_sampling)
stata_n_25<- c(lost=50)
strata_eqprob<- grts(sg_clip,n_base = stata_n_25, stratum_var = "var")
sp_plot(strata_eqprob)

stata_n_sg_25<- c(FLUCCSCODE=50)
strata_eqprob_sg<- grts(sg22_clip,n_base = stata_n_sg_25)
sp_plot(strata_eqprob_sg)


#transform list of sites into a dataframe and project to prj4 for consistency
Loss_sites_25<- as.data.frame(strata_eqprob$sites_base)
write.csv(Loss_sites_25, "C:\\Users\\sscol\\OneDrive\\Desktop\\Logger\\otb-temp\\data\\loss_sites_2025.csv")
sg_sites_25<- as.data.frame(strata_eqprob_sg$sites_base)
write.csv(sg_sites_25, "C:\\Users\\sscol\\OneDrive\\Desktop\\Logger\\otb-temp\\data\\sg_sites_2025.csv")


Loss_sites_geo25<-st_as_sf(Loss_sites_25,coords = c("lon_WGS84","lat_WGS84"), crs=4326)
Loss_sites_geo25<-Loss_sites_geo24 %>% st_transform(crs = prj4)

sg_sites_geo25<-st_as_sf(sg_sites_25,coords = c("lon_WGS84","lat_WGS84"), crs=4326)
sg_sites_geo25<-sg_sites_geo24 %>% st_transform(crs = prj4)

#Generate a map selected sites
mapview(sg_clip, zcol = 'var', layer.name = 'Seagrass', col.regions = Loss_cols)+
  mapview(Loss_sites_geo25, layer.name='loss', color = 'yellow', alpha.regions=0)+
  mapview(sg_sites_geo25, layer.name='SG', color = 'blue', alpha.regions=0)

mapview(sg22_clip, zcol = 'FLUCCSCODE', col.regions = cols)+
  mapview(Loss_sites_geo, layer.name='loss', color = 'red', alpha.regions=0)+
  mapview(sg_sites_geo, layer.name='SG', color = 'blue', alpha.regions=0)

          #### 2024 Analysis####

####Seagrass Changes for loss sites
#Load [seagrass change data from 2022 to 2024]

require(sf)
Loss24 <- st_read(dsn = "Tampa_2024_ChangeAnalysis.shp")
SG24 <- st_read(dsn="Tampa_Bay_2024_Seagrass_Final.shp")

#load seagrass management areas and Tampa Bay Segments
tbseg

## colors
Loss_cols <- c('tomato1','green','green4', 'tomato4')
names(cols) <- c('Decrease', 'Gain', 'Increase' , 'Loss')

sg_cols <- c('green', 'green4')
names(cols) <- c('9113', '9116')

#### Processing the change file for OTB ####

# reproject layers
prj4 <- '+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs'

tbseg<- tbseg %>% 
  st_transform(crs = prj4)
Loss24 <- Loss24 %>% 
  st_transform(crs = prj4)
SG24 <- SG24%>% 
  st_transform(crs = prj4)

##Filter boundaries to only include OTB
filt_dat <- tbseg %>% 
  filter(bay_segment=='OTB')
filt_dat

##Filter SG and loss layer to only include 
filt_sg_dat <- SG24 %>% 
  filter(FLUCCS_Cod %in% c('9113','9116'))
filt_sg_dat

filt_loss_dat <- Loss24 %>% 
  filter(Change %in% c('Loss'))
filt_loss_dat

                          filt_gain_dat <- Loss24 %>% 
                               filter(Change %in% c('Gain'))
                          filt_gain_dat

#clip sg change data to only Old Tampa Bay segment
loss_clip= st_intersection(filt_loss_dat,filt_dat)
sg24_clip= st_intersection(filt_sg_dat,filt_dat)
                      gain24=st_intersection(filt_gain_dat,filt_dat)
sg24_clip

mapview(loss_clip, zcol = 'Change', col.regions = Loss_cols)+
mapview(sg24_clip, zcol = 'FLUCCS_Cod', col.regions = sg_cols)


#### Selecting new sites ####

###Load bathy file, filter out all depths >2
load(file = 'data/dem (1).RData')
dem[dem[] < -2] <- NA

##Select a sample stratified by seagrass change area (https://cran.r-project.org/web/packages/spsurvey/vignettes/sampling.html#3_Stratified_sampling)
stata_loss_25<- c(Loss=50)
strata_eqprob<- grts(loss_clip,n_base = stata_loss_25)
sp_plot(strata_eqprob)

stata_n_sg_25<- c(FLUCCSCODE=50)
strata_eqprob_sg<- grts(sg24_clip,n_base = stata_n_sg_25)
sp_plot(strata_eqprob_sg)

                             stata_gain_25<- c(Gain=15)
                             strata_eqprobgain<- grts(gain24,n_base = stata_gain_25)
                             sp_plot(strata_eqprobgain)

#transform list of sites into a dataframe and project to prj4 for consistency
Loss_sites_25_update<- as.data.frame(strata_eqprob$sites_base)
write.csv(Loss_sites_25_update, "C:\\Users\\sscol\\OneDrive\\Desktop\\Logger\\otb-temp\\data\\loss_sites_2025_update.csv")
sg_sites_25<- as.data.frame(strata_eqprob_sg$sites_base)
write.csv(sg_sites_25, "C:\\Users\\sscol\\OneDrive\\Desktop\\Logger\\otb-temp\\data\\sg_sites_2025_update.csv")

gain_sites <- as.data.frame(strata_eqprob$sites_base)
write.csv(gain_sites, "C:\\Users\\sscol\\OneDrive\\Desktop\\Logger\\otb-temp\\data\\gain_sites_2025.csv")

Loss_sites_geo25<-st_as_sf(Loss_sites_25_update,coords = c("lon_WGS84","lat_WGS84"), crs=4326)
Loss_sites_geo25<-Loss_sites_geo25 %>% st_transform(crs = prj4)

sg_sites_geo25<-st_as_sf(sg_sites_25,coords = c("lon_WGS84","lat_WGS84"), crs=4326)
sg_sites_geo25<-sg_sites_geo25 %>% st_transform(crs = prj4)

#Generate a map selected sites
mapview(sg24_clip, zcol = 'FLUCCS_Cod', col.regions = sg_cols)+
  mapview(Loss_sites_geo25, layer.name='loss', color = 'yellow', alpha.regions=0)+
  mapview(sg_sites_geo25, layer.name='SG', color = 'blue', alpha.regions=0)

mapview(Loss24, zcol = 'Change', col.regions = Loss_cols)+
  mapview(Loss_sites_geo25, layer.name='Loss', color = 'yellow', alpha.regions=0)+
  mapview(sg_sites_geo25, layer.name='SG', color = 'blue', alpha.regions=0)
