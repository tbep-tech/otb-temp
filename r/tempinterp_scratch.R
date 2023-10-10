library(akima)
library(sf)
library(tbeptools)

prj <- 6443

otbseg <- tbseg[tbseg$bay_segment == 'OTB', ] %>% 
  st_transform(crs = prj) %>% 
  st_bbox()

locdat <- metadat %>% 
  select(site) %>% 
  st_transform(crs = prj) %>% 
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>% 
  st_set_geometry(NULL) %>% 
  summarise(
    lon = mean(lon), 
    lat = mean(lat), 
    .by = site
  )

tointerp <- dat %>% 
  mutate(
    date = date(datetime)
  ) %>% 
  unite('grp', date, site, remove = F) %>% 
  mutate(
    n = n(), 
    .by = c('grp')
  ) %>% 
  filter(n == 144) %>% 
  summarise(
    tempc = mean(tempc), 
    .by = c(date, site)
  ) %>% 
  filter(date == min(date)) %>% 
  inner_join(locdat, by = 'site')

# doesnt require and akima requires regular grid
xint <- seq(otbseg[['xmin']], otbseg[['xmax']], length = 100)
yint <- seq(otbseg[['ymin']], otbseg[['ymax']], length = 100)
asinterp <- interp::interpp(x = tointerp$lon, y = tointerp$lat, z = tointerp$tempc, 
       xo = xint,
       yo = yint, extrap = T)

tmp <- asinterp$z %>% 
  as.matrix() %>% 
  data.frame() 

names(yint) <- names(tmp)
yint <- yint %>% 
  enframe(value = 'lat')
tmp2 <- tmp %>%  
  mutate(
    lon = xint
  ) %>%
  pivot_longer(names_to = 'name', values_to = 'tempc', -lon) %>% 
  left_join(yint, by = 'name') %>% 
  select(lon, lat, tempc) %>% 
  st_as_sf(coords= c('lon', 'lat'), crs = prj)

