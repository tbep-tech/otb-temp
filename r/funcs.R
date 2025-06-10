tsplo_dd <- function(tempdat, dd){
  
  yr_site_logger <- dd
  
  rng <- range(tempdat$tempc, na.rm  = T)
  
  p <- tempdat %>% 
    filter(yr_site_logger %in% !!yr_site_logger) %>% 
    ggplot(aes(x = datetime, y = tempc, color = site)) + 
    coord_cartesian(ylim = rng) +
    scale_y_continuous(breaks = seq(round(rng[1], 0), rng[2], by = 2)) +
    geom_line() + 
    theme_minimal() + 
    scale_color_viridis_d() +
    theme(
      panel.grid.minor = element_blank(), 
      legend.position  = 'top'
    ) +
    labs(
      y = 'Temp (C)',
      x = NULL, 
      color = 'Site'
    )
  
  pout <- ggplotly(p)
  
  return(pout)
  
}

mpplo_dd <- function(metadat, dd){
  
  yr_site_logger <- dd

  bnds <- st_bbox(metadat)
  
  mout <- metadat %>% 
    filter(yr_site_logger %in% !!yr_site_logger) %>% 
    mapview(zcol = 'site', layer.name = 'Site') %>% 
    .@map %>% 
    fitBounds(lng1 = bnds[[1]], lat1 = bnds[[2]], lng2 = bnds[[3]], lat2 = bnds[[4]])
  
  return(mout)
    
}

dlmetadat_fun <- function(fl){

  out <- fls %>% 
    filter(grepl('OTB_TEMP_LOGGER_DATA', name)) %>% 
    pull(id) %>% 
    read_sheet(na = c('', 'NA'), col_types = 'ccDcDcnnnnnncnc') %>% 
    clean_names %>% 
    select(
      deploy_date,
      logger = logger_id,
      site = site_id,
      stratum, 
      lat, 
      long, 
      depthm = depth_m
    ) %>%
    filter(!is.na(logger)) %>% 
    mutate(
      yr = year(deploy_date), 
      logger = sprintf('%04d', logger)
    ) %>%
    unite('yr_site_logger', yr, site, logger, sep = '_', remove = F) %>% 
    st_as_sf(coords = c('long', 'lat'), crs = 4326)
  
  return(out)
  
}

dltempdat_fun <- function(fls, metadat){
  
  flexts <- file.exists(here('data/tempdat.RData'))
  
  # logger, site lookup
  lkup <- metadat %>% 
    st_set_geometry(NULL) %>% 
    select(yr_site_logger, yr, site, logger) %>% 
    unique()
  
  datfls <- fls %>% 
    .[!grepl('OTB_TEMP_LOGGER_DATA|DATASHEETS|^Avg|calcheck', .$name),] 
    
  if(flexts){
    browser()
    load(file = here('data/tempdat.RData'))
    
    unilog <- tempdat$yr_site_logger %>% 
      unique()

    datfls <- datfls %>% 
      .[!grepl(paste(unilog, collapse = '|'), .$name),]
  
    if(nrow(datfls) == 0){
      cat('up to date\n')
      return(tempdat)
    }
      
  }
  
  tempdatrw <- NULL
  for(i in 1:nrow(datfls)){
    
    cat(i,'\n')
    
    id <- datfls[i, ] %>% 
      pull(id)
    
    yr_site_logger <- datfls[i, ] %>% 
      pull(name)
    
    out <- read_sheet(id) %>% 
      mutate(
        yr_site_logger = yr_site_logger, 
        elapsed = `Date-Time (EDT)` - min(`Date-Time (EDT)`)
      ) %>% 
      filter(elapsed > 3600) # remove first hour
    
    names(out)[grepl('Temp', names(out))] <- 'tempc'
    tempdatrw <- bind_rows(tempdatrw, out)
    
  }
  
  tempdatrw <- tempdatrw %>% 
    clean_names %>% 
    select(
      yr_site_logger, 
      datetime = date_time_edt, 
      tempc
    ) %>% 
    mutate(
      datetime = force_tz(datetime, tzone = 'America/New_York'),
      yr = year(datetime), 
      logger = gsub('^.*_.*_(.*$)', '\\1', yr_site_logger),
      site = gsub('^.*_(.*)_.*$', '\\1', yr_site_logger)
    ) %>% 
    filter(yr_site_logger %in% lkup$yr_site_logger)
  
  if(flexts)
    tempdat <- bind_rows(tempdat, tempdatrw)
  else
    tempdat <- tempdatrw
  
  return(tempdat)
  
}
