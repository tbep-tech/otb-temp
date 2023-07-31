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
