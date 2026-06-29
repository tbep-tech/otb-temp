tsplo_dd <- function(tempdat, dd){
  
  # Filter data first
  toplo <- tempdat |> 
    filter(yr_site_logger %in% !!dd) |>
    mutate(datetime_ms = as.numeric(datetime) * 1000)
  
  # axis range
  rng <- range(tempdat$tempc, na.rm  = T)

  # Calculate average for reference line
  avev <- mean(toplo$tempc, na.rm = TRUE)
  
  # Create base highchart
  hc <- toplo |>
    hchart("line", 
           hcaes(x = datetime_ms, y = tempc, group = site)) |>
    hc_chart(
      zoomType = "xy",
      backgroundColor = "white"  # White background
    ) |>
    hc_plotOptions(
      line = list(
        marker = list(enabled = FALSE)
      ),
      series = list(
        backgroundColor = "white"  # Ensure plot area is white
      )
    ) |>
    hc_tooltip(
      pointFormat = "<span style='color:{series.color}'>{series.name}</span>: <b>{point.y:.2f}°C ({point.tempf:.2f}°F)</b><br/>",
      shared = FALSE,
      backgroundColor = "white",
      borderColor = "#ccc",
      borderRadius = 3,
      pointFormatter = JS("
        function() {
          var tempF = (this.y * 9/5) + 32;
          return '<span style=\"color:' + this.series.color + '\">' + this.series.name + 
                 '</span>: <b>' + this.y.toFixed(2) + '°C (' + tempF.toFixed(2) + '°F)</b><br/>';
        }
      ")
    ) |>
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(backgroundColor = "white"),  # White background for exports
        plotOptions = list(
          series = list(
            dataLabels = list(
              style = list(textOutline = "none")
            )
          )
        )
      ),
      buttons = list(
        contextButton = list(
          menuItems = list(
            "viewFullscreen",
            "separator",
            "downloadPNG",
            "separator",
            "downloadCSV"
          )
        )
      )
    ) |>
    hc_xAxis(
      type = "datetime",
      title = list(text = "")
    ) |>
    hc_yAxis(
      title = list(text = "Temp (C)"), 
      min = rng[1],
      max = rng[2]
    ) |>
    highcharter::hc_yAxis_multiples(
      list(
        title = list(text = "Temp (C)"), 
        min = rng[1],
        max = rng[2], 
        id = 'primary-axis'
      ),
      list(
        title = list(text = "Temp (F)"),
        id = "secondary-axis",
        opposite = TRUE,
        # Link the axes with proper conversion
        linkedTo = 0,
        labels = list(
          formatter = highcharter::JS(
            "function() {
              return ((this.value  * 9/5) + 32).toFixed(1);
            }"
          )
        )
      )
    ) |> 
    hc_add_series(
      data = list(list(x = min(toplo$datetime_ms, na.rm = TRUE), y = avev),
                  list(x = max(toplo$datetime_ms, na.rm = TRUE), y = avev)),
      type = "line",
      name = "Mean",
      color = "black",
      dashStyle = "dash",
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE
    ) |>
    hc_legend(
      align = "center", 
      verticalAlign = "top",
      symbolHeight = 5 
    ) |> 
    hc_colors(viridis::viridis(length(unique(toplo$site))))
  
  return(hc)

}

mpplo_dd <- function(tempdat, metadat, dd){

  bnds <- st_bbox(metadat)

  yrsitlog <- tempdat |> 
    filter(yr_site_logger %in% !!dd) |> 
    pull(yr_site_logger) |> 
    unique()
  toplo <- metadat %>%
    filter(yr_site_logger %in% yrsitlog)

  sites <- sort(unique(toplo$site))
  pal <- colorFactor(viridis::viridis(length(sites)), domain = sites)

  mout <- leaflet(toplo) %>%
    addProviderTiles(providers$CartoDB.Positron,  group = "CartoDB.Positron") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
    addProviderTiles(providers$OpenStreetMap,       group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery,  group = "Esri.WorldImagery") %>%
    addProviderTiles(providers$OpenTopoMap,         group = "OpenTopoMap") %>%
    addCircleMarkers(
      color = ~pal(site),
      fillColor = ~pal(site),
      fillOpacity = 1,
      opacity = 1,
      radius = 6,
      label = ~site,
      group = "Site"
    ) %>%
    addLegend(
      position = "topright",
      pal = pal,
      values = ~site,
      title = "Site",
      opacity = 1
    ) %>%
    addLayersControl(
      baseGroups = c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap",
                     "Esri.WorldImagery", "OpenTopoMap"),
      overlayGroups = "Site",
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    fitBounds(
      lng1 = bnds[[1]], lat1 = bnds[[2]],
      lng2 = bnds[[3]], lat2 = bnds[[4]]
    )

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

    load(file = here('data/tempdat.RData'))

    # remove records whose source file no longer exists on Google Drive
    tempdat <- tempdat %>%
      filter(yr_site_logger %in% datfls$name)

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
