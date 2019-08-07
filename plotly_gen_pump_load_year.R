# Based on Kaifeng's script which Ella sent me on April 16 ~9:30AM
# Originally called '5_plot_ldc_files.R'
# This should create a 3d plotly object showing the generation and pumped load for every day and how of the year

pacman::p_load(plot3D, ggplot2, plotly, tidyr, dplyr, data.table)

#set up plot.ly account
Sys.setenv("plotly_username"="ellachou")
Sys.setenv("plotly_api_key"="d0QE19HmJM1K0vwvXxZc")

saveWidgetFix <- function (widget,file,...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd<-getwd()
  on.exit(setwd(wd))
  outDir<-dirname(file)
  file<-basename(file)
  setwd(outDir);
  htmlwidgets::saveWidget(widget,file=file,...)
}


plotly_3d_gen_pumpload <- function(DR_data, DR_data_categories, agg.style = "Total", date_focus = NA, hour_focus = NA, 
                                   scen_focus = NA, sd.... = "plots", zone_focus = NA, period_name = ""){
  
  # -------------------------------------------------- /
  # Global objects, etc
  # -------------------------------------------------- /
  if(agg.style == "Regional"){stop("this particular function currently does not support Regional aggregation, use Total or Zonal for now.")}
  
  dr.cat <- grep("^DR_", names(gen.cat.mapping), perl = T, value = T)
  # DR_data <- dr_gen_pump_load_data_comp(DR_categories = dr.cat, aggregation = agg.style)
  
  # -------------------------------------------------- /
  # Apply Filters
  # -------------------------------------------------- /
  
  if(all(!is.na(date_focus))){
    if(!is.POSIXct(date_focus)){stop("date_focus must be a POSIXct object")}
    if(!(units(diff(date_focus)) == "hours")){stop("This is currently set up to handle datetimes with hour-granularity, although it could be changed to handle larger intervals if necessary")}
    if(!all(diff(date_focus) == 1)){stop("This is currently set up to handle datetimes with 1 hour differences, although it could be changed to handle larger intervals if necessary")}
    
    DR_data <- DR_data[time %in% date_focus]
    
    if(nrow(DR_data) <= 0){stop("You have selected dates outside of the range of the data, DR_data is now empty, cannot proceed, please change date_focus")}
    if(nchar(period_name) == 0){warning("You have subselected data by dates but have provided no period name by which to distinguish the data, this may lead to overwriting previous runs")}
  }
  
  if(all(!is.na(hour_focus))){
    if(!is.integer(hour_focus)){stop("hour_focus should be of class integer or a sequence of integers, not characters of numerals with decimals")}
    if(!all(hour_focus %in% 0:23)){stop("all hour_focus entries must be between 0 and 23")}
    
    DR_data <- DR_data[lubridate::hour(time) %in% hour_focus]
    if(nrow(DR_data) <= 0){stop("You have selected hours outside of the range of the data, DR_data is now empty, cannot proceed, please change hour_focus")}
    if(nchar(period_name) == 0){warning("You have subselected data by hours but have provided no period name by which to distinguish the data, this may lead to overwriting previous runs")}
  }
  
  if(all(!is.na(scen_focus))){
    if(all(!is.character(scen_focus))){stop("scen focus must be a character string")}
    # if(length(scen_focus) > 1){stop("currently this is only set up to handle one scenario at a time, please lapply this function over scenario names if you want to see different scenarios, or get Thomas to rewrite the function")}
    
    DR_data <- DR_data[scenario %in% scen_focus]
    if(nrow(DR_data) <= 0){stop("You have selected scenarios outside of the range of the data, DR_data is now empty, cannot proceed, please change scen_focus")}
  }
  
  if(all(!is.na(zone_focus)) & agg.style == "Zonal"){
    if(all(!is.character(zone_focus))){stop("zone_focus must be a character string")}
    DR_data <- DR_data[Location %in% zone_focus]
  }
  
  # -------------------------------------------------- /
  # lapply over properties and scenarios
  # -------------------------------------------------- /
  
  DR_data_focus_scen <- DR_data[, unique(scenario)]
  DR_data_properties <- DR_data[, unique(property)]
  DR_data_groups <- DR_data[, unique(category)]
  
  if(agg.style == "Total"){
    DR_data[, Location := "Total"]
  }
  
  DR_data_zones <- DR_data[, unique(Location)]
  
  file.loc.group.zone.prop.scen <- lapply(DR_data_focus_scen, function(scen..., zone... = DR_data_zones, DR_data_focus... = DR_data, prop... = DR_data_properties, group... = DR_data_groups, sd... = sd...., pn... = period_name){
    
    file.loc.group.zone.prop <- lapply(prop..., function(prop.., scen.. = scen..., zone.. = zone..., DR_data_focus.. = DR_data_focus..., sd.. = sd..., pn.. = pn..., group.. = group...){
      
      file.loc.group.zone <- lapply(zone.., function(zone., prop. = prop.., scen. = scen.., DR_data_focus. = DR_data_focus.., sd. = sd.., pn. = pn.., group. = group..){
        
        file.loc.group <- lapply(group., function(group, zone = zone., prop = prop., scen = scen., DR_data_focus = DR_data_focus., sd = sd., pn = pn.){
          
          DR_data_focus <- DR_data_focus[property == prop & scenario == scen & Location == zone & category == group,]
          DR_data_focus[, `:=`(property = NULL, scenario = NULL, Location = NULL)]
          DR_data_focus[, `:=`(doy = as.Date(time),
                               hour = lubridate::hour(time))]
          
          DR_data_focus_reshaped <- dcast.data.table(DR_data_focus, "hour~doy", value.var = "value")
          z <- as.matrix(DR_data_focus_reshaped[, .SD, .SDcols = !"hour"])
          
          hours = paste0(as.character(DR_data_focus[, sort(unique(hour)),]),":00")
          dates = DR_data_focus[, sort(unique(doy)),]
          
          # -------------------------------------------------- /
          # Create plot image, save
          # -------------------------------------------------- /
          
          p <- plot_ly(x = dates, 
                       y = hours, 
                       z = z) %>% add_surface() %>%
            layout(scene = list(xaxis = list(title = 'Day', tickformat = "%b-%d"),
                                yaxis = list(title = 'Hour'),
                                zaxis = list(title = paste0('DR', prop, ' (MW)'))),
                   title = paste0(zone, "-", group, "-", scen, ": ", prop))
          
          saveWidgetFix(p, file = file.path(sd, paste0(paste(pn, group, zone, scen, prop, sep = "_"),".html")),
                        title = paste(pn, zone, scen, prop, sep = "_"), 
                        selfcontained = T)
          
          return(file.path(sd, paste0(paste(pn, group, zone, scen, prop, sep = "_"),".html")))
          
        })
        
        names(file.loc.group) <- group.

        
      })
      
      names(file.loc.group.zone) <- zone..
      return(file.loc.group.zone)
      
    })
    
    names(file.loc.group.zone.prop) <- prop...
    return(file.loc.group.zone.prop)
    
  })
  
  names(file.loc.group.zone.prop.scen) <- DR_data_focus_scen
  return(file.loc.group.zone.prop.scen)
  
}

