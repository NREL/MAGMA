# This contains plot functions created by Thomas Bowen for Ella Zhou and the Electric Futures Study 2050
# These plots are based on a meeting with Ella on 04/08/2019 ~10 AM
# Notes for the plots are in "C:\Users\tbowen\Documents\Electric Futures Study\Notes_Meeting_04-08-2019.txt"

# ---------------------------------------------------------------------------------------- /
# ------------------------------ Libraries, functions, etc. ------------------------------
# ---------------------------------------------------------------------------------------- /



# ---------------------------------------------------------------------------------------- /
#         ---------------------------- DR graphs --------------------------------
# ---------------------------------------------------------------------------------------- /

# There is a category of generators called 'DR', 
# All of the technologies in this group start with 'DR_' (e.g. DR_tran_hdv, DR_tran_idv, etc.)
#       See 'gen.cat.mapping' for all categories
# These are currently grouped under the umbrella 'DR'

# For each DR category we want:
#     1) a line graph for the generation and the pump load (this should be done under the key period dispatch);
#     2) a line graph for generation and pump load for the entire year averaged by hour and day of the week
# - This should yield a representative week for the entire year
#     3) a plotly interactive graph, 3 axes:
#           a) hours of the day 1:24;
#           b) days of the year 1:365;
#           c) height = generation & height = pumped load where value is graded by color

# ---------------------------------------------------------------------------------------- /
# combine and filter DR data --------------
# ---------------------------------------------------------------------------------------- /

dr_gen_pump_load_data_comp <- function(DR_categories, DR_category_groups, generation_data = interval.generation, 
                                       pumped_load_data = interval.pump.load, 
                                       keep_category = FALSE, aggregation = "Total", 
                                       region_zone_generator_map = region.zone.mapping, zone_focus = NA,
                                       date_focus = NA, hour_focus = NA, scen_focus = NA, region_focus = NA){
  
  if(!(aggregation %in% c("Zonal", "Regional", "Total"))){stop("'aggregation variable must be one of 'Zonal', 'Regional', or 'Total'")}
  
  
  # --------------------------- /
  # Find DR Categories and Region/Zone ---------------
  # --------------------------- /
  generation_data_DR <- generation_data[category %in% DR_categories,,]
  pumped_load_data_DR <- pumped_load_data[category %in% DR_categories,,]
  
  # -------------------------------------------------- /
  # Apply Filters
  # -------------------------------------------------- /
  
  # The Goal here is to get these as small as possible before attempting any cumbersome merges
  
  if(all(!is.na(date_focus))){
    if(!is.POSIXct(date_focus)){stop("date_focus must be a POSIXct object")}
    if(!(units(diff(date_focus)) == "hours")){stop("This is currently set up to handle datetimes with hour-granularity, although it could be changed to handle larger intervals if necessary")}
    if(!all(diff(date_focus) == 1)){stop("This is currently set up to handle datetimes with 1 hour differences, although it could be changed to handle larger intervals if necessary")}
    
    generation_data_DR <- generation_data_DR[time %in% date_focus]
    pumped_load_data_DR <- pumped_load_data_DR[time %in% date_focus]
    
    if(nrow(generation_data_DR) <= 0 | nrow(pumped_load_data_DR) <= 0){
      stop("You have selected dates outside of the range of the data, the datasets are now empty, 
           cannot proceed, please change date_focus")
    }
  }
  
  if(all(!is.na(hour_focus))){
    if(!is.integer(hour_focus)){stop("hour_focus should be of class integer or a sequence of integers, not characters of numerals with decimals")}
    if(!all(hour_focus %in% 0:23)){stop("all hour_focus entries must be between 0 and 23")}
    
    generation_data_DR <- generation_data_DR[lubridate::hour(time) %in% hour_focus]
    pumped_load_data_DR <- pumped_load_data_DR[lubridate::hour(time) %in% hour_focus]
    
    if(nrow(generation_data_DR) <= 0 | nrow(pumped_load_data_DR) <= 0){
      stop("You have selected hours outside of the range of the data, , the datasets are now empty, 
           cannot proceed, please change date_focus")}
  }
  
  if(all(!is.na(scen_focus))){
    if(all(!is.character(scen_focus))){stop("scen focus must be a character string")}
    
    generation_data_DR <- generation_data_DR[scenario %in% scen_focus]
    pumped_load_data_DR <- pumped_load_data_DR[scenario %in% scen_focus]
    if(nrow(generation_data_DR) <= 0 | nrow(pumped_load_data_DR) <= 0){
      stop("You have selected scenarios outside of the range of the data, the datasets 
                                are now empty, cannot proceed, please change scen_focus")
    }
  }
  
  
  
  generation_data_DR <- merge(generation_data_DR, 
                              region_zone_generator_map[, .(Region, Zone, name)],
                              by = "name", all.x = T)
  pumped_load_data_DR <- merge(pumped_load_data_DR, 
                               region_zone_generator_map[, .(Region, Zone, name)], 
                               by = "name", all.x = T)
  
  # If focusing on regions, remove others
  if(all(!is.na(region_focus)) & aggregation %in% c("Zonal", "Regional")){
    if(all(!is.character(region_focus))){stop("region_focus must be a character string")}
    generation_data_DR <- generation_data_DR[Region %in% region_focus]
    pumped_load_data_DR <- pumped_load_data_DR[Region %in% region_focus]
    if(all(!is.na(region_focus)) & agg.style == "Zonal"){
      warning("You have chosen to aggregate by Zone but have subselected Regions 
              (and potential zones), this means that zonal aggregations will not 
              represent the full zone")
    }
    if(all(!is.na(region_focus)) & aggregation == "Total"){
      warning("You have chosen to aggregate by Total but have subselected regions 
              (and potentialy zones), this means that total aggregation will not 
              represent the full area")
    }
  }
  
  # If focusing on zones, remove others
  if(all(!is.na(zone_focus)) & aggregation == "Zonal"){
    if(all(!is.character(zone_focus))){stop("zone_focus must be a character string")}
    generation_data_DR <- generation_data_DR[Zone %in% zone_focus]
    pumped_load_data_DR <- pumped_load_data_DR[Zone %in% zone_focus]
    if(all(!is.na(zone_focus)) & aggregation == "Total"){
      warning("You have chosen to aggregate by Total but have subselected Zones 
              (and potentialy regions), this means that total aggregation will not 
              represent the full area")
    }
  }
  
  if(keep_category){
    
    generation_data_DR[, category := str_replace_all(category, pattern = DR_category_groups)]
    pumped_load_data_DR[, category := str_replace_all(category, pattern = DR_category_groups)]
    
  }
  
  
  # --------------------------- /
  # Aggregate by non-time levels ---------------
  # --------------------------- /
  
  if(aggregation == "Total"){
    
    if(!keep_category){
      
      generation_data_DR <- generation_data_DR[, .(value = sum(value)), 
                                               by = c("property", "time", "scenario")]
      pumped_load_data_DR <- pumped_load_data_DR[, .(value = -sum(value)), 
                                                 by = c("property", "time", "scenario")]
      
    }else{
      
      generation_data_DR <- generation_data_DR[, .(value = sum(value)), 
                                               by = c("property", "time", "scenario", "category")]
      pumped_load_data_DR <- pumped_load_data_DR[, .(value = -sum(value)), 
                                                 by = c("property", "time", "scenario", "category")]
    }
    
    
    
  }else{
    
    
    if(!keep_category){
      
      generation_data_DR <- 
        generation_data_DR[, .(value = sum(value)), 
                           by = c("property", "time", "scenario", 
                                  c("Region", "Zone")[match(aggregation, c("Regional", "Zonal"))])]
      
      pumped_load_data_DR <- 
        pumped_load_data_DR[, .(value = -sum(value)), 
                            by = c("property", "time", "scenario", 
                                   c("Region", "Zone")[match(aggregation, c("Regional", "Zonal"))])]
      
    }else{
      
      generation_data_DR <- 
        generation_data_DR[, .(value = sum(value)), 
                           by = c("property", "time", "scenario", "category", 
                                  c("Region", "Zone")[match(aggregation, c("Regional", "Zonal"))])]
      
      pumped_load_data_DR <- 
        pumped_load_data_DR[, .(value = -sum(value)), 
                            by = c("property", "time", "scenario", "category", 
                                   c("Region", "Zone")[match(aggregation, c("Regional", "Zonal"))])]
      
    }
  } 
  
  
  total_data_DR <- rbindlist(list(generation_data_DR, pumped_load_data_DR), 
                             use.names = T, fill = T)
  
  if(aggregation == "Total"){total_data_DR[, Location := "Total"]}
  if(aggregation == "Zonal"){setnames(total_data_DR, "Zone", "Location")}
  if(aggregation == "Region"){setnames(total_data_DR, "Region", "Location")}
  
  return(total_data_DR)
  
}

# ---------------------------------------------------------------------------------------- /
# DR graphs 1 : line graph, generation and pump load by key period dispatch --------------
# ---------------------------------------------------------------------------------------- /

dr_gen_pump_load_by_keyperiod <- function(comb.gen.pump.data, aggregation = "Total", facet_by_location = FALSE, 
                                          color_scheme = setNames(c("firebrick2", "steelblue3"), 
                                                                  c("Generation", "Pump Load")),
                                          x_axis_format = "%b %d\n%I %p", x_axis_angle = 0, ticks_by_day_hour = "day"){
  
  p <- ggplot(comb.gen.pump.data)+
    
    geom_line(aes(x = time, y = value, color = property, group = property), lwd = 1.25)+
    
    scale_color_manual(name = "Generation vs\nPump Load", 
                       values = color_scheme)+
    
    scale_y_continuous(name = "Generation & Pump Load (MWh)\n")+
    
    theme(axis.text.x = element_text(angle = x_axis_angle))
  
  if(ticks_by_day_hour == "day"){
    
    p <- p +     
      scale_x_datetime(name = "", labels = date_format(x_axis_format), expand = c(0, 0), 
                       breaks = unique(comb.gen.pump.data[lubridate::hour(time) == 0, time]))
    
  }
  if(ticks_by_day_hour == "hour"){
    
    p <- p +     
      scale_x_datetime(name = "", labels = date_format(x_axis_format), expand = c(0, 0), 
                       breaks = comb.gen.pump.data[, unique(time)])
    
  }
  
  p <- p + facet_wrap(~Location)
  
  
  
  return(p)
  
}

# ----------------------------- /
# Save Plots ----------------
# ----------------------------- /

# this function will take the data and lapply the above plotting function across the data by scenario, 
# aggregation and zone so far this will create all possible combinations, 
# if you want to limit this you will have to do so in the dat that you pass to this object



# ---------------------------------------------------------------------------------------- /
# DR graphs 2 : line graph, generation and pump load for year averaged by DOW and hour --------------
# ---------------------------------------------------------------------------------------- /

# DR_categories <- grep("^DR_", names(gen.cat.mapping), perl = T, value = T)

dr_gen_pump_load_by_dispatch <- function(total_data_DR, aggregation = "Total",
                                         color_scheme = setNames(c("firebrick2", "steelblue3"), 
                                                                 c("Generation", "Pump Load")),
                                         x_axis_format = "%b %d\n%I %p", x_axis_angle = 0){
  
  # --------------------------- /
  # Aggregate by time levels ---------------
  # --------------------------- /
  
  setorder(total_data_DR, time)
  
  total_data_DR[, Hour := format(time, format = "%H"), ]
  total_data_DR[, Weekday := format(time, format = "%a"), ]
  
  # create dummy POSIXct column variable for first week of year to match time and date
  # that way we can aggregate and still keep date format for plotting
  start.date <- as.POSIXct("2050-01-02 00:00:00", tz = "UTC")
  date.week <- start.date + seq(0, 3600*24*7-3600, 3600)
  date.week <- data.table(time = date.week, 
                          Weekday = format(date.week, format = "%a"), 
                          Hour = format(date.week, format = "%H"))
  
  total_data_DR <- merge(total_data_DR[, .SD, .SDcols = !"time"], 
                         date.week, by = c("Weekday", "Hour"), all.x = T)
  
  if(aggregation == "Total"){
    total_data_DR <- total_data_DR[, .(value = mean(value)), by = .(property, scenario, time)]
  }
  if(aggregation %in% c("Zonal", "Regional")){
    total_data_DR <-  total_data_DR[, .(value = mean(value)), by = .(property, scenario, Location, time)]
  }

  
  setorder(total_data_DR, time)
  
  # --------------------------- /
  # Create Plot Object ---------------
  # --------------------------- /
  
  p <- ggplot(total_data_DR)+
    geom_line(aes(x = time, y = value, color = property, group = property), lwd = 1.25)+
    
    theme(axis.text.x = element_text(angle = x_axis_angle, hjust = .5))+
    
    scale_x_datetime(name = "", labels = date_format(x_axis_format), expand = c(0, 0), 
                     breaks = total_data_DR[lubridate::hour(time) == 0, unique(time)]) +
    
    scale_color_manual(name = "Generation vs\nPump Load", 
                       values = color_scheme)+
    
    scale_y_continuous(name = "Generation & Pump Load (MWh)\n")
  
  
  if(aggregation %in% c("Zonal", "Regional")){
    p <- p + facet_wrap(~Location)
  }

  return(p)
  
}

# ---------------------------------------------------------------------------------------- /
# DR graphs 3 : plotly 3D objects, pump load and  generation by hour and day --------------
# ---------------------------------------------------------------------------------------- /

source("plotly_gen_pump_load_year.R")



