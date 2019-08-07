# ---------------------------------------------------------------------------------------------- /
# Global variables, sourcing ---------------------------
# ---------------------------------------------------------------------------------------------- /

save_dir <- paste0(fig.path.name,"pump")
dir.create(save_dir, showWarnings = FALSE)
dr.cat <- grep("^DR_", names(gen.cat.mapping), perl = T, value = T)

source("plot_functions_TB_ESF_2050_sum.R")
source("plotly_gen_pump_load_year_sum.R")

# For any of the 'focus_global' variables use NA to select all options
# Note that changing any of these will exclude data from all of the plots 
# except the key period dispatch plots. This is especially pernicious for the generation and pumped Load vs 
# the day of the week plot as it may not be obvious that data has been excluded

date_focus_global <- NA
                    #seq(as.POSIXct("2050-08-27 00:00:00", tz = "UTC"), as.POSIXct("2050-10-03 23:00:00", tz = "UTC"), by = "hours")
hour_focus_global <- NA
zone_focus_global <- NA
scen_focus_global <- c("NoFlex_RefElec_MIP_DR")

# This is for setting plotting options such as how dates are displayed and the colors
# currently these are all set the same for every plot but can be changed below by providing different inputs
color_scheme_global = setNames(c("firebrick2", "steelblue3"), 
                               c("Generation", "Pump Load"))
x_axis_format_global = "%b %d\n%I %p" 
x_axis_angle_global = 0
ticks_by_day_hour_global = "day"

x_axis_format_dayofweek <- "%a\n%I %p"
 
aggregation_global <- c("Zonal", "Total")
int_gen_data_global <- interval.generation
int_pump_data_global <- interval.pump.load
keep_categories_global <- FALSE

# ---------------------------------------------------------------------------------------------- /
# Find Key Periods (or make new ones) ---------------------------
# ---------------------------------------------------------------------------------------------- /

# For key periods !!!!!!! need a way to make own key periods ...

key.period.sequences <- lapply(1:length(period.names), function(index, p.name = period.names, 
                                                                st.end = start.end.times){
  
  time.seq <- seq(st.end[index,start], st.end[index,end], by = "hour")
  return(time.seq)
  
})

names(key.period.sequences) <- period.names

# To add key period run below
# key.period.sequences[["new period name"]] <- 
#   seq(as.POSIXct("date", format = "", tz = "UTC"), as.POSIXct("date", format = "", tz = "UTC"), by = "hour")


# ---------------------------------------------------------------------------------------------- /
# Get both zonal and total data for specified periods - %% NOT FOR KEY DISPATCH %% ---------------------------
# ---------------------------------------------------------------------------------------------- /

# %% NOT FOR KEY DISPATCH %%
  
DR_data_zonal <- dr_gen_pump_load_data_comp(DR_categories = dr.cat, generation_data = int_gen_data_global, 
                                      pumped_load_data = int_pump_data_global, 
                                      keep_category = keep_categories_global, aggregation = "Zonal", 
                                      region_zone_generator_map = region.zone.mapping,
                                      zone_focus = zone_focus_global,
                                      date_focus = date_focus_global,
                                      hour_focus = hour_focus_global,
                                      scen_focus = scen_focus_global)

DR_data_total <- dr_gen_pump_load_data_comp(DR_categories = dr.cat, generation_data = int_gen_data_global, 
                                            pumped_load_data = int_pump_data_global, 
                                            keep_category = keep_categories_global, aggregation = "Total", 
                                            region_zone_generator_map = region.zone.mapping,
                                            zone_focus = zone_focus_global,
                                            date_focus = date_focus_global,
                                            hour_focus = hour_focus_global,
                                            scen_focus = scen_focus_global)

# ---------------------------------------------------------------------------------------------- /
# Get both zonal and total data for specified periods - %% FOR KEY DISPATCH %% ---------------------------
# ---------------------------------------------------------------------------------------------- /

# %% FOR KEY DISPATCH %%

DR_data_zonal_key <- lapply(key.period.sequences, 
                            function(t.seq, dr.cat. = dr.cat, gen.data. = int_gen_data_global,
                                     pl.data. = int_pump_data_global, kc. = keep_categories_global, agg. = "Zonal",
                                     reg_z_m. = region.zone.mapping, zf. = zone_focus_global, sf. = scen_focus_global){
                              
                              data. <- dr_gen_pump_load_data_comp(DR_categories = dr.cat., 
                                                                  generation_data = gen.data., 
                                                         pumped_load_data = pl.data., 
                                                         keep_category = kc., aggregation = agg., 
                                                         region_zone_generator_map = reg_z_m.,
                                                         zone_focus = zf.,
                                                         date_focus = NA,
                                                         hour_focus = NA,
                                                         scen_focus = sf.)
                              
                              data.focus <- data.[time %in% t.seq,,]
                              
                              if(length(data.[, unique(scenario)]) > length(data.focus[, unique(scenario)])){
                                warning("Some scenarios lost when focusing in on key period")
                              }
                              
                              return(data.focus)
                              
                            })


names(DR_data_zonal_key) <- names(key.period.sequences)

DR_data_total_key <- lapply(key.period.sequences, 
                            function(t.seq, dr.cat. = dr.cat, gen.data. = int_gen_data_global,
                                     pl.data. = int_pump_data_global, kc. = keep_categories_global, agg. = "Total",
                                     reg_z_m. = region.zone.mapping, zf. = zone_focus_global,
                                     sf. = scen_focus_global){
                              
                              data. <- dr_gen_pump_load_data_comp(DR_categories = dr.cat., 
                                                                  generation_data = gen.data., 
                                                                  pumped_load_data = pl.data., 
                                                                  keep_category = kc., aggregation = agg., 
                                                                  region_zone_generator_map = reg_z_m.,
                                                                  zone_focus = zf.,
                                                                  date_focus = NA,
                                                                  hour_focus = NA,
                                                                  scen_focus = sf.)
                              
                              data.focus <- data.[time %in% t.seq,,]
                              
                              if(length(data.[, unique(scenario)]) > length(data.focus[, unique(scenario)])){
                                warning("Some scenarios lost when focusing in on key period")
                              }
                              
                              return(data.focus)
                              
                            })


names(DR_data_total_key) <- names(key.period.sequences)


# ---------------------------------------------------------------------------------------------- /
# Line graph, generation and pump load by key period dispatch ---------------------------
# ---------------------------------------------------------------------------------------------- /

lapply(names(key.period.sequences), function(p.name, #t.seqs = key.period.sequences,
                                             dr_data_t = DR_data_total_key,
                                             dr_data_z = DR_data_zonal_key, sd = save_dir,
                                             cs = color_scheme_global, xaf = x_axis_format_global,
                                             xaa = x_axis_angle_global, tbdh = ticks_by_day_hour_global){
  
  
  dr_data_t_key_focus <- dr_data_t[[p.name]]
  
  
  # Total
  for(scen in dr_data_t_key_focus[, unique(scenario)]){
    p.t <- dr_gen_pump_load_by_keyperiod(dr_data_t_key_focus[scenario == scen], 
                                         aggregation = "Total", 
                                         facet_by_location = TRUE, color_scheme = cs, 
                                         x_axis_format = xaf, 
                                         x_axis_angle = xaa, 
                                         ticks_by_day_hour = tbdh)
    
    ggsave(p.t, 
           filename = file.path(sd, paste0("Total_Generation_Pumped_Load_Plot_Key_Period_", 
                                            p.name, "_Scenario_", scen, ".png")), 
           device = "png", width = 7, height = 3, units = "in", dpi = 150)  
    
    }
  
  dr_data_z_key_focus <- dr_data_z[[p.name]]
  
  # Zonal, combined and faceted
  for(scen in dr_data_z_key_focus[, unique(scenario)]){
    p.zc <- dr_gen_pump_load_by_keyperiod(dr_data_z_key_focus[scenario == scen], 
                                         aggregation = "Zonal", 
                                         facet_by_location = TRUE, color_scheme = cs, 
                                         x_axis_format = xaf, 
                                         x_axis_angle = xaa, 
                                         ticks_by_day_hour = tbdh)
    
    ggsave(p.zc, 
           filename = file.path(sd, paste0("Zonal_Combined_Generation_Pumped_Load_Plot_Key_Period_", 
                                           p.name, "_Scenario_", scen, ".png")), 
           device = "png", width = 10, height = 8, units = "in", dpi = 150)  
    
  }
  
  # Zonal, separated
  for(scen in dr_data_z_key_focus[, unique(scenario)]){
    
    for(zone in dr_data_z_key_focus[, unique(Location)]){
      
      p.zs <- dr_gen_pump_load_by_keyperiod(dr_data_z_key_focus[scenario == scen & Location == zone], 
                                            aggregation = "Total", 
                                            facet_by_location = TRUE, color_scheme = cs, 
                                            x_axis_format = xaf, 
                                            x_axis_angle = xaa, 
                                            ticks_by_day_hour = tbdh)
      
      ggsave(p.zs, 
             filename = file.path(sd, paste0("Zone_", zone, "_Generation_Pumped_Load_Plot_Key_Period_", 
                                             p.name, "_Scenario_", scen, ".png")), 
             device = "png", width = 7, height = 3, units = "in", dpi = 150)  
      
    }
    
    
    
  }
  
  
})


# ---------------------------------------------------------------------------------------------- /
# Line graph, generation and pump load for year averaged by DOW and hour ---------------------------
# ---------------------------------------------------------------------------------------------- /

lapply(c("Total", "Zonal"), function(agg.style, #t.seqs = key.period.sequences,
                                     dr_data_t = DR_data_total,
                                     dr_data_z = DR_data_zonal, sd = save_dir,
                                     cs = color_scheme_global, xaf = x_axis_format_dayofweek,
                                     xaa = x_axis_angle_global, tbdh = ticks_by_day_hour_global){
  # First Total
  if(agg.style == "Total"){
    
    for(scen in dr_data_t[, unique(scenario)]){
    
    p.t <- dr_gen_pump_load_by_dispatch(total_data_DR = dr_data_t[scenario == scen], 
                                      aggregation = agg.style, color_scheme = cs, x_axis_format = xaf, 
                                      x_axis_angle = xaa)
    
    
    
    ggsave(p.t, width = 7, height = 3, units = "in", dpi = 150,device = "png", 
           filename = file.path(sd, paste0("Total_Average_Generation_Pumped_Load_DoW_Hour", 
                                            "_Scenario_", scen, ".png")))  
    
    }
    
  }
  
  # Then Zonal, first combined and then individual
  if(agg.style == "Zonal"){
    
    # combined
    for(scen in dr_data_z[, unique(scenario)]){
      
      p.zc <- dr_gen_pump_load_by_dispatch(total_data_DR = dr_data_z[scenario == scen], 
                                          aggregation = agg.style, color_scheme = cs, x_axis_format = xaf, 
                                          x_axis_angle = xaa)
      
      
      
      ggsave(p.zc, width = 10, height = 8, units = "in", dpi = 150, device = "png", 
             filename = file.path(sd, paste0("Zonal_Combined_Average_Generation_Pumped_Load_DoW_Hour", 
                                              "_Scenario_", scen, ".png")))  
      
    }
    
    # separate
    for(scen in dr_data_z[, unique(scenario)]){
      
      for(zone in dr_data_z[, unique(Location)]){
      
      p.z <- dr_gen_pump_load_by_dispatch(total_data_DR = dr_data_z[scenario == scen & Location == zone], 
                                           aggregation = "Total", color_scheme = cs, x_axis_format = xaf, 
                                           x_axis_angle = xaa)
      
            ggsave(p.zc, width = 7, height = 3, units = "in", dpi = 150,device = "png", 
             filename =  file.path(sd, paste0("Zone_", zone, "_Average_Generation_Pumped_Load_DOW_Hour_", 
                                              p.name, "_Scenario_", scen, ".png")))  
      
      }
      
    }
    
  }
    
})

# ---------------------------------------------------------------------------------------------- /
# Plotly 3D objects, pump load and  generation by hour and day --------------
# ---------------------------------------------------------------------------------------------- /

# In this function all of the various lapplying is done internally

# Zonal
plotly.z <- plotly_3d_gen_pumpload(DR_data = DR_data_zonal,
                                   date_focus = NA, hour_focus = NA, scen_focus = NA, zone_focus = NA,
                                   agg.style = "Zonal", sd... = save_dir)
# Total
plotly.t <- plotly_3d_gen_pumpload(DR_data = DR_data_total,
                                   date_focus = NA, hour_focus = NA, scen_focus = NA, zone_focus = NA,
                                   agg.style = "Total", sd... = save_dir)











