#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|
myPath = c("C:/Program Files/R/R-3.4.4/library", "D:/ella/R/win-library/3.4")
.libPaths(myPath)
data.dir = '//nrelqnap02/reeds/FY18-EFS/plexos/figure_data'
magma.dir        = 'D:/ella/MAGMA/'
input.csv        = "D:/ella/MAGMA/efs_inputs/magma_input_dsf_separate.csv"
#input.csv        = file.path(dirname(sys.frame(1)$ofile),'magma_input.csv')
db.loc           = c(
                     '//nrelqnap02/reeds/FY18-EFS/plexos/db_files/high_hiflex')
output.dir       = '//nrelqnap02/reeds/FY18-EFS/plexos/db_plots'
fig.path.name    = '//nrelqnap02/reeds/FY18-EFS/plexos/db_plots'
output.name      = 'high.html'
db.day.ahead.loc = NULL
query.data       = TRUE
save.data        = TRUE
load.data        = NULL #'//nrelqnap02/reeds/FY18-EFS/plexos/NoFlex_HighElec/plexos_export/0731_trc/reports_2/NoFlex_HighElec_MIP.RData'
save.data.name   = 'high.RData'
reassign.zones   = FALSE
use.gen.type.csv = FALSE
gen.type.csv.loc = NULL
scenario.names   = c('High-HiFlex')
wrap.cols = 6
library(data.table)
all_scenario = as.data.table(read.csv('D:/ella/MAGMA/efs_inputs/efs_scenario_color.csv'))
rto_table = as.data.table(read.csv('D:/ella/MAGMA/efs_inputs/rto.csv'))
dir.create(output.dir, showWarnings = FALSE)
dir.create(file.path(output.dir,paste(scenario.names, collapse = '_')), showWarnings = FALSE)
efs_color = c(#"DSF Ind"='#D9531E', "DSF Comm"='#5E9732',
  #"DSF Res"='#F6A01A', "DSF Tran"='#007BBD', 
  #"DSF Other"='#D49E2E', "Other"='#D49E2E',
  #"Industry"='#D9531E', "Commercial"='#5E9732',
  #"Residential"='#F6A01A',
  "Other" ='#F6A01A',
  "Transportation"='#007BBD')

#------------------------------------------------------------------------------|
# Run code to create HTML
#------------------------------------------------------------------------------|
setwd(magma.dir)
library(data.table)
library(dplyr)
scen_color <- function (selected_scen = scenario.names){
  as.character(droplevels(subset(all_scenario, all_scenario$scenario %in% selected_scen)[,color]))
} 
agg_plexos <- function(plexos_table){
  if ("category" %in% colnames(plexos_table)){
    plexos_table$Type <- gen.cat.plexos$Type[match(plexos_table$category, gen.cat.plexos$category)]
    df = plexos_table[,!c("category"), with=FALSE]
    if ("name" %in% colnames(plexos_table)){
      df = df[,!c("name"), with=FALSE]
    }
    df = df[,(value = lapply(.SD, sum)), by=eval(colnames(df)[!colnames(df) %in% c('value')]), .SDcols = 'value']
    return (df)
  } else if ("name" %in% colnames(plexos_table)){
    plexos_table$Type <- gen.cat.plexos$Type[match(plexos_table$name, gen.cat.plexos$name)]
    df = plexos_table[,!c("name"), with=FALSE]
    df = df[,(value = lapply(.SD, sum)), by=eval(colnames(df)[!colnames(df) %in% c('value')]), .SDcols = 'value']
    return (df)
  } else print ("original plexos table does not contain the right category or name to match mapping")
}

# Sourcing the setup file and required functions
source(file.path('query_functions.R'))
source(file.path('plot_functions.R'))

# Read CSV file with all inputs
inputs = read.csv(file.path(input.csv))
inputs[inputs==""]=NA

# Either query data from database or load existing data
source(file.path('setup_plexosAnalysis.R'))

if (query.data){
  source(file.path('setup_dataQueries.R'), echo=TRUE)
} else{
  load(load.data)
}

# unserved energy total and top
tot_ue <- interval.zone.ue[,.(value = sum(value)), by = .(scenario)]
write.csv(tot_ue, file = file.path(output.dir,paste(scenario.names, collapse = '_'),'total_ue.csv'))
top_ue = setorderv(interval.region.ue, cols="value", order=-1)
top_ue = top_ue[0:300,]
write.csv(top_ue, file = file.path(output.dir,paste(scenario.names, collapse = '_'),'top100_ue.csv'))

write.csv(total.reserve.provision, file = file.path(output.dir,paste(scenario.names, collapse = '_'),'total_res_provision.csv'))
write.csv(total.reserve.shortage, file = file.path(output.dir,paste(scenario.names, collapse = '_'),'total_res_shortage.csv'))
write.csv(z.load, file = file.path(output.dir,paste(scenario.names, collapse = '_'),'z_load.csv'))

# aggregate interval data
agg_interval_generation = agg_plexos(interval.generation)
agg_interval_avail_cap = agg_plexos(interval.avail.cap)
agg_interval_gen_reserve_provision = agg_plexos(interval.gen.reserve.provision)

res.short = total.reserve.shortage[,.(value = sum(value)), by = .(scenario)][,property:= "reserve shortage"]
res.prov = total.reserve.provision[,.(value = sum(value)), by = .(scenario)][,property:= "reserve provision"]
tot.load = z.load[,.(value = sum(value)), by = .(scenario)][,property:= "load"]
tot.pumpload = yr.pumpload[,.(value = sum(GWh)), by = .(scenario)][,property:= "pump load"]
sum_prop = rbind(res.short,res.prov, tot.load, tot.pumpload)
sum_prop = dcast(sum_prop, scenario ~ property)
sum_prop$nopump = sum_prop$load - sum_prop$`pump load`
sum_prop$res_prov = 1 - (sum_prop$`reserve shortage`/(sum_prop$`reserve provision` + sum_prop$`reserve shortage`))
write.csv(sum_prop, file = file.path(output.dir,paste(scenario.names, collapse = '_'),'sum_prop.csv'))



##--------------------------------------------##
## generation and pump load stack
##--------------------------------------------##
total.avail.cap      = tryCatch( total_avail_cap(db), error = function(cond) { return('ERROR') } ) 
if (typeof(total.avail.cap)=='character') { message('\nMissing total generator available capacity data from solution .db file.')}
total.generation     = tryCatch( total_generation(db), error = function(cond) { return('ERROR') } ) 
if (typeof(total.generation)=='character') { message('\nMissing total generator generation data from solution .db file.')}

yr.gen = tryCatch( gen_by_type(total.generation, total.avail.cap), 
                   error = function(cond) { return('ERROR: gen_by_type function for total generation and avail cap not returning correct results.') } )
yr.pumpload= tryCatch( pumpload_by_type(total.pumpload), 
                       error = function(cond) { return('ERROR: pumpload_by_type function for pump load not returning correct results.') } )

#yr.pumpload$Type = paste(yr.pumpload$Type, "Pump_Load", sep="_")
yr.pumpload = yr.pumpload[Type %like% 'DSF']
yr.pumpload$GWh = yr.pumpload$GWh * (-1)
curt.by.type = tryCatch( curt_by_type(total.generation, total.avail.cap),
                         error = function(cond) { return('ERROR: curt_by_type function not returning correct results.') } )

yr.gen = rbind(yr.gen, curt.by.type[,.(Type='Curtailment', GWh=sum(Curtailment)), by=scenario])
#yr.gen=rbind(yr.gen, yr.pumpload)
#yr.gen <- yr.gen[ GWh != 0]
yr.gen$scenario = str_wrap(yr.gen$scenario, width = 15)

# remove pump load from total load
r_pumpload = yr.pumpload[,.(value=sum(GWh)), by=scenario]
r.load = r.load[, .(value = sum(value)), by=scenario]
r.load = rbind(r.load, r_pumpload)
r.load$scenario = str_wrap(r.load$scenario, width = 15)

# data checkout
save(yr.gen, r.load, file = file.path(output.dir, paste(scenario.names, collapse = '_'), 'yr_gen_and_load.RData'))

plot.data = gen_stack_plot(yr.gen, load.data = r.load)
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'generation_stack_w_pump.png'),
       plot.data[[1]] + scale_y_continuous(breaks=plot.data[[2]], limits=c(min(yr.pumpload$GWh)*0.002, (max(plot.data[[2]])*1.02)), 
                                          expand=c(0,0), labels= scales::comma)+
        theme(plot.margin =unit(c(0.5,0.5,0.5,0.5), "cm"),
               aspect.ratio = 4/length(unique(yr.gen$scenario)),
               axis.text.x = element_text(angle = -30, hjust = 0),
               text = element_text(face='bold')),
       width = 12, height = 6,dpi = 300,device = 'png')
write.csv(yr.gen, file=file.path(output.dir,paste(scenario.names, collapse = '_'), 'generation_stack_w_pump.csv'))

## --------------------------------------
# total production cost
## --------------------------------------
cost.table = tryCatch( costs(total.emissions.cost, total.fuel.cost, total.ss.cost, total.vom.cost), 
                       error = function(cond) { return('ERROR: costs function not returning correct results.') })
write.csv(cost.table, file=file.path(output.dir, paste(scenario.names, collapse = '_'), 'total_cost.csv'))
agg_ss_cost = agg_plexos(total.ss.cost)
agg_ss_cost$value = agg_ss_cost$value * (-1)
agg_vom_cost = agg_plexos(total.vom.cost)
agg_vom_cost$value = agg_vom_cost$value * (-1)
agg_fuel_cost = agg_plexos(total.fuel.cost)
agg_fuel_cost$value = agg_fuel_cost$value* (-1)
write.csv(agg_ss_cost, file=file.path(output.dir,paste(scenario.names, collapse = '_'), 'startshut_cost.csv'))
write.csv(agg_vom_cost, file=file.path(output.dir,paste(scenario.names, collapse = '_'), 'vom_cost.csv'))
write.csv(agg_fuel_cost, file=file.path(output.dir, paste(scenario.names, collapse = '_'),'fuel_cost.csv'))

# calculate revenue
r.z.revenue = tryCatch( revenue_calculator(interval.generation, interval.pump.load, interval.region.price,
                                           interval.gen.reserve.provision,interval.reserve.price), 
                        error = function(cond) { return('ERROR')})

# If there is a problem with the query return an error.
if ( typeof(r.z.revenue)=='character' ) { 
  print('ERROR: revenue_calculator function not returning correct results.')
} else {
  total.revenue = r.z.revenue[, .(revenue=sum(revenue)), by=.(scenario,Type,Revenue_Type)]
  total.revenue[, Revenue_Type:=factor(Revenue_Type,levels=c('Generation','Reserves','Charging'))]
  net.revenue = total.revenue[, .(revenue=sum(revenue), Revenue_Type='Net Revenue'), by=.(scenario,Type)]}
  
write.csv(total.revenue, file=file.path(output.dir, paste(scenario.names, collapse = '_'),'total_revenue.csv'))

# total capacity
write.csv(total.installed.cap, file=file.path(output.dir, paste(scenario.names, collapse = '_'),'total_cap.csv'))

## -----------------------------------
# Full run emissions production
## -----------------------------------
emission_production = function(database) {
  if ("Production" %in% properties[is_summary==1 & collection=="Emission",property]){
    total.emission.production = data.table(query_year(database, 'Emission', 'Production', columns = c('category', 'name')))
  } else if ("Production" %in% properties[is_summary==0 & collection=="Emission", property]){
    total.emission.production = data.table(query_interval(database, 'Emission', 'Production', columns = c('category', 'name')))
    total.emission.production = total.emission.production[, .(value=sum(value)/1000), by=.(scenario, property, name, category)]
  }
  return(total.emission.production[, .(value=sum(value)), by=.(scenario, property, name, category)])
}
total.emission.production=emission_production(db)
total.co2 = total.emission.production[name=='CO2']
write.csv(total.co2, file=file.path(output.dir, paste(scenario.names, collapse = '_'),'total_co2.csv'))
# data checkout
save(total.co2, file = file.path(output.dir, paste(scenario.names, collapse = '_'), 'total.co2.RData'))


total_emission_plot <- ggplot(data=total.co2, aes(x = scenario, y = (value/1000000), fill = scenario)) +
  scale_fill_manual(values = scen_color())+
  #scale_colour_brewer(palette = "Set1")+
  geom_col()+
  xlab("")+
  ylab("Emission (Million Tonnes)")+
  guides(fill=guide_legend(title="Scenario"))+
  theme(text=element_text(size=20),
        axis.text.x = element_blank())
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'co2_production.png'), plot = total_emission_plot, width = 10, height = 6,dpi = 300)

# plot emission difference
emission.diff = total.emission.production[, .(scenario, value = value - value[as.character(scenario)==ref.scenario]), by=.(name)]
co2.diff=total.co2[, .(scenario, value = value - value[as.character(scenario)==ref.scenario]), by=.(name)]
emission_diff_plot <- ggplot(data=co2.diff, aes(x = scenario, y = (value/1000000), fill = name)) +
  geom_col()+
  xlab("")+
  ylab("Emission Difference (Million Tonnes)")+
  guides(fill=guide_legend(title="Emission"))+
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(text=element_text(size=20),
        axis.text.x = element_text(angle = 30, hjust=1))
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'CO2_diff.png'), plot = emission_diff_plot, width = 10, height = 8,dpi = 300)


# net load calculation
int.region.load=copy(interval.region.load)
agg_load = int.region.load[,.(total_load=sum(value)), by=list(scenario, time)]
agg_vre = agg_interval_generation[agg_interval_generation$Type %in% c('Wind','Solar'),]
agg_vre = agg_vre[,.(vre_gen=sum(value)), by = list(scenario,time)]
agg_netload = merge(agg_load, agg_vre, by = c("scenario","time"))
agg_netload$net_load = agg_netload$total_load - agg_netload$vre_gen

# # plot High vs. High-HiRE net load graph
# high_netload = agg_netload[scenario != 'Ref-NoFlex'][,.(scenario, time, net_load)]
# high_netload$date = as.Date(high_netload$time)
# high_netload_day = high_netload[,.(GW = mean(net_load)/1000), by=.(scenario, date)]
# high_netload_day$date = as.POSIXct.Date(high_netload_day$date)
# high_netload_plot_day = ggplot(high_netload_day,aes(x = date, y = GW))+
#   geom_line(aes(color = scenario), size = 1) +
#   scale_x_datetime(breaks = date_breaks(width = "2 month"), labels = date_format("%b"), 
#                  expand = c(0, 0), timezone='UTC')+
#   scale_color_manual(values = scen_color(c('High-NoFlex','High-HiRE-NoFlex')))+
#   labs (x = "", y = "Net Load (GW)", title = "Mean Net Load")+
#   theme(
#               legend.key =       element_rect(color = "grey80", size = 0.5),
#               legend.key.size =  grid::unit(1, "lines"),
#               legend.text =      element_text(size=text.plot*1.3,face = 'bold'),
#               legend.title =     element_text(size=text.plot*1.3,face = 'bold'),
#               axis.text =        element_text(size=text.plot*1.3,face = 'bold'),
#               axis.title =       element_text(size=text.plot*1.4, face='bold'),
#               axis.title.x =     element_text(vjust=-0.3),
#               plot.title =       element_text(size = text.plot*1.4),
#               plot.margin =      unit(c(1,1,1,1),"mm"),
#               aspect.ratio =     0.4
# )
# ggsave(filename = paste0(output.dir,scenario.names[1],'annual_net_load_day.png'), 
#        plot = high_netload_plot_day, width = 10, height = 5,dpi = 300)
# 
# high_netload$net_load = high_netload$net_load*0.001 ## convert to GW
# high_netload_plot = ggplot(high_netload,aes(x = time, y = net_load))+
#   geom_line(aes(color = scenario), linetype = 'solid', size = 0.2) +
#   scale_x_datetime(breaks = date_breaks(width = "2 month"), labels = date_format("%b"), 
#                    expand = c(0, 0), timezone='UTC')+
#   scale_color_manual(values = scen_color(c('High-NoFlex','High-HiRE-NoFlex')))+
#   labs (x = "", y = "Net Load (GW)", title = "Mean Net Load")+
#   theme(
#     legend.key =       element_rect(color = "grey80", size = 0.5),
#     legend.key.width  =  unit(1, "cm"),
#     legend.text =      element_text(size=text.plot*1.3,face = 'bold'),
#     legend.title =     element_text(size=text.plot*1.3,face = 'bold'),
#     axis.text =        element_text(size=text.plot*1.3,face = 'bold'),
#     axis.title =       element_text(size=text.plot*1.4, face='bold'),
#     axis.title.x =     element_text(vjust=-0.3),
#     plot.title =       element_text(size = text.plot*1.4),
#     plot.margin =      unit(c(1,1,1,1),"mm"),
#     aspect.ratio =     0.35
#   ) +
#   guides(color=guide_legend(override.aes = list(size = 6)))
# ggsave(filename = paste0(output.dir,scenario.names[1],'annual_net_load_internal.png'), 
#        plot = high_netload_plot, width = 10, height = 6,dpi = 300)


# plot national price duration curve
int.region.price = copy(interval.region.price)
colnames(int.region.price) <- c('scenario', 'property','name','time','price')
int.region.price <- subset(int.region.price, select = -c(property))
int.region.load = copy(interval.region.load)
colnames(int.region.load) <- c('scenario','property','name','time','load')
int.region.load <- subset(int.region.load, select = -c(property))
sum.price = merge (int.region.price, int.region.load, by=c('scenario','name','time'))
sum.price$sum = sum.price$price * sum.price$load
sum.price=sum.price[,total_load_cost:=sum(sum), by=list(scenario,time)]
sum.price = sum.price[,national_load:=sum(load), by=list(scenario,time)]
sum.price = subset(sum.price, select=-c(price, sum))
sum.price$national = sum.price$total_load_cost/sum.price$national_load
sum.price=subset(sum.price, select=c(scenario,time,national))
colnames(sum.price) <- c('scenario','time','value')
sum.price=unique(sum.price)

# data checkpoint
save(sum.price, file = file.path(output.dir, paste(scenario.names, collapse = '_'), 'sum_price.RData'))
write.csv(sum.price, file.path(data.dir, 'figure26.csv'))

# calculate for netload price correlation graph
netload_price = merge(sum.price, agg_netload[,c("scenario","time","net_load")], by=c("scenario", "time"))
#

price_summary = sum.price[,.(Average = mean(value), SD = sd(value), RSD = sd(value)/mean(value)), by =.(scenario)]
write.csv(price_summary, file=file.path(output.dir, paste(scenario.names, collapse = '_'),'price_summary.csv'))

rownames(sum.price) <- seq(length=nrow(sum.price))
p1 = price_duration_curve(sum.price, filters = c('scenario'), color='scenario') +
  scale_colour_manual(values = scen_color())+
  #scale_colour_brewer(palette = "Set1")+
  theme(plot.margin = unit(c(5,5,5,5), "mm"))
p1
ggsave(p1, filename = file.path(output.dir, paste(scenario.names, collapse = '_'), 'national_price_duration_curve.png'), device = "png",
       width=9, height=4, units="in", dpi=330)
p2 = p1 + coord_cartesian(ylim=c(0,60))
ggsave(p2, filename = file.path(output.dir, paste(scenario.names, collapse = '_'), 'national_price_duration_curve_magnified.png'), device = "png",
       width=9, height=4, units="in", dpi=330)

# ## plot price correlation with net load, national
# # netload_price$value[netload_price$value > 200] <- NA
# netload_price$net_load = netload_price$net_load/1000
# netload_price_plot = ggplot(netload_price, aes(x=net_load, y=value, color=scenario))+
#     geom_point(size=0.5)+
#     ylim(-10, 120)+
#     scale_colour_manual(values = scen_color())+
#     labs(x="Net Load (GW)", y = "Average National Price ($/MWh)")+
#     geom_smooth(span = 0.7, se = FALSE)+
#     theme_bw(base_size = 16)
# ggsave(netload_price_plot, filename = file.path(output.dir, paste(scenario.names, collapse = '_'), 'national_price_netload.png'), device = "png",
#        width=8, height=5, units="in", dpi=330)
   


### plot price duration curves in one graph

# plot_pdc = interval.region.price[!Region %in% ignore.regions & property == 'Price', ]
# plot_pdc[, interval := rank(-value,ties.method="random"), by=c('scenario','Region')]
# plot_pdc=subset(plot_pdc, select=-c(property,time))
# 
# p.1 = ggplot(plot_pdc)+
#   geom_line(aes_string(x='interval', y='value', color='scenario'), size=0.8)+  
#   scale_colour_manual(values = scen_color())+
#   #scale_colour_brewer(palette = "Set1")+
#   labs(y=paste0("Price (", currency, "/MWh)"), x='Hours of Year')+
#   theme( legend.key =       element_rect(color = "grey80", size = 0.4),
#          legend.key.size =  grid::unit(0.9, "lines"),
#          legend.text =      element_text(size=text.plot/1.1,face = 'bold'),
#          axis.text =        element_text(size=text.plot/1.2,face = 'bold'),
#          axis.title =       element_text(size=text.plot, face='bold'),
#          axis.title.x =     element_text(vjust=-0.3),
#          strip.text =       element_text(size = text.plot/1.1,face = 'bold'),
#          panel.grid.major = element_line(colour = "grey85"),
#          panel.grid.minor = element_line(colour = "grey93"),
#          panel.spacing =     unit(0.5, "lines"),
#          plot.margin = unit(c(0,0,0,0),"mm")
#          #,aspect.ratio =     .65
#   )
# 
# p.1 = p.1 + theme(plot.margin = unit(c(5,5,5,5), "mm"))
# ggsave(p.1, filename = paste0(output.dir,'all_price_duration_curve.png'), device = "png",
#        width=9, height=4, units="in", dpi=330)
# p.2 = p.1 + coord_cartesian(ylim=c(0,60))
# ggsave(p.2, filename = paste0(output.dir,'all_price_duration_curve_magnified.png'), device = "png",
#        width=9, height=4, units="in", dpi=330)


## plog average price by node and their standard deviation
trim = as.data.table(int.region.price)
trim$price[trim$price > 400] <- 400
#trim$price[trim$price < 0] <- 0
no_na = sum(is.na(trim$price))
lmp = trim[,.(Average=mean(price, na.rm = TRUE), `Coefficient of Variation` = (sd(price, na.rm=TRUE)/mean(price, na.rm=TRUE))), by = list(name,scenario)]
lmp = melt(lmp, id.vars = c("scenario",'name'))
lmp = lmp[,rank := rank(-value,ties.method="random"), by=list(scenario, variable)]
lmp_plot = ggplot(lmp, aes(x=as.numeric(as.character(rank)), y=value, color=scenario)) +
  geom_point(size=1.5)+
  scale_colour_manual(values = scen_color()) +
  facet_wrap(. ~ variable, scales="free_y") +
  xlab("ReEDS Balancing Area") +
  ylab("$/MWh")+
  theme_bw(base_size = 20)
ggsave(lmp_plot, filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'price_by_ba.png'), device = "png",
       width=10, height=5, units="in", dpi=330)

# ## output mean price by BA for python mapping
# mean_price_ba = int.region.price[,.(val = mean(price)), by=.(scenario, name)]
# high_p = mean_price_ba[scenario == 'High-NoFlex']
# write.csv(high_p, file = paste0(output.dir,"High-NoFlex_price_ba.csv"))
# hire_p = mean_price_ba[scenario == 'High-HiRE-NoFlex']
# write.csv(hire_p, file = paste0(output.dir,"High-HiRE-NoFlex_price_ba.csv"))

##---------------------------------------------##
# plot total reserve by generation
##---------------------------------------------##
library(stringr)
yr.res.scen = tryCatch( annual_reserves_provision(total.gen.res), error = function(cond) { return('ERROR') })
#yr.res.scen.diff = yr.res.scen[,.(scenario, GWh=GWh-GWh[scenario==ref.scenario]),by=.(Type,Reserve)]
total.res = copy(yr.res.scen)
total.res$res_type = as.data.frame(str_split_fixed(total.res$Reserve, "_", 2))[,1]
total.res=total.res[,reserve_gwh:=sum(GWh), by=list(scenario,Type, res_type)]
total.res=unique(subset(total.res, select=-c(Reserve, GWh)))
colnames(total.res) = c('scenario','Type','Reserve','GWh')

## aggregate DSF types if exist
total.res[Type %like% 'DSF', Type := 'DSF']

# data checkout
save(total.res, file = file.path(output.dir, paste(scenario.names, collapse = '_'), 'total.res.RData'))

plot.total.res = gen_stack_plot(total.res, filters = c('Reserve','scenario'))
res.p = plot.total.res[[1]] + facet_wrap(~Reserve,ncol=wrap.cols,scales='free') + 
  labs(y = "Provision (TW-h)")+
  theme(aspect.ratio = 0.7, 
        axis.text.x = element_text(angle = -30, hjust = 0))
ggsave(res.p, filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'reserve_by_generator.png'), device = "png",
       width=10, height=5, units="in", dpi=330)

##-----------------------------------------------------------------------##
## ---- plot DSF providing reserve and energy serves, month of year------##
##-----------------------------------------------------------------------##
interval.gen.reserve.provision = interval_gen_reserve_provision(db)
dsf.reserve = interval.gen.reserve.provision[Type %like% "DSF"][,.(GW=sum(value)*0.001), 
                                                                by = .(scenario, parent, time, Type)]
dsf.reserve = dsf.reserve[,c('Reserve', "RTO"):= tstrsplit(parent,"_",fixed=TRUE)][,.(GW=sum(GW)), by = .(scenario, time, Type, Reserve)]

interval.generation   = tryCatch( interval_gen(db), error = function(cond) { return('ERROR') } )
dsf.generation = interval.generation[, Type:=gen.type.mapping[as.character(name)] ][,.(scenario,Type,value,time)]
dsf.generation = dsf.generation[Type %like% "DSF"][,.(GW=sum(value)*0.001), 
                                                   by = .(scenario, time, Type)]
dsf.generation$Reserve = "Energy"
dsf.int.prov = rbind(dsf.generation, dsf.reserve)

## save(dsf.int.prov, file = paste0(output.dir,'dsf_reserve_gen.RData'))

colnames(dsf.int.prov)[colnames(dsf.int.prov)=="Reserve"] <- "Service"
dsf.int.prov$date = as.Date(dsf.int.prov$time)
dsf.int.prov$month = as.numeric((format(dsf.int.prov$date, format = "%m")))
dsf.int.prov$day = as.numeric((format(dsf.int.prov$date, format = "%d")))

mean_daily_dsf_prov = dsf.int.prov[,.(GW= mean(GW)), by=.(scenario, Service, month)]
mean_daily_dsf_prov$Service = factor(mean_daily_dsf_prov$Service, levels = c("Flexibility", "Contingency", "Energy"))
mean_daily_dsf_prov_ref = mean_daily_dsf_prov[scenario %like% 'Ref']
mean_daily_dsf_prov_high = mean_daily_dsf_prov[scenario %like% 'High']
mean_daily_dsf_plot = function(mean_daily_dsf_prov) {
  ggplot(mean_daily_dsf_prov, aes(x= month, y=GW))+
  geom_area(stat = "identity", aes(color=Service, fill=Service), position="stack")+
  labs(title = as.character(unique(mean_daily_dsf_prov$scenario)), y=str_wrap("Mean Hourly Provisioned Capacity by Month (GW)", width = 20), x='Month')+
    scale_fill_manual(values=c("#FFE66D","#FF6B6B","#4ECDC4"))+
    scale_color_manual(values=c("#FFE66D","#FF6B6B","#4ECDC4"))+
  scale_x_continuous(breaks = pretty(mean_daily_dsf_prov$month, n = 6))+
  #scale_x_datetime(breaks = date_breaks(width = "1 day"), labels = date_format("%b %d\n%I %p"), expand = c(0, 0))+
  #scale_y_continuous(breaks=seq.py.t, limits=c(0, max(seq.py.t)), expand=c(0,0))+
  theme(legend.key       = element_rect(color = "grey80", size = 0.4),
        legend.key.size  = grid::unit(1.2, "lines"),
        legend.text      = element_text(size=text.plot*1.1,face = 'bold'),
        legend.title     = element_text(size=text.plot*1.1,face = 'bold'),
        axis.text        = element_text(size=text.plot*1.2,face = 'bold'),
        plot.title       = element_text(size=text.plot*1.2,face = 'bold'),
        axis.title       = element_text(size=text.plot, face='bold'),
        axis.title.x     = element_text(vjust=-0.3),
        aspect.ratio = 0.3)}
ref_plot = mean_daily_dsf_plot(mean_daily_dsf_prov_ref)
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'ref_enh_dsf_service_provision_mean_month.png'), ref_plot,
       device = "png", width=10, height=4, units="in", dpi=330)

high_plot =mean_daily_dsf_plot(mean_daily_dsf_prov_high)
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'high_enh_dsf_service_provision_mean_month.png'), high_plot,
       device = "png", width=10, height=4, units="in", dpi=330)

##----------------------------------------------------##
##------ make diurnal plot of DSF services-------------##
##----------------------------------------------------##
interval.pump.load   = tryCatch( interval_pump_load(db), error = function(cond) { return('ERROR') } ) 
interval.pump.load = interval.pump.load[, Type:=gen.type.mapping[as.character(name)] ][,.(scenario,Type,value,time)]
dsf.pumpload = interval.pump.load[Type %like% "DSF"][,.(GW=sum(value)*0.001), 
                                                     by = .(scenario, time, Type)]
rm(interval.pump.load)
dsf.pumpload$date = as.Date(dsf.pumpload$time)
dsf.pumpload$month = as.numeric((format(dsf.pumpload$date, format = "%m")))
dsf.pumpload$day = as.numeric((format(dsf.pumpload$date, format = "%d")))
dsf.pumpload$interval = format(strptime(dsf.pumpload$time, "%Y-%m-%d %H:%M:%S"), "%H:%M")
dsf.int.prov$interval = format(strptime(dsf.int.prov$time, "%Y-%m-%d %H:%M:%S"), "%H:%M")

##save(dsf.pumpload, dsf.int.prov, file = paste0(output.dir,"ref_high_enh_dsf_pumpload_gen.RData"))
##setwd("//nrelqnap02/reeds/FY18-EFS/plexos/rplexos_solutions/plot/1120")
##load("ref_high_enh_dsf_pumpload_gen.RData")


dsf.pumpload.pivot = dsf.pumpload[,.(GW = sum(GW)* (-1)), by=.(scenario, time, date, month, day, interval, Type)]
dsf.pumpload.pivot$Service = "Energy (Consumption)"
dsf.service.int = rbind(dsf.pumpload.pivot, dsf.int.prov)
dsf.service.int$interval = as.ITime(dsf.service.int$interval)

##save(dsf.service.int, file = paste0(output.dir,"ref_high_enh_dsf_service_int.RData"))

dsf.service.int.agg = dsf.service.int[,.(GW = sum(GW)), by = .(scenario, time, interval, Service)]
dsf.service.int_ref = dsf.service.int.agg[scenario %like% 'Ref'][,.(GW = mean(GW)), by = .(scenario, interval, Service)]
dsf.service.int_high = dsf.service.int.agg[scenario %like% 'High'][,.(GW = mean(GW)), by = .(scenario, interval, Service)]

dsf_diurnal_plot =  function(dt){
  dt$Service = factor(dt$Service, levels = c("Flexibility", "Contingency", "Energy", "Energy (Consumption)"))
  ggplot(dt, aes(x= interval, y=GW))+
  geom_area(stat = "identity", aes(color=Service, fill=Service), position="stack")+
  labs(title = as.character(unique(dt$scenario)), y=str_wrap("Mean Hourly Provisioned Capacity (GW)", width = 20), x='Time of a Day')+
  scale_fill_manual(values=c("#FFE66D","#FF6B6B","#4ECDC4","#1A535C"))+
  scale_color_manual(values=c("#FFE66D","#FF6B6B","#4ECDC4","#1A535C"))+
  scale_x_time()+
  #geom_line(data = dt[Service %like% 'Consumption'], aes(color = "black"))+
  #scale_y_continuous(breaks=seq.py.t, limits=c(0, max(seq.py.t)), expand=c(0,0))+
  theme(legend.key       = element_rect(color = "grey80", size = 0.4),
        legend.key.size  = grid::unit(1.2, "lines"),
        legend.text      = element_text(size=text.plot,face = 'bold'),
        legend.title     = element_text(size=text.plot,face = 'bold'),
        axis.text        = element_text(size=text.plot,face = 'bold'),
        axis.text.x      = element_text(angle = -30, hjust = 0),
        plot.title       = element_text(size=text.plot,face = 'bold'),
        axis.title       = element_text(size=text.plot, face='bold'),
        axis.title.x     = element_text(vjust=-0.3),
        aspect.ratio = 0.3)}

ref_dsf_diurnal_service_plot = dsf_diurnal_plot(dsf.service.int_ref)
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'ref_enh_dsf_service_provision_mean_diurnal.png'), ref_dsf_diurnal_service_plot,
       device = "png", width=10, height=4, units="in", dpi=330)

high_dsf_diurnal_service_plot = dsf_diurnal_plot(dsf.service.int_high)
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'high_enh_dsf_service_provision_mean_diurnal.png'), high_dsf_diurnal_service_plot,
       device = "png", width=10, height=4, units="in", dpi=330)





##-----------------------------------------------------------------------------##
##--- diurnal plot of DSF energy provision by sector by aggregated region -----##
##-----------------------------------------------------------------------------##
reg.zone = region.zone[,.(region = Region, rto = Zone)]
gen_region_agg = merge(gen.mapping, reg.zone, on = 'region')
gen_region_agg = gen_region_agg[,c('name','rto')]
gen_region_agg = unique(as.data.table(gen_region_agg))
setkey(gen_region_agg, name)

interval.generation   = tryCatch( interval_gen(db), error = function(cond) { return('ERROR') } )
interval.pump.load   = tryCatch( interval_pump_load(db), error = function(cond) { return('ERROR') } ) 

rto_agg = rto_table[,c('name','region')][,.(rto = name, region=region)]
setkey(rto_agg, rto)


diurnal_rto_data = function(dt){
  dt = dt[, Type:=gen.type.mapping[as.character(name)] ]
  dt = dt[Type %like% "DSF"]
  #remove DSF from type names
  dt$Type = gsub("DSF ", "", dt[,Type])
  dt$Type = gsub("Ind", "Other", dt[,Type])
  dt$Type = gsub("Comm", "Other", dt[,Type])
  dt$Type = gsub("Res", "Other", dt[,Type])
  dt$Type = gsub("Tran", "Transportation", dt[,Type])
  
  setkey(dt, name)
  dt = merge(dt, gen_region_agg, all.x=TRUE, allow.cartesian=TRUE)
  setkey(dt, rto)
  dt = merge(dt, rto_agg, all.x = TRUE)
  dt = unique(dt[,.(GW=sum(value)*0.001), by = .(scenario, time, Type, region)])
  names(dt)[names(dt) == 'region'] = 'rto'
  ##dt = unique(dt[,.(GW=sum(value)*0.001), by = .(scenario, time, Type, rto)]) ## if merge by original rto
  dt$date = as.Date(dt$time)
  dt$month = as.numeric((format(dt$date, format = "%m")))
  dt$day = as.numeric((format(dt$date, format = "%d")))
  dt$interval = format(strptime(dt$time, "%Y-%m-%d %H:%M:%S"), "%H:%M")
  return (dt)
}

dsf_gen_rto = diurnal_rto_data(interval.generation)
dsf_pump_rto = diurnal_rto_data(interval.pump.load)
dsf_gen_rto[,Service:='Energy']
dsf_pump_rto[,Service:="Energy (Consumption)"]
dsf_pump_rto$GW = - dsf_pump_rto$GW
dsf_rto = rbind(dsf_gen_rto, dsf_pump_rto)

## data checkpoint
#save(dsf_rto, gen_region_agg, rto_agg, file = paste0(output.dir,"dsf_rto.RData"))
load(paste0(output.dir, "dsf_rto.RData"))
## data checkpoint

dsf_rto_summer = dsf_rto[month>5 & month <9]
dsf_rto_winter = dsf_rto[month<3 | month >10]

calc_sum = function(dt, period = 'year'){
  if (period == 'year') {
    days = 365
  } else if (period == 'summer') {
    days = 92
  } else if (period == 'winter') {
    days = 120
  } else {print ('Must have a period')}
  dt[,.(GW = sum(GW)/days), by=.(scenario, rto, Type, interval, Service)]
  #dt[,.(gw_sum = sum(GW)/days), by=.(scenario, rto, interval, Service)]
}

calc_percent = function(dt, period = 'year'){
  if (period == 'year') {
    days = 365
  } else if (period == 'summer') {
    days = 92
  } else if (period == 'winter') {
    days = 120
  } else {print ('Must have a period')}
  dt = dt[,Daily_Total := .(sum(abs(GW))), by= .(scenario, rto, date, Type, Service)]
  dt[,Int_Percent := GW/ Daily_Total]
  dt[is.na(dt)] = 0
  dt = dt[,.(percent = sum(Int_Percent)*100/days), by=.(scenario, rto, interval, Type, Service)]
  #dt = dt[,percent_sum := sum(percent), by = .(scenario, rto, interval, Service)]
}

dsf_rto_interval_sum = calc_sum(dsf_rto)
dsf_percent = calc_percent(dsf_rto)

y_max = 50

add_season_to_strip = function(rto, suffix = ' winter') {paste0(rto, suffix)}

dsf_rto_plot =  function(dt, variable, period='year'){
  #dt$Type = gsub("DSF ", "", dt[,Type])
  #dt$Type = gsub("Ind", "Industry", dt[,Type])
  #dt$Type = gsub("Comm", "Commercial", dt[,Type])
  #dt$Type = gsub("Res", "Residential", dt[,Type])
  #dt$Type = gsub("Tran", "Transportation", dt[,Type])
  dt$Type = factor(dt$Type, levels = c("Other", "Transportation"))
  dt$Service = factor(dt$Service, levels = c("Energy", "Energy (Consumption)"))
  dt$scenario = factor(dt$scenario, levels = scenario.names)
  dt$time = as.POSIXct(dt$interval, format = "%H:%M")
  if (variable == 'GW'){
    ylabel = "Mean Energy Provision \n and Recovery (GW)"} else if (variable == 'percent') {
      ylabel = "Mean Energy Provision and Recovery (%)"} else {print ("wrong variable provided")}
  ggplot(dt, aes(x= time, y=GW))+ ## Need to change y variable manually
    geom_area(stat = "identity", aes(color=Type, fill=Type, alpha = Service), position="stack")+
    #geom_line(aes(x = time, y = GW, alpha = Service), ## Need to change y variable manually
    #          size = 1, color = "black")+
    labs(title = "", y=str_wrap(ylabel, width = 20), x='Time of a Day')+
    scale_fill_manual(values = efs_color)+
    scale_color_manual(values = efs_color)+
    scale_alpha_manual(values=c(1, 0.3)) +
    scale_x_datetime(date_breaks = "6 hour", date_labels = "%H:%M")+
    #geom_line(data = dt[Service %like% 'Consumption'], aes(color = "black"))+
    scale_y_continuous(breaks=pretty(c(-y_max,y_max), n = 7, min.n = 6), 
                       limits=c(-y_max,y_max), expand=c(0,0))+
    #guides(alpha = FALSE)+
    theme(legend.key       = element_rect(color = "grey80", size = 0.4),
          legend.key.size  = grid::unit(1.2, "lines"),
          legend.text      = element_text(size=text.plot,face = 'bold'),
          legend.title     = element_text(size=text.plot,face = 'bold'),
          legend.position  = 'none', #remove all legends
          axis.text        = element_text(size=text.plot,face = 'bold'),
          axis.text.x      = element_text(angle = -30, hjust = 0),
          plot.title       = element_text(size=text.plot,face = 'bold'),
          axis.title       = element_text(size=text.plot, face='bold'),
          axis.title.x     = element_text(vjust=-0.3),
          strip.text     = element_text(size=text.plot,face = 'bold'),
          aspect.ratio = 0.6)+
    facet_wrap(~ rto, nrow = 4, labeller = as_labeller(add_season_to_strip))}

p = dsf_rto_plot(dsf_rto_interval_sum, 'GW')
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'dsf_rto_diurnal_sum.png'), p,
       device = "png", width=13, height=10, units="in", dpi=330)

p2 = dsf_rto_plot(dsf_percent, 'percent')
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'dsf_rto_diurnal_percent.png'), p2,
       device = "png", width=13, height=10, units="in", dpi=330)

p_summer = dsf_rto_plot(calc_sum(dsf_rto_summer, 'summer'),'GW')
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'dsf_rto_diurnal_sum_summer.png'), p_summer,
       device = "png", width=13, height=10, units="in", dpi=330)

p_winter = dsf_rto_plot(calc_sum(dsf_rto_winter, 'winter'),'GW')
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'dsf_rto_diurnal_sum_winter.png'), p_winter,
       device = "png", width=13, height=10, units="in", dpi=330)

p_summer_perc = dsf_rto_plot(calc_percent(dsf_rto_summer, 'summer'), 'percent')
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'dsf_rto_diurnal_percent_summer.png'), p_summer_perc,
       device = "png", width=13, height=10, units="in", dpi=330)

p_winter_perc = dsf_rto_plot(calc_percent(dsf_rto_winter, 'winter'), 'percent')
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'dsf_rto_diurnal_percent_winter.png'), p_winter_perc,
       device = "png", width=13, height=10, units="in", dpi=330)



## plot individual rto/region figures

zone = unique(dsf_rto$rto)
for (i in zone){
  p = dsf_rto_plot(calc_sum(dsf_rto_summer[rto == i], 'summer'), 'GW')
  ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'), paste0(i, '_dsf_summer_gw_agg.png')),
         p, device = 'png', width = 7, height = 5, units = "in", dpi = 330)
}

for (i in zone){
  p = dsf_rto_plot(calc_sum(dsf_rto_winter[rto == i], 'winter'), 'GW')
  ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'), paste0(i, '_dsf_winter_gw_agg.png')),
         p, device = 'png', width = 7, height = 5, units = "in", dpi = 330)
}



##----------------------------------------------------------------##
##--- diurnal plot of DSF energy provision by sector by season-----##
##----------------------------------------------------------------##
interval.generation   = tryCatch( interval_gen(db), error = function(cond) { return('ERROR') } )
interval.generation = interval.generation[, Type:=gen.type.mapping[as.character(name)] ][,.(scenario,Type,value,time)]
dsf.generation = interval.generation[Type %like% "DSF"][,.(GW=sum(value)*0.001), 
                                                   by = .(scenario, time, Type)]
dsf.energy.gen = copy(dsf.generation)

interval.pump.load   = tryCatch( interval_pump_load(db), error = function(cond) { return('ERROR') } ) 
interval.pump.load = interval.pump.load[, Type:=gen.type.mapping[as.character(name)] ][,.(scenario,Type,value,time)]
dsf.pumpload = copy(interval.pump.load[Type %like% "DSF"][,.(GW=sum(value)*0.001), 
                                                     by = .(scenario, time, Type)])
dsf.energy.gen[,Service:='Energy']
dsf.pumpload[,Service:="Energy (Consumption)"]
dsf.energy.service = rbind(dsf.energy.gen, dsf.pumpload)

dsf.energy.service$date = as.Date(dsf.energy.service$time)
dsf.energy.service$month = as.numeric((format(dsf.energy.service$date, format = "%m")))
dsf.energy.service$day = as.numeric((format(dsf.energy.service$date, format = "%d")))
dsf.energy.service$interval = format(strptime(dsf.energy.service$time, "%Y-%m-%d %H:%M:%S"), "%H:%M")

season = data.table(month=1:12, season = c("winter","winter","spring","spring","spring","summer",
                                           "summer","summer", "fall", "fall","winter", "winter"))
dsf.energy.season = dsf.energy.service[season, on = "month"]
dsf.energy.season[,Total := .(sum(GW)), by= .(scenario, date, Type, Service)]
dsf.energy.season[,Mean := .(mean(GW)), by=.(scenario, time, Type, Service)]
dsf.energy.season[,Percentage := Mean/Total]
invisible(lapply(names(dsf.energy.season),function(.name) set(dsf.energy.season, which(is.infinite(dsf.energy.season[[.name]])), j = .name,value =0)))

## plot DSF energy service total
dsf.energy.season.mean= dsf.energy.season[,.(mean_provision = mean(GW)), by = .(scenario, Type, Service, season, interval)]
dsf.energy.season.mean[Service == "Energy (Consumption)", mean_provision := -mean_provision]

## data checkpoint DSF energy provision and recovery by season by sector plot
##save(dsf.energy.season, dsf.energy.season.mean, season, 
##        dsf.energy.service, dsf.energy.gen, dsf.pumpload, 
##        file = paste0(output.dir,"ref_high_enh_dsf_energy_prov_season.RData"))
#setwd("//nrelqnap02/reeds/FY18-EFS/plexos/rplexos_solutions/plot/1120")
#load("ref_high_enh_dsf_energy_prov_season.RData")


#select scenario to plot
#dsf.energy.season.agg_ref = dsf.energy.season.agg[scenario %like% 'Ref']
#dsf.energy.season.agg_high = dsf.energy.season.agg[scenario %like% 'High']

dsf_energy_season_mean_plot =  function(dt){
  dt$Type = gsub("DSF ", "", dt[,Type])
  dt$Type = gsub("Ind", "Industry", dt[,Type])
  dt$Type = gsub("Comm", "Commercial", dt[,Type])
  dt$Type = gsub("Res", "Residential", dt[,Type])
  dt$Type = gsub("Tran", "Transportation", dt[,Type])
  dt$Type = factor(dt$Type, levels = c("Other", "Industry", "Commercial", "Residential", "Transportation"))
  dt$Service = factor(dt$Service, levels = c("Energy", "Energy (Consumption)"))
  dt$season = factor(dt$season, levels = c("spring", "summer", "fall", "winter"))
  dt$time = as.POSIXct(dt$interval, format = "%H:%M")
  ggplot(dt, aes(x= time, y=mean_provision))+
    geom_area(stat = "identity", aes(color=Type, fill=Type, alpha = Service), position="stack")+
    labs(title = "", y="Mean Energy Provision \n and Recovery (GW)", x='Time of a Day')+
    scale_fill_manual(values = efs_color)+
    scale_color_manual(values = efs_color)+
    scale_alpha_manual(values=c(1, 0.3)) +
    scale_x_datetime(date_breaks = "6 hour", date_labels = "%H:%M")+
    #geom_line(data = dt[Service %like% 'Consumption'], aes(color = "black"))+
    #scale_y_continuous(breaks=seq.py.t, limits=c(0, max(seq.py.t)), expand=c(0,0))+
    guides(alpha = FALSE)+
    theme(legend.key       = element_rect(color = "grey80", size = 0.4),
          legend.key.size  = grid::unit(1.2, "lines"),
          legend.text      = element_text(size=text.plot,face = 'bold'),
          legend.title     = element_text(size=text.plot,face = 'bold'),
          axis.text        = element_text(size=text.plot,face = 'bold'),
          axis.text.x      = element_text(angle = -30, hjust = 0),
          plot.title       = element_text(size=text.plot,face = 'bold'),
          axis.title       = element_text(size=text.plot, face='bold'),
          axis.title.x     = element_text(vjust=-0.3),
          strip.text     = element_text(size=text.plot,face = 'bold'),
          aspect.ratio = 0.7)+
    facet_grid(cols = vars(season), 
               rows = vars(scenario),
               scales = "free_y")}
p = dsf_energy_season_mean_plot(dsf.energy.season.mean)

ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'energy_season_plot_unfix_new.png'), p,
       device = "png", width=11, height=5, units="in", dpi=330)
##save(dsf.energy.season, file = paste0(output.dir,"ref_high_enh_dsf_energy_season.RData"))


## make DSF percentage plot
dsf.energy.season.agg = dsf.energy.season[,.(Percentage = mean(Percentage)), by =.(scenario, interval, season, Type, Service)]
dsf.energy.season.agg[Service == "Energy (Consumption)", Percentage := -Percentage]

#dsf.energy.season.agg_ref = dsf.energy.season.agg[scenario %like% 'Ref']
#dsf.energy.season.agg_high = dsf.energy.season.agg[scenario %like% 'High']

dsf_energy_season_plot =  function(dt){
  dt$Type = gsub("DSF ", "", dt[,Type])
  dt$Type = gsub("Ind", "Industry", dt[,Type])
  dt$Type = gsub("Comm", "Commercial", dt[,Type])
  dt$Type = gsub("Res", "Residential", dt[,Type])
  dt$Type = gsub("Tran", "Transportation", dt[,Type])
  dt$Type = factor(dt$Type, levels = c("Other", "Industry", "Commercial", "Residential", "Transportation"))
  dt$Service = factor(dt$Service, levels = c("Energy", "Energy (Consumption)"))
  dt$season = factor(dt$season, levels = c("spring", "summer", "fall", "winter"))
  dt$Percentage  = dt$Percentage * 100 # multiple 100 to get X%
  dt$interval = as.POSIXct(dt$interval, format = "%H:%M")
  ggplot(dt, aes(x= interval, y=Percentage))+
    geom_area(stat = "identity", aes(color=Type, fill=Type, alpha = Service), position="stack")+
    labs(title = "", y=str_wrap("Mean Energy Provision and Recovery (%)", width = 20), x='Time of a Day')+
    scale_fill_manual(values = efs_color)+
    scale_color_manual(values = efs_color)+
    scale_alpha_manual(values=c(1, 0.3)) +
    scale_x_datetime(date_breaks = "6 hour", date_labels = "%H:%M")+
    #geom_line(data = dt[Service %like% 'Consumption'], aes(color = "black"))+
    #scale_y_continuous(breaks=seq.py.t, limits=c(0, max(seq.py.t)), expand=c(0,0))+
    guides(alpha = FALSE)+
    theme(legend.key       = element_rect(color = "grey80", size = 0.4),
          legend.key.size  = grid::unit(1.2, "lines"),
          legend.text      = element_text(size=text.plot,face = 'bold'),
          legend.title     = element_text(size=text.plot,face = 'bold'),
          axis.text        = element_text(size=text.plot,face = 'bold'),
          axis.text.x      = element_text(angle = -30, hjust = 0),
          plot.title       = element_text(size=text.plot,face = 'bold'),
          axis.title       = element_text(size=text.plot, face='bold'),
          axis.title.x     = element_text(vjust=-0.3),
          strip.text     = element_text(size=text.plot,face = 'bold'),
          aspect.ratio = 0.7)+
    facet_grid(cols = vars(season), rows = vars(scenario))}

p = dsf_energy_season_plot(dsf.energy.season.agg)
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'energy_season_percentage_new.png'), p,
       device = "png", width=11, height=5, units="in", dpi=330)


##----------------------------------------------------##
## revenue and cost plot
##----------------------------------------------------##
setnames(total.revenue, old = c('Revenue_Type','revenue'), new = c('property','value'))
total_rev_cost = do.call("rbind", list (total.revenue, agg_ss_cost,agg_vom_cost,agg_fuel_cost))
# data checkpoint
save(total_rev_cost, file = file.path(output.dir, paste(scenario.names, collapse = '_'), 'total_rev_cost.RData'))

revenue.p = ggplot(total_rev_cost)+
  geom_bar(aes(scenario,value/10^6,fill=property), stat='identity')+
  geom_errorbar(data = net.revenue, aes(x = scenario, ymin=revenue/10^6, ymax=revenue/10^6,linetype=Revenue_Type), 
                size=0.75) +
  ylab("Total Revenue, Million $") + xlab("Scenario")+
  #scale_fill_manual(values=c('firebrick1','#313D27','dodgerblue2','goldenrod1','#5C5E40','#729C7A'),name='Revenue and Cost Type')+
  scale_linetype_manual(values='longdash',name='')+
  facet_wrap(~Type,ncol=4,scales='free') +
  theme(    legend.key      = element_rect(color="grey80", size = 0.8),
            legend.key.size = grid::unit(1.0, "lines"),
            legend.text     = element_text(size=text.plot),
            legend.title    = element_blank(),
            axis.text       = element_text(size=text.plot/1.2),
            axis.text.x     = element_text(angle = -30, hjust = 0),
            axis.title      = element_text(size=text.plot, face=2),
            axis.title.y    = element_text(vjust=1.2),
            strip.text      = element_text(size = text.plot),
            panel.spacing   = unit(1, "lines"),
            plot.margin = unit(c(0,0,0,0),"mm"))
ggsave(revenue.p, filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'revenue_cost_by_gen_type.png'), device = "png",
       width=10, height=9, units="in", dpi=330)

##-----------------------------------------------------------------------##
## curtailment query
##-----------------------------------------------------------------------##
total_curtailment = function(interval.generation, interval.avail.cap) {
    c.gen = interval.generation[Type %in% re.types,.(value=sum(value)),by=.(scenario, time)]
    c.avail = interval.avail.cap[Type %in% re.types,.(value=sum(value)),by=.(scenario, time)]
    setkey(c.avail,scenario,time)
    setkey(c.gen,scenario,time)
    curt = c.avail[c.gen][,curt := value-i.value]
    curt.tot = curt[,.(Curtailment=sum(curt)),by=.(scenario,time)]
    curt.tot[,month := 1+as.POSIXlt(time)[[5]]]
    curt.tot[,day := 1+as.POSIXlt(time)[[8]]]
    curt.tot[,interval := 1:intervals.per.day,by=.(scenario,day)]
  return(curt.tot)
}
agg_curt = total_curtailment(agg_interval_generation, agg_interval_avail_cap)
# data checkpoint
save(agg_curt, file = file.path(output.dir, paste(scenario.names, collapse = '_'), 'agg_curt.RData'))
tmp = as.data.table(agg_curt)
tmp = tmp[,c('scenario','time','Curtailment')]
write.csv(tmp, file.path(data.dir,'figure29_high.csv'))

## curtailment monthly plot

monthly.curt.tot = agg_curt[,.(Curtailment = sum(Curtailment)/1000000),by=.(scenario,month)]
p.monthly = ggplot(monthly.curt.tot, aes(x=month, y = Curtailment, fill = scenario))+
                    geom_bar(stat = "identity", position=position_dodge()) +
                    scale_fill_manual(values = scen_color()) +
                    scale_x_continuous(name = "Month", breaks = c(3,6,9,12)) +
                    scale_y_continuous(name = 'Total Curtailment by Month (TWh)', 
                                       limits = c(0, 50))+
                    theme_bw(base_size = 16)+
                    theme(aspect.ratio = 1)
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'curtailment_monthly_twh.png'), p.monthly, device = 'png',
       width=7,height=5, units='in', dpi=330)

##-----------------------------------------------------------------------##
## diurnal curtailment plot
##-----------------------------------------------------------------------##
agg_curt = rbind(agg_curt, hire_agg_curt)

avg.curt = agg_curt[,.(Curtailment_GWh=mean(Curtailment)/1000),by=.(scenario,interval)]
avg.curt[, hour := floor((interval-1)*(3600*24/intervals.per.day)/3600)]
avg.curt[, minute := floor((interval-1)*(3600*24/intervals.per.day)/60-hour*60)]
avg.curt[, second := floor((interval-1)*(3600*24/intervals.per.day)-hour*3600-minute*60)]
avg.curt[, time := as.POSIXct(paste(hour,minute,second, sep=":"),'UTC', format="%H:%M:%S")]
# data checkpoint
save(avg.curt, file = file.path(output.dir, paste(scenario.names, collapse = '_'), 'avg.curt.RData'))

# Total plots
# Sum up the curtailment each interval to get average interval curtailment. Assign an interval to each row.
avg.curt.tot = avg.curt[,.(Curtailment_GWh=sum(Curtailment_GWh)),by=.(scenario,hour,minute,second,time)]

p.int = line_plot(avg.curt.tot, filters=c('scenario','time'), x.col='time', y.col='Curtailment_GWh',
                  y.lab='Mean Hourly Curtailment (GWh)', color='scenario') 
p.int = p.int + 
  scale_color_manual(values = scen_color())+
  scale_x_datetime(breaks = date_breaks(width = "4 hour"), labels = date_format("%H:%M"), 
                   expand = c(0, 0), timezone='UTC')+
  scale_y_continuous(limits = c(0,70))+
  theme(legend.title = element_text(size = 16),
    aspect.ratio = 1)

## Calculate diffs
# diff.curt = avg.curt.tot[, .(scenario, Curtailment_GWh = Curtailment_GWh - Curtailment_GWh[as.character(scenario)==ref.scenario]), by=.(time)]
# 
# p.int.diff = line_plot(diff.curt, filters=c('scenario','time'), x.col='time', y.col='Curtailment_GWh',
#                        y.lab='Difference in Curtailment (GWh)', color='scenario')
# p.int.diff = p.int.diff + scale_color_brewer(palette='Set1') +
#   scale_x_datetime(breaks = date_breaks(width = "2 hour"), labels = date_format("%H:%M"), 
#                    expand = c(0, 0), timezone='UTC')
# p.int.diff = p.int.diff
#ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'), 'curtailment_int_2.png'), p.int, device = 'png',
#       width=10,height=8, units='in', dpi=330)

ggsave(filename = file.path(output.dir, 'curtailment_int_2.png'), p.int, device = 'png',
       width=10,height=8, units='in', dpi=330)

##-----------------------------------------------------------------------##
# region generation
##-----------------------------------------------------------------------##
# r.z.gen = tryCatch( region_zone_gen(total.generation, total.avail.cap), 
#                     error = function(cond) { return('ERROR: region_zone_gen function not returning correct results.') } )
# # r.z.pumpload = tryCatch(region_zone_pumpload(total.pumpload), 
# #                         error = function(cond) {return('ERROR: calculating total region zone pump load function not returning correct results.')})
# # # add pump load as negative to gen stack; subtract pump load from load
# # temp.pumpload = as.data.table(r.z.pumpload)
# # temp.pumpload$Type = paste(temp.pumpload$Type, "Pump_Load", sep="_")
# # temp.pumpload$GWh = temp.pumpload$GWh * (-1)
# # r.z.gen = rbind(r.z.gen, temp.pumpload)
# # z.pumpload = temp.pumpload[,.(value = sum(GWh)), by = .(scenario, Zone)]
# # z.load = rbind(z.load, z.pumpload)
# z.load = z.load[,.(value=sum(value)), by = .(scenario,Zone)]
# 
# z.gen.plot.data = gen_stack_plot(r.z.gen, load.data = z.load, 
#                            filters = 'Zone', x_col='scenario')
# z.gen.plot.data = z.gen.plot.data[[1]] + facet_wrap(~Zone, scales = 'free', ncol=5)+
#      theme(plot.margin =unit(c(0.5,0.5,0.5,0.5), "cm"),
#            #aspect.ratio = 1.5/length(unique(r.z.gen$scenario)), 
#            axis.text.x = element_text(angle = -30, hjust = 0),
#            text = element_text(face='bold'))
# ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'zone_gen_total.png'), z.gen.plot.data, device = 'png',
#        width=13,height=12, units='in', dpi=330)

##----------------------------------------##
## get region zone import export data
##----------------------------------------##
# for BA, p-level data
r.stats = tryCatch( region_stats(total.region.load, total.region.imports, total.region.exports, total.region.ue), 
                    error = function(cond) { return('ERROR: region_stats function not returning correct results.') })
# for zone, RTO-level data
z.stats = tryCatch( zone_stats(total.region.load, total.region.imports, total.region.exports, total.region.ue, 
                               total.zone.load, total.zone.imports, total.zone.exports, total.zone.ue), 
                    error = function(cond) { return('ERROR: zone_stats function not returning correct results.') })
z.export = as.data.table(z.stats)[,c("name","scenario","Imports","Exports")]
z.export$Imports = z.export$Imports* (-1)
z.export$NetEx = z.export$Exports + z.export$Imports
z.export = melt(z.export, id.vars = c("name", "scenario", "NetEx"), variable.name = "property", value.name = "value")

rto.export = merge(z.export, rto_table[,.(name,region)], by = c("name"))

rto.export = rto.export[,.(value = sum(value)/1000, NetEx = sum(NetEx)/1000), by = .(region, scenario, property)]
# data checkpoint
save(rto.export, file = file.path(output.dir, paste(scenario.names, collapse = '_'), 'rto.export.RData'))


rto_export_plot = ggplot (data = rto.export, aes(x=region, y=value, fill=factor(scenario)))+
  geom_bar(position = "dodge", stat = "identity") +
  geom_point(position = position_dodge(width = 0.9), shape = 18, size=3, color = 'red', aes(x=region, y=NetEx, group=scenario))+
  scale_fill_manual(values = scen_color())+
  scale_y_continuous(breaks = pretty(c(-max(rto.export$value), max(rto.export$value)), n = 5, min.n = 4),
                     limits=c(-max(rto.export$value)*1.1, max(rto.export$value))*1.1, expand=c(0,0))+
  coord_flip()+
  labs(fill = "Scenario") +
  xlab("Region") +
  ylab("Total Export and Import (GWh)")+
  geom_hline(yintercept=0, size = 1, color = "black")+
  annotate( geom = "text", x = "FRCC", y =-250, label = "Import", color = "black", size = 7)+
  annotate( geom = "text", x = "FRCC", y =250, label = "Export", color = "black", size = 7)+
  theme_bw(base_size = 20)
ggsave(filename = file.path(output.dir, paste(scenario.names, collapse = '_'),'rto_export.png'), rto_export_plot, device = 'png',
       width=10,height=8, units='in', dpi=330)


