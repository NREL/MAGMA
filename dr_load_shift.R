library(reshape2)
library(ggplot2)

scen='NoFlex_RefElec_MIP_DR'
peri='Summer'

new_load = data.table(gen.load.total)
new_load=new_load[scenario==scen]
new_load=new_load[,.(value=sum(value)), by=.(Period,time)]
setnames(new_load,"value","new_load")

dr_gen=data.table(gen.type.total)
dr_gen=dr_gen[scenario==scen & Type=='DR']
dr_gen=dr_gen[,.(value=sum(value)), by=.(Period,time)]
setnames(dr_gen,'value','load_reduction')

for ( i in 1:n.periods ) {
  key.period.time = seq(start.end.times[i,start], start.end.times[i,end], 
                        by = timediff)
  key.period.pump = interval.pump.load[interval.pump.load$time %in% key.period.time]
  key.period.pump[, Period := period.names[i]]
  
  if ( i == 1 ) {
    int.pump.key.periods = key.period.pump
  } else {
    int.pump.key.periods = rbindlist(list(int.pump.key.periods, key.period.pump))
  }
}


dr_pump=data.table(int.pump.key.periods)
dr_pump=dr_pump[grep("^DR_",dr_pump$category)]
dr_pump=dr_pump[,.(value=sum(value)), by=.(Period,time)]
setnames(dr_pump,'value','load_increase')

tot_dr_load=merge(new_load,dr_gen,by=c('Period','time'))
tot_dr_load=merge(tot_dr_load,dr_pump,by=c('Period','time'))
tot_dr_load$old_load=tot_dr_load$new_load - tot_dr_load$load_increase + tot_dr_load$load_reduction
tot_dr_load$load_reduction=NULL
tot_dr_load$load_increase=NULL
dr_load_plot=melt(tot_dr_load,id.vars=c('Period','time'))


dr_load_plot[, value:=value/1000]
#dis_load$time=as.POSIXct(strptime(dis_load$time),"%Y-%m-%d %H:%M:%S")

dr_load_plot=dr_load_plot[Period==peri]

p = ggplot(dr_load_plot,aes(x = time, y = value, group = variable)) + 
  geom_line(aes(linetype=variable),size=2)+
  #scale_colour_brewer(palette='Accent',direction = -1)+
  scale_x_datetime(breaks = date_breaks(width = "6 hour"),
                   labels = date_format("%b %d\n%I %p"), expand = c(0, 0))+
  scale_y_continuous(name='GWh')+
  theme(legend.key       = element_rect(color = "grey80", size = 2),
        legend.key.size  = grid::unit(2, "lines"),
        #legend.text      = element_text(size=16),
        legend.title = element_blank(),
        text = element_text(size=25),
        #strip.text       = element_text(size=text.plot,face = 'bold'),
        #axis.text        = element_text(size=text.plot/1.2,face = 'bold'),
        ##axis.title       = element_text(size=text.plot, face='bold'),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour = "grey85"),
        panel.grid.minor = element_line(colour = "grey93"),
        panel.spacing    = unit(2,'lines'),
        aspect.ratio     = 0.5)
p
ggsave(filename = paste0(fig.path.name,scen,'_',peri,'load_shift.png'), width = 10, height = 8,dpi = 300)