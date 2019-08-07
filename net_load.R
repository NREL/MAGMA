library(reshape2)
scen='BaseCase_HighElec'
peri='Summer'

vre_gen = data.table(gen.type.total)
vre_gen=vre_gen[Type=='Solar' | Type=='Wind']
vre_gen=vre_gen[,.(value=sum(value)), by=.(scenario,Period,time)]
#vre_gen$time=as.POSIXct(strptime(vre_gen$time, "%Y-%m-%d %H:%M:%S"))

dis_load = data.table(gen.load.total)
dis_load=dis_load[,.(value=sum(value)), by=.(scenario,Period,time)]
setnames(dis_load,"value","load")
#dis_load$time=as.POSIXct(strptime(dis_load$time),"%Y-%m-%d %H:%M:%S")

net_load =dis_load[vre_gen, on = c('scenario','Period','time')]
net_load$net_load=net_load$load-net_load$value
net_load = melt(net_load, id.vars = c('scenario','Period','time'),measure.vars = c("load","net_load"))
setnames(net_load,'variable','property')
net_load[, value:=value/1000]

net_load=net_load[scenario==scen]
net_load=net_load[Period==peri]

p = ggplot(net_load) + 
  geom_line(aes(x = time, y = value, color = property, group = property), lwd = 2)+
  scale_colour_brewer(palette='Dark2')+
  scale_x_datetime(breaks = date_breaks(width = "6 hour"), labels = date_format("%b %d\n%I %p"), expand = c(0, 0))+
  scale_y_continuous(breaks=c(0,500,1000,1500,2000,2500), limits=c(0,2600))+ ##manually adjust based on value
  theme(legend.key       = element_rect(color = "grey80", size = 1.5),
        legend.key.size  = grid::unit(1.5, "lines"),
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
        aspect.ratio     = 0.5)+
  ylab('GWh')
p
ggsave(filename = paste0(fig.path.name,scen,peri,'.png'), width = 10, height = 8,dpi = 300)

