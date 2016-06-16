# Check if this section was selected to run in the input file
if (interface.flow.plots) {

# Call the query function to get interface flows for the interfaces selected in the query function.
interface.flows = tryCatch( interval_interface_flows(int.data.interface), error=function(cond) {return('ERROR: interface_flows query not returning correct results.')})
  
# Check for errors in the query function. If theres an error don't continue.
if ( typeof(interface.flows)=='character' ) { 
print('ERROR: interface_flows function not returning correct results.')
} else {

# Define zone names, remove region flows, and order zone names for plotting.
interface.flows$name = factor(interface.flows$name, levels = interfaces) 

# Create plot of interval zone interface flow
p1 = ggplot(interface.flows, aes(x=time, y=value/1000, color=name, group=name))+
      geom_line(size=1.2)+
      geom_hline(yintercept=0, color="black", size=0.3)+
        scale_x_datetime(breaks = date_breaks(width = "1 month"), limits=c(first.day, last(seq(first.day, by='month', length.out=12))), labels = date_format("%b"), expand = c(0, 0))+
        scale_color_manual("", values = scen.pal)+
            labs(y="Flow (GW)", x = '', title='Interval Flow')+
            theme(legend.key = element_rect(NULL),
                  legend.text = element_text(size=text.plot),
                  text=element_text(size=text.plot),
                  strip.text=element_text(face="bold", size=rel(1)),
                  axis.text=element_text(face=2, size=text.plot/1),
                  axis.title=element_text(size=text.plot, face=2.3),
                  # legend.position=c(0.80, 0.12),
                  panel.margin = unit(0.35, "lines"))

# Aggregate interval flow data into daily flow data
daily.flows = dcast(interface.flows, time ~ name, value.var = 'value', fun.aggregate=mean)
daily.flows$day = rep(1:(nrow(daily.flows)/24), each=24)
daily.flows = melt(daily.flows, id.vars = 'day', measure.vars = interfaces, variable.name = 'name')

daily.flows = daily.flows %>%
  group_by(day, name) %>%
  summarise(value = sum(value))

# Create daily flow plot.
p2 = ggplot()+
      geom_line(data=daily.flows, aes(x=day, y=value/1000, color=name, group=name), size=1.2)+
      geom_hline(yintercept=0, color="black", size=0.3)+
      scale_color_manual("", values = scen.pal)+
      scale_x_continuous(breaks=seq(0, 360, by=30), limits=c(0,370), expand=c(0,0))+
      # scale_x_datetime(breaks = date_breaks(width = "1 month"), labels = date_format("%b %d\n%I %p"), expand = c(0, 0))+
        labs(y="Flow (GW)", x = 'Day', title='Daily Flow')+
            theme(legend.key = element_rect(NULL),
                  legend.text = element_text(size=text.plot),
                  text=element_text(size=text.plot),
                  strip.text=element_text(face="bold", size=rel(1)),
                  axis.text=element_text(face=2, size=text.plot/1),
                  axis.title=element_text(size=text.plot, face=2.3),
                  # legend.position=c(0.80, 0.12),
                  panel.margin = unit(0.35, "lines"))
print(p1)
print(p2)

}

} else { print('Section not run according to input file.') }
