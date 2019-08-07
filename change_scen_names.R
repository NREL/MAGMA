print(scenario.names)
newname <-c('BaseCase_HighElec','BaseCase_HighElec_DR')
new_scen <- function(data) {
  if ('scenario' %in% colnames(data)){
     data$scenario<-ifelse(data$scenario==toString(scenario.names[1]),newname[1],
                        ifelse(data$scenario==toString(scenario.names[2]),newname[1],
                               ifelse(data$scenario==toString(scenario.names[3]),newname[2],
                                      ifelse(data$scenario==toString(scenario.names[4]),newname[2],data$scenario))))
  }
  return(data)
}

results=lapply(ls(),function(i){
  x=get(i)
  if(is.data.frame(x)){
    x=new_scen(x)
  }
})
results=results[!sapply(results,is.null)]