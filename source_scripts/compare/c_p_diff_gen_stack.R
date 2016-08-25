# Check if this section was selected run in the input file
if (total.gen.stack & length(db.loc)>1){

# Query annual generation by type.
yr.gen.scen = tryCatch( gen_diff_by_type(total.generation, total.avail.cap), error = function(cond) { return('ERROR') } )

# If the query doesn't work, print an error. Else continue.
if ( typeof(yr.gen.scen)=='character' ) {
  print('ERROR: gen_by_type function not returning correct results.')
} else {

    
  py = pretty(c(0,gen.sum$TWh), n=5, min.n = 5)
  seq.py = seq(min(py[1],0), py[length(py)], (py[2]-py[1]))

  # Call function to create plot and print
  [p1, seq.py] = gen_diff_stack_plot(yr.gen.scen, r.load)
  # This automatically creates the y-axis scaling
  p1 = p1 + scale_y_continuous(breaks=seq.py, limits=c(min(py), max(py)), expand=c(0,0), label=comma)
  print(p1)

}

} else { print('Section not run according to input file.')}
