# Check if this section was selected run in the input file
if (total.gen.stack & length(db.loc)>1){

# Query annual generation by type.
yr.gen.scen = tryCatch( gen_diff_by_type(total.generation, total.avail.cap), error = function(cond) { return('ERROR') } )

# If the query doesn't work, print an error. Else continue.
if ( typeof(yr.gen.scen)=='character' ) {
  print('ERROR: gen_by_type function not returning correct results.')
} else {

  # Call function to create plot and print
  p1 = gen_diff_stack_plot(yr.gen.scen, r.load)
  print(p1)

}

} else { print('Section not run according to input file.')}
