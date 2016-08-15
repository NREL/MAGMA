# Check if this section was selected run in the input file
if (total.gen.stack){

# Query annual generation by type.
yr.gen = tryCatch( gen_by_type(total.generation, total.avail.cap), error = function(cond) { return('ERROR') } )

# If the query doesn't work, print an error. Else continue.
if ( typeof(yr.gen)=='character' ) {
  print('ERROR: gen_by_type function not returning correct results.')
} else {

  # Create plot
  p1<-gen_stack_plot(yr.gen, r.load)
  print(p1)

}

} else { print('Section not run according to input file.')}
