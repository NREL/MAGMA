#PLEXOS-Vis Readme

This repository is intended as a submodule to a PLEXOS project specific repository. 

This package creates figures and an HTML file with all of those figures for plexos solutions.
* You must use process_folder() from the rplexos package to create a database file from the PLEXOS solution .zip, before running this code.

##To Run:
1. Copy run_html_output.R and input_data.csv into another folder associated with your project specific repository.
2. Edit input_data.csv to point to the required databases, input files, and generator categories for your project.
3. Edit run_html_output.R to point to your input_data.csv
4. Run: ```source('run_html_output.R')```

##### input_data.csv columns:
1. Database.Location
	+ Location of the .db file created from PLEXOS .zip solution file.
2. Using.Gen.Type.Mapping.CSV
	+ If usings a CSV file to assign generation type by generator name, this should be TRUE, else it should be FALSE. Column names should be "name" and "Type"
	+ If you don't do it this way, it will assign generation type by PLEXOS category.
3. Gen.Region.Zone.Mapping.Filename
	+ CSV file which assigns a Region and Zone to each generator. Column names should be "name", "Region", "Zone". This can also be the same file you use to assign generation type.
4. CSV.Gen.Type.File.Location
	+ Location of CSV file to assign generation type by generator name, if using this (Using.Gen.Type.Mapping.CSV == TRUE)
5. CSV.Gen.Type
	+ List containing all generation types in the CSV which maps generator name to generation type.
6. CSV.Desired.Type
	+ If you want to rename the generation type for displaying results, you can change them here.
7. PLEXOS.Gen.Category
	+ Categories in PLEXOS database. This must contain all the categories in your PLEXOS database.
8. PLEXOS.Desired.Type	
	+ Assigns a generation type to each of the PLEXOS categores
9. Gen.Type
	+ All types of generation that you either assign through the CSV or PLEXOS category. It can contain more types than you currently have, but it must have all of them otherwise they will be missing from the results.
10. Plot.Color
	+ Assigns a plot color to each generation type
11. Gen.Order
	+ Assigns an order from top to bottom of generation types for dispatch stacks. This list must also contain every generation type in that you assign either in 5-6 or 7-8.
12. Renewable.Types.for.Curtailment
	+ Generation types to be considered in curtailment calculations.
13. Key.Periods
	+ You can choose individual time spans to plot results for. This column assigns names to those time periods.
14. Start.Time
	+ Corresponding start time for each of the periods in 13.
15. End.Time
	+ Corresponding end time for each of the periods in 13. 
16. Start.Day
	+ Starting day of the PLEXOS run. 
17. End.Day
	+ Ending day of the PLEXOS run.
18. Intervals.Per.Day
	+ Number of intervals per day.
19. Sections.to.Run
	+ Sections (chunks) of the HTML_output.Rmd file to run.
20. Fig.Path
	+ Location to save each figure.
21. Cache.Path
	+ Location to save caches if any chunks have cahce=TRUE. This is not required but if you are running the script several times for the same solution file, certain chunks take a long time to run and this can be beneficial. Described below.

##### About cache=TRUE

Setting cache=TRUE makes the markdown script save the data before it outputs it into an HTML for an individual chunk. The "key-period-dispatch" and "yearly-curtailment" chunks take a long time to process due to the interval queries (instead of yearly queries) required. With cache=TRUE, the first time still takes the full length to run, but subsequent runs of the script just load the saved cache data. However, if anything in the file or chunk (not sure) changes that has cache=TRUE, it will overwrite the cache and take the full length of time to process. 
	
For "key-period-dispatch", to set cache=TRUE, change ```{r key-period-dispatch}``` to ```{r key-period-dispatch, cache=TRUE}```

For "yearly-curtailment", you must open up the source_scripts/p_yearly_curtailment.Rmd file, and change 
```{r yearly_curtailment, include=TRUE}``` to ```{r yearly_curtailment, include=TRUE, cache=TRUE}```
