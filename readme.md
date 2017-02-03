# MAGMA Readme
## Multi-area Grid Metrics Analyzer

This package creates figures and an HTML file with all of those figures for plexos solutions.
* You must use process_folder() from the rplexos package to create a database file from the PLEXOS solution .zip, before running this code.
* The Examples folder contains templates and examples of the input data CSV, the generator name, region, and zone mapping CSV, the R script that runs the package, and example HTML reports (within the reports folder).
* Section_guide.md lists what sections correspond to each number for the sections to run column in the input data CSV file.

##To Run:
1. Copy run_html_output.R and input_data_template.csv from the Examples/ folder into another folder associated with your project specific repository.
2. Edit input_data.csv according to your model. Some of these values may be defined in run_html_output.R
3. If using a CSV to map generator name to type, create this.
4. If generation type by region or zone is wanted, add region and zone columns to mapping CSV.
5. Edit run_html_output.R to point to your input_data.csv and databases. You may point to multiple databases by listing a vector of database locations. Additionally, you may specify 'scenario.names' to re-name the scenario for each database. Default is the folder name. 
6. Edit run_html_output.R to set html output file name, directory and current working directory where files are located.
7. Run: ```source('run_html_output.R')```

### Notes:
1. If you have trouble with the render function, make sure the "rmarkdown" package is installed. If there are issues locating or sourcing files, make sure the appropriate working directories and paths are setup correctly.
2. You can choose to run individual chunks or the entire package. This is setup in the input_data.csv file. Running the entire package could take a long time, particularly for large solution files and if the solution file has to be processed by rplexos (done the first time MAGMA is run).
3. The name of your database file must start with "Model".
4. Report any problems or issues to Matt O'Connell at NREL. matthew.oconnell@nrel.gov 

##### input_data.csv columns:
1. ref.scenario
	+ Name of reference scenario if comparing multiple solution files. Only used if more than one database is provided
2. PLEXOS.Gen.Category
	+ Categories in PLEXOS database. This must contain all the categories in your PLEXOS database.
	+ Only required if "use.gen.type.csv" or "Using.Gen.Type.Mapping.CSV" is FALSE.
3. PLEXOS.Desired.Type	
	+ Assigns a generation type to each of the PLEXOS categories.
	+ Only required if "use.gen.type.csv" or "Using.Gen.Type.Mapping.CSV" is FALSE.
4. Gen.Type
	+ All types of generation that you either assign through the CSV or PLEXOS category. It can contain more types than you currently have, but it must have all of them otherwise they will be missing from the results.
5. Plot.Color
	+ Assigns a plot color to each generation type
6. Gen.Order
	+ Assigns an order from top to bottom of generation types for dispatch stacks. This list must also contain every generation type that you assign.
7. Renewable.Types.for.Curtailment
	+ Generation types to be considered in curtailment calculations.
8. DA.RT.Plot.Types
	+ Types of generation to show in the committment and dispatch plots.
9. Key.Periods
	+ You can choose individual time spans to plot results for. This column assigns names to those time periods.
10. Start.Time
	+ Corresponding start time for each of the periods in 9.
11. End.Time
	+ Corresponding end time for each of the periods in 9. 
12. Sections.to.Run
	+ Sections (chunks) of the HTML_output.Rmd file to run.
13. Ignore.Zones
 	+ Zones to exclude from plots
14. Ignore.Regions
	+ Regions to exclude from plots
15. Interfaces.for.Flows
	+ Specific interfaces to show flow data for
	+ If all interfaces are desired, list 'ALL' as only entry
16. Lines.for.Flows
	+ Specific lines to show flow data for
	+ If all lines are desired, list 'ALL' as only entry
17. Region.Order
	+ Optional column specifying a certain order for Regions to appear in your plots. 
	+ Defaults to alphabetical
18. Zone.Order
	+ Optional column specifying a certain order for Zones to appear in your plots. 
	+ Defaults to alphabetical
19. Using.Gen.Type.Mapping.CSV
	+ If usings a CSV file to assign generation type by generator name, this should be TRUE, else it should be FALSE. Column names should be "name" and "Type"
	+ If you don't do it this way, it will assign generation type by PLEXOS category.
	+ This variable may be set in run_html_output.R as "use.gen.type.csv"
20. reassign.zones
	+ If you want to recategorize zones (what regions make up what zones) this must be TRUE. Otherwise it can be FALSE and the default PLEXOS zones will be used. 
	+ This variable may be set in run_html_output.R as "reassign.zones"
5. Gen.Region.Zone.Mapping.Filename
	+ CSV file which assigns a Region and Zone to each generator name. 
	+ Column names for generator name, region, and zone must be "name", "Region", "Zone". This can also be the same file you use to assign generation type.
	+ Only needed for region and zone dispatch stacks.
	+ This variable may be set in run_html_output.R as "gen.region.zone"
6. CSV.Gen.Type.File.Location
	+ Location of CSV file to assign generation type by generator name, if using this (Using.Gen.Type.Mapping.CSV == TRUE).
	+ Column names for generator name and type must be "name", and "Type". This can also be the same file you use to assign region and zone.
	+ If "Using.Gen.Type.Mapping.CSV" is FALSE, this field can be left blank.
	+ This variable may be set in run_html_output.R as "gen.type.csv.loc"

## Required PLEXOS outputs

PLEXOS must report the following data in order for all scripts to work. If you try to run a section and are missing the required data you will get an error telling you what PLEXOS output data is missing.

### Annual
##### Generator:
 + Generation
 + Available Energy
 + Emission Cost
 + Fuel Cost
 + Start and Shutdown Cost
 + VO&M Cost
 + Installed Capacity

##### Region:
 + Load
 + Imports
 + Exports
 + Unserved Energy

##### Zone:
 + Load
 + Imports
 + Exports
 + Unserved Energy

##### Reserves:
 + Provision
 + Shortage

##### Interface
 + Flow

### Interval
##### Generator:
 + Generation
 + Available Capacity
 + Units Generating

##### Region:
 + Load
 + Price

##### Zone:
 + Load
 
##### Reserve:
 + Provision

##### Interface
 + Flow
