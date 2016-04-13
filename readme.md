#PLEXOS-Vis Readme

This repository is intended as a submodule to a PLEXOS project specific repository. 

## This package creates figures and an HTML file with all of those figures for plexos solutions.
* You must use process_folder() from the rplexos package to create a database file from the PLEXOS solution .zip, before running this code.

##To Run:
1. Copy run_html_output.R and input_data.csv into another folder associated with your project specific repository.
2. Edit input_data.csv to point to the required databases, input files, and generator categories for your project.
3. Edit run_html_output.R to point to your input_data.csv
4. Run: ```source('run_html_output.R')```

### input_data.csv
* Database Location
	+ Location of the .db file created from PLEXOS .zip solution file.
* Using.Gen.Type.Mapping.CSV
	+ If usings a CSV file to assign generation type by generator name, this should be TRUE, else it should be FALSE. Column names should be "name" and "Type"
* Gen.Region.Zone.Mapping.Filename
	+ CSV file which assigns a Region and Zone to each generator. Column names should be "name", "Region", "Zone". This can also be the same file you use to assign generation type.

