
library(shiny)

shinyUI(

fluidPage(

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Heading Text ----
  # Create and display heading text
  fluidRow( column( 8, headerPanel('MAGMA Input CSV Creator'), offset = 2 ) 
          ),
  fluidRow( column(4, h5("Example HTML Report:", a("example_html", href="https://github.com/NREL/MAGMA/blob/master/Examples/RTS-2016/reports/HTML_output.html")) ),
            column(4, h3("Sections to Run:", position = 'center') ) 
          ),
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Sections to Run Checkboxes ----
  # Creates the 3 parts of the input object "sectionsToRun1, sectionsToRun2, sectionsToRun3" which tell the rest of the interface what input optinos to show.
  fluidRow(
    column(4, checkboxGroupInput('sectionsToRun1', "", sectionList[1:ceiling(length(sectionList)/3)]) ),
    column(4, checkboxGroupInput('sectionsToRun2', '', sectionList[(ceiling(length(sectionList)/3)+1):(ceiling(length(sectionList)/3)+ceiling(length(sectionList)/3))]) ),
    column(4, checkboxGroupInput('sectionsToRun3', "", sectionList[(ceiling(length(sectionList)/3)+ceiling(length(sectionList)/3)+1):length(sectionList)]) )
  ), 
  
 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # PLEXOS Gen Category Mapping ----
 # This whole section is the list that allows you to reassign generation type from PLEXOS categories to whatever type is desired. 
 # It is only displayed if assigning gen type by plexos category, AND reassign plexos gen category is selected.
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory1', 
                                                                                                                                                     label='PLEXOS Category:') ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType1', 
                                                                                                                                                     label='Generation Type:') ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory2', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType2', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory3', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType3', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory4', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType4', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory5', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType5', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory6', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType6', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory7', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType7', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory8', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType8', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory9', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType9', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory10', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType10', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory11', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType11', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory12', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType12', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory13', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType13', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory14', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType14', label=NULL) ) ) ),
 fluidRow( column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('plexosCategory15', label=NULL) ) ),
           column(4, conditionalPanel( condition = "input.reassignPlexosGenTypes == 1 && input.genTypeMapping == 2 && output.genType", textInput('genType15', label=NULL) ) ) ),

 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # Order and Color of Generation for Stacks ----
 # This section sets the order of generation type for the dispatch stacks as well as the plot color for the stacks.
 # It also asks if each generation type should be considered for curtailment calculations, and if selected, asks if each type should be shown in the DA-RT plots.
 # Only show genStackPlot if the sections that produce a generation dispatch stack are selected.
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", h4('Generation Stack Order (Top to Bottom):'))), 
           column(3, conditionalPanel( condition = "output.genStackPlot", h4('Generation Type Plot Color:'))), 
           column(3, conditionalPanel( condition = "output.genStackPlot", h4('Consider as RE for curtailment calculation?'))), 
           column(3, conditionalPanel( condition = "output.DA_RT", h4('Show in DA-RT committment/dispatch plots?'))) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder1', label=NULL, value='Curtailment') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor1', choices=plotColors, label=NULL, selected='red') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType1', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type1', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder2', label=NULL, value='PV') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor2', choices=plotColors, label=NULL, selected='goldenrod1') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType2', label=NULL, value=TRUE) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type2', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder3', label=NULL, value='CSP') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor3', choices=plotColors, label=NULL, selected='darkorange2') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType3', label=NULL, value=TRUE) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type3', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder4', label=NULL, value='Wind') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor4', choices=plotColors, label=NULL, selected='steelblue3') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType4', label=NULL, value=TRUE) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type4', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder5', label=NULL, value='Storage') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor5', choices=plotColors, label=NULL, selected='gray45') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType5', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type5', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder6', label=NULL, value='Other') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor6', choices=plotColors, label=NULL, selected='mediumpurple3') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType6', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type6', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder7', label=NULL, value='Geothermal') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor7', choices=plotColors, label=NULL, selected='khaki1') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType7', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type7', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder8', label=NULL, value='Gas CT') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor8', choices=plotColors, label=NULL, selected='lightpink') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType8', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type8', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder9', label=NULL, value='Gas CC') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor9', choices=plotColors, label=NULL, selected='darkolivegreen4') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType9', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type9', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder10', label=NULL, value='Hydro') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor10', choices=plotColors, label=NULL, selected='lightblue') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType10', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type10', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder11', label=NULL, value='Coal') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor11', choices=plotColors, label=NULL, selected='gray20') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType11', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type11', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder12', label=NULL, value='Nuclear') ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor12', choices=plotColors, label=NULL, selected='firebrick') ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType12', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type12', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder13', label=NULL) ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor13', choices=plotColors, label=NULL) ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType13', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type13', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder14', label=NULL) ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor14', choices=plotColors, label=NULL) ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType14', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type14', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder15', label=NULL) ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor15', choices=plotColors, label=NULL) ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType15', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type15', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder16', label=NULL) ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor16', choices=plotColors, label=NULL) ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType16', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type6', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder17', label=NULL) ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor17', choices=plotColors, label=NULL) ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType17', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type17', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder18', label=NULL) ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor18', choices=plotColors, label=NULL) ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType18', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type18', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder19', label=NULL) ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor19', choices=plotColors, label=NULL) ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType19', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type19', label=NULL) ), offset=2 ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.genStackPlot", textInput('genOrder20', label=NULL) ) ),
           column(3, conditionalPanel( condition = "output.genStackPlot", selectInput('genTypeColor20', choices=plotColors, label=NULL) ) ),
           column(1, conditionalPanel( condition = "output.genStackPlot", checkboxInput('reType20', label=NULL) ), offset=1 ),
           column(1, conditionalPanel( condition = "output.DA_RT",        checkboxInput('DA_RT_Type20', label=NULL) ), offset=2 ) ),
 
 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # Key Period Sections ----
 # If any sections involving key periods are selected, this list allows input of key period names and start/end times.
 fluidRow( column(3, conditionalPanel( condition = "output.keyPeriodPlots", h4('Key Period Name:')) ),
           column(5, conditionalPanel( condition = "output.keyPeriodPlots", h4('Period Date Range:')) ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.keyPeriodPlots", textInput('keyPeriodName1', label=NULL, value='Winter Week')) ), 
           column(5, conditionalPanel( condition = "output.keyPeriodPlots", dateRangeInput('keyPeriodRange1', label=NULL, startview='decade', start='2030-01-01', end='2030-01-07')) ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.keyPeriodPlots", textInput('keyPeriodName2', label=NULL, value='Spring Week')) ), 
           column(5, conditionalPanel( condition = "output.keyPeriodPlots", dateRangeInput('keyPeriodRange2', label=NULL, startview='decade', start='2030-04-01', end='2030-04-07')) ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.keyPeriodPlots", textInput('keyPeriodName3', label=NULL, value='Summer Week')) ), 
           column(5, conditionalPanel( condition = "output.keyPeriodPlots", dateRangeInput('keyPeriodRange3', label=NULL, startview='decade', start='2030-07-01', end='2030-07-07')) ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.keyPeriodPlots", textInput('keyPeriodName4', label=NULL, value='Fall Week')) ),
           column(5, conditionalPanel( condition = "output.keyPeriodPlots", dateRangeInput('keyPeriodRange4', label=NULL, startview='decade', start='2030-10-01', end='2030-10-07')) ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.keyPeriodPlots", textInput('keyPeriodName5', label=NULL)) ), 
           column(5, conditionalPanel( condition = "output.keyPeriodPlots", dateRangeInput('keyPeriodRange5', label=NULL, startview='decade')) ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.keyPeriodPlots", textInput('keyPeriodName6', label=NULL)) ), 
           column(5, conditionalPanel( condition = "output.keyPeriodPlots", dateRangeInput('keyPeriodRange6', label=NULL, startview='decade')) ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.keyPeriodPlots", textInput('keyPeriodName7', label=NULL)) ), 
           column(5, conditionalPanel( condition = "output.keyPeriodPlots", dateRangeInput('keyPeriodRange7', label=NULL, startview='decade')) ) ),
 fluidRow( column(3, conditionalPanel( condition = "output.keyPeriodPlots", textInput('keyPeriodName8', label=NULL)) ), 
           column(5, conditionalPanel( condition = "output.keyPeriodPlots", dateRangeInput('keyPeriodRange8', label=NULL, startview='decade')) ) ),
 
 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # Exclude regions or zones from plots ----
 # If generation stacks are going to be created, these check boxes allow the user to select regions or zones to be exluded from the plots. 
 fluidRow( column(4, conditionalPanel( condition = "output.genStackPlot", checkboxInput('ignoreZones', label="Exclude any zones from generation stack plots?")) ),
           column(4, conditionalPanel( condition = "output.genStackPlot", checkboxInput('ignoreRegions', label="Exclude any regions from generation stack plots?")) ) ),
 fluidRow( column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreZones == 1", h5('List zone names to exclude:')) ), 
           column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreRegions == 1", h5('List region names to exclude:')) ) ),
 fluidRow( column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreZones == 1", textInput('ignoreZone1', label=NULL)) ), 
           column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreRegions == 1", textInput('ignoreRegion1', label=NULL)) ) ),
 fluidRow( column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreZones == 1", textInput('ignoreZone2', label=NULL)) ), 
           column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreRegions == 1", textInput('ignoreRegion2', label=NULL)) ) ),
 fluidRow( column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreZones == 1", textInput('ignoreZone3', label=NULL)) ), 
           column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreRegions == 1", textInput('ignoreRegion3', label=NULL)) ) ),
 fluidRow( column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreZones == 1", textInput('ignoreZone4', label=NULL)) ),
           column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreRegions == 1", textInput('ignoreRegion4', label=NULL)) ) ),
 fluidRow( column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreZones == 1", textInput('ignoreZone5', label=NULL)) ), 
           column(4, conditionalPanel( condition = "output.genStackPlot && input.ignoreRegions == 1", textInput('ignoreRegion5', label=NULL)) ) ),
 
 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # Interfaces to query ----
 # If sections involving interface data are selected, this list allows the user to input the interface names they want data for.
 fluidRow( column(5, conditionalPanel( condition = "output.interfacePlots", textInput('interface1', label='Interfaces to create results for:')) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.interfacePlots", textInput('interface2', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.interfacePlots", textInput('interface3', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.interfacePlots", textInput('interface4', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.interfacePlots", textInput('interface5', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.interfacePlots", textInput('interface6', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.interfacePlots", textInput('interface7', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.interfacePlots", textInput('interface8', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.interfacePlots", textInput('interface9', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.interfacePlots", textInput('interface10', label=NULL)) ) ),
 
 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # Lines to query ----
 # If sections involving line data are selected, this list allows the user to input the line names they want data for.
 fluidRow( column(5, conditionalPanel( condition = "output.linePlots", textInput('line1', label='Lines to create results for:')) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.linePlots", textInput('line2', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.linePlots", textInput('line3', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.linePlots", textInput('line4', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.linePlots", textInput('line5', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.linePlots", textInput('line6', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.linePlots", textInput('line7', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.linePlots", textInput('line8', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.linePlots", textInput('line9', label=NULL)) ) ),
 fluidRow( column(5, conditionalPanel( condition = "output.linePlots", textInput('line10', label=NULL)) ) ),
 
 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # Generation to consider curtailment ----
 # If sections involving curtailment are selected but none that produce generation stacks, this list asks what type of generation should be considered as curtailment.
 fluidRow( column(4, conditionalPanel( condition = "output.curtailmentCalcs && output.genStackPlot == false", 
                                       textInput('reType1', label='Renewable types for curtailment calculations:', value='PV')) ) ),
 fluidRow( column(4, conditionalPanel( condition = "output.curtailmentCalcs && output.genStackPlot == false", textInput('reType2', label=NULL, value='CSP')) ) ),
 fluidRow( column(4, conditionalPanel( condition = "output.curtailmentCalcs && output.genStackPlot == false", textInput('reType3', label=NULL, value='Wind')) ) ),
 fluidRow( column(4, conditionalPanel( condition = "output.curtailmentCalcs && output.genStackPlot == false", textInput('reType4', label=NULL)) ) ),
 fluidRow( column(4, conditionalPanel( condition = "output.curtailmentCalcs && output.genStackPlot == false", textInput('reType5', label=NULL)) ) ),

 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # Reference Scenario ----
 # Option to enter reference scenario name
 fluidRow( column(3, checkboxInput('referenceBox', label = 'Comparing Multiple Scenarios?' ) ),
           column(3, conditionalPanel( condition = "input.referenceBox == 1", textInput('referenceName', label = 'Reference Scenario Name' ) ) ),
           column(3, conditionalPanel( condition = "input.referenceBox == 1", numericInput('numScenarios', label = 'Number of Scenarios', value=1) ) )
           ),
 
 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # Scenario Name(s) ----
 # Names for scenarios in HTML Report
 fluidRow( column(2, textInput('scenario1', label = 'Scenario Name' ) ),
           column(2, conditionalPanel( condition = "input.numScenarios > 1", textInput('scenario2', label = "Scenario 2 Name" ) ) ),
           column(2, conditionalPanel( condition = "input.numScenarios > 2", textInput('scenario3', label = "Scenario 3 Name" ) ) ),
           column(2, conditionalPanel( condition = "input.numScenarios > 3", textInput('scenario4', label = "Scenario 4 Name" ) ) ),
           column(2, conditionalPanel( condition = "input.numScenarios > 4", textInput('scenario5', label = "Scenario 5 Name" ) ) ),
           column(2, conditionalPanel( condition = "input.numScenarios > 5", textInput('scenario6', label = "Scenario 6 Name" ) ) )
           ),
 
 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # Create CSV file button ----
 # Button to create the input CSV file. 
 fluidRow( column(4, textInput('csvLocation', label='Directory to save input CSV:') ) ), 
 fluidRow( column(4, actionButton('createCSV', label='Create input CSV file.') ) )

))


