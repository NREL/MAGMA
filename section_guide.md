# This guide shows what section corresponds to each number in the input file. Below you can find what individal outputs need to be reported by PLEXOS for that section to successfully run.

1. Total database generation stack
 + 1.1 Total generation difference
2. Total generation by zone stack
 + 2.1 Total generation difference by zone
3. Total generation by region stack
 + 3.1 Total generation difference by region
4. Individual region, total generation stacks
5. Total database specific period generation dispatch plots
6. Zone specific period generation dispatch plots
7. Region specific period generation dispatch plots
8. Average curtailment in each day
 + 8.1 Average daily curtailment difference
9. Average curtailment in each day, by type
 + 9.1 Average daily curtailment difference by type
10. Average curtailment in each interval
 + 10.1 Average interval curtailment difference
11. Average curtailment in each interval, by type
 + 11.1 Average intervail curtailment difference by type
12. Total generation table
 + 12.1 Total generation difference table
13. Total curtailment table
 + 13.1 Total curtailment difference table
14. Total cost table
 + 14.1 Total cost difference table
15. Region and zone info table
 + 15.1 Region and zone difference table
16. Total interface flow
17. Average interval and daily interface flow
 + 17.1 Average interval and daily interface flow difference
18. Specific period interface flow plot
19. Total line flow
20. Average interval and daily line flow
21. Specific period line flow
22. Total reserve provision
 + 22.1 Total reserve provision difference
23. Total reserve shortage
 + 23.1 Total reserve shortage difference
24. Average interval and daily reserve provision
  + 24.1 Average interval and daily reserve provision difference
25. Reserve provision by generation type
  + 25.1 Difference in reserve provision by generation type
26. Region and Zone generation
27. Capacity factors
28. Region price duration curves
29. Reserve price duration curves
30. Day Ahead - Real Time Committment and Dispatch Plots by Zone
31. Day Ahead - Real Time Committment and Dispatch Plots by Region
32. Revenue by generator type
33. Generation comparison by generator type in each zone
34. Generation comparison by generator type in each region
35. Model Run Times
36. Installed Capacity

## Required PLEXOS outputs in report for each section to run:

1. Total Generation Stack
 + Total: 
 + Generator - generation, available energy
 + Region - load
2. Zone Generation Stacks
 + Total: 
 + Generator - generation, available energy
 + Zone - load
3. Regional Generation Stacks
 + Total: 
 + Generator - generation, available energy
 + Region - load
4. Individual Region Generation Stacks
 + Total: 
 + Generator - generation, available energy
 + Region - load
5. - 7. Specific Period Time Series Dispatch Plots
 + Interval: 
 + Generator - generation, available capacity, units generating
 + Region - load, unserved energy
 + Zone - load, unserved energy
8. - 11. Curtailment Plots
 + Interval:
 + Generator - generation, available capacity, units generating
12. Total Generation Table
 + Total:
 + Generator - generation, available energy
13. Total Curtailment Table
 + Total:
 + Generator - generation, available energy
14. Total Cost Table
 + Total:
 + Generator - emissions cost, fuel cost, start and shutdown cost, VO&M cost
15. Region and Zone Info
 + Total:
 + Region - load, imports, exports, unserved energy
 + Zone - load, imports, exports, unserved energy
16. Total Interface Flow
 + Total:
 + Interface - flow
17. Average Daily and Interval Interface Flow
 + Interval:
 + Interface - flow
18. Specific Period Interface Flow Plot
 + Interval:
 + Interface - flow
19. Line Flow Table
 + Total:
 + Line - flow
20. Average Daily and Interval Line Flow
 + Interval:
 + Line - flow
21. Specific Period Line Flow Plot
 + Interval:
 + Line - flow
22. Total Reserves Provision
 + Total:
 + Reserves - provision, shortage
23. Total Reserves Shortage
 + Total:
 + Reserves - provision, shortage
24. Average Daily and Interval Reserves Provision
 + Interval:
 + Reserves - provision
25. Reserves Provision by Generator Type
 + Total:
 + Reserve.Generator - Provision
26. Region and Zone Generation
 + Total:
 + Generator - generation, available energy
 + Region - load
 + Zone - load
27. Capacity Factors
 + Total:
 + Generator - installed capacity
28. Region Price Duration Curves
 + Interval:
 + Region - price
29. Reserves Price Duration Curves
 + Interval:
 + Reserves - price
30. - 31. Day Ahead to Real Time Committment and Dispatch Plots
 + Interval:
 + Generator - generation, available capacity, units generating
 + Region - load, unserved energy
 + Zone - load, unserved energy
32. Revenue by Generator Type
 + Interval:
 + Generator - generation, pump load
 + Region - rice
 + Reserves - price
 + Reserve.Generator - provision
33. - 34. Generation Comparison by Generator Type
 + Interval:
 + Generator - generation, available capacity, units generating
 + Region - load, unserved energy
 + Zone - load, unserved energy
35. Model Run Times
 + Information contained in log file
36. Installed Capacity
 + Total:
 + Generator - installed capacity







