# This guide shows what section corresponds to each number in the input file. Below you can find what individal outputs need to be reported by PLEXOS for that section to successfully run.

1. Total Database Generation Stack**
 + 1.1 Total Generation Difference
2. **Total Generation by Zone Stack**
 + 2.1 Total Generation Difference by Zone
3. **Total Generation by Region Stack**
 + 3.1 Total Generation Difference by Region
4. **Individual Region, Total Generation Stacks**
5. **Total Database Specific Period Generation Dispatch Plots**
6. **Zone Specific Period Generation Dispatch Plots**
7. **Region Specific Period Generation Dispatch Plots**
8. **Average Curtailment in each day**
 + 8.1 Average Daily Curtailment Difference
9. **Average Curtailment in each day, by Type**
 + 9.1 Average Daily curtailment Difference by Type
10. **Average curtailment in each Interval**
 + 10.1 Average Interval Curtailment Difference
11. **Average Curtailment in each Interval, by Type**
 + 11.1 Average Intervail Curtailment Difference by Type
12. **Total Generation Table**
 + 12.1 Total Generation Difference Table
13. **Total Curtailment Table**
 + 13.1 Total Curtailment Difference Table
14. **Total Cost Table**
 + 14.1 Total Cost Difference Table
15. **Region and Zone Info Table**
 + 15.1 Region and Zone Difference Table
16. **Total Interface Flow**
17. **Average Interval and Daily Interface Flow**
 + 17.1 Average Interval and Daily Interface Flow Difference
18. **Specific Period Interface Flow Plot**
19. **Total Line Flow**
20. **Average Interval and Daily Line Flow**
21. **Specific Period Line Flow**
22. **Total Reserve Provision**
 + 22.1 Total Reserve Provision Difference
23. **Total Reserve Shortage**
 + 23.1 Total Reserve Shortage Difference
24. **Average Interval and Daily Reserve Provision**
  + 24.1 Average Interval and Daily Reserve Provision Difference
25. **Reserve Provision by Generation Type**
  + 25.1 Difference in Reserve Provision by Generation Type
26. **Region and Zone Generation**
27. **Capacity Factors**
28. **Region Price Duration Curves**
29. **Reserve Price Duration Curves**
30. **Day Ahead - Real Time Committment and Dispatch Plots by Zone**
31. **Day Ahead - Real Time Committment and Dispatch Plots by Region**
32. **Revenue by Generator Type**
33. **Generation Comparison by Generator Type in Each Zone**
34. **Generation Comparison by Generator Type in Each Region**
35. **Model Run Times**
36. **Installed Capacity**

## Required PLEXOS outputs in report for each section to run:

1. **Total Generation Stack**
 + **Total:** 
 + Generator - generation, available energy
 + Region - load
2. **Zone Generation Stacks**
 + **Total:**
 + Generator - generation, available energy
 + Zone - load
3. **Regional Generation Stacks**
 + **Total:**
 + Generator - generation, available energy
 + Region - load
4. **Individual Region Generation Stacks**
 + **Total:** 
 + Generator - generation, available energy
 + Region - load
5. - 7. **Specific Period Time Series Dispatch Plots**
 + **Interval:**
 + Generator - generation, available capacity, units generating
 + Region - load, unserved energy
 + Zone - load, unserved energy
8. - 11. **Curtailment Plots**
 + **Interval:**
 + Generator - generation, available capacity, units generating
12. **Total Generation Table**
 + **Total:**
 + Generator - generation, available energy
13. **Total Curtailment Table**
 + **Total:**
 + Generator - generation, available energy
14. **Total Cost Table**
 + **Total:**
 + Generator - emissions cost, fuel cost, start and shutdown cost, VO&M cost
15. **Region and Zone Info**
 + **Total:**
 + Region - load, imports, exports, unserved energy
 + Zone - load, imports, exports, unserved energy
16. **Total Interface Flow**
 + **Total:**
 + Interface - flow
17. **Average Daily and Interval Interface Flow**
 + **Interval:**
 + Interface - flow
18. **Specific Period Interface Flow Plot**
 + **Interval:**
 + Interface - flow
19. **Line Flow Table**
 + **Total:**
 + Line - flow
20. **Average Daily and Interval Line Flow**
 + **Interval:**
 + Line - flow
21. **Specific Period Line Flow Plot**
 + **Interval:**
 + Line - flow
22. **Total Reserves Provision**
 + **Total:**
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







