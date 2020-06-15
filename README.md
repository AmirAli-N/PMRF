# PMRF
Performance Measures for Maintenance of Roadside Features

1-amend_odometer(OdometrMarker).R\
  fix odometer markers in order of postmiles

2-bind_df(LEMO_WorkOrder, LCS).R\
  clean up LEMO, Work Order, and Lane Closure System data sets. Merge LEMO and Work Order. Print for conversion of postmiles to odometer
  
3-bind_files(AADT, Truck).R\
  row bind AADT and Truck AADT data sets for different years into a single dataset
  
4-bind_files(LCS).R\
5-bind_files(LEMO).R\
6-bind_files(SWITRS).R\
7-bind_files(WorkOrder).R

8-bind_odometer_geocode(SWITRS).R\
  adding odometer and coordinate values to the SWITRS database
  
9-bind_odometer(LEMO_WorkOrder_LCS_AADT_TRUCK).R\
10-bind_odometer(LEMO_WorkOrder_LCS).R

11-Classification_decisionTrees(SWITRS_wzOnly).R\
  Classification code implementing decision tree for feature selection on SWITRS data set for work zone collisions
  
12-Classification_ElasticNet(LogReg).py\
13-Classification_ElasticNet(Multinomial).R\
14-Classification_RFE.py\
15-Classification_REF(LogReg).R\
16-Classification_RFE(Multinomial).R\
17-Classification_RFE(Ordinal).R\
18-Classification_xgBoost(binary).R\
19-Classification_xgBoost(Multiclass).R

17-Extract_pdfTable(IMMS).R\
  extract activity tables from the IMMS pdf files

18-FUNC_clean(FinalDataSet).R\
  function to clean up the final joint data set

19-FUNC_match(LEMO_WorkOrder_LCS, AADT, TRUCK).R\
  function to match LEMO_WorkOrder_LCS data set with AADT and TRUCK AADT data sets based on route, odometer and date
  
20-FUNC_match(LEMO_WorkOrder, LCS).R\
  funcion to match LEMO_WorkOrder data set with Lane Closure System based on route, odometer, and date
  
21-generate_collisionDensity(SWITRS).R\
  generate collision densities for every 2 mile segment of each road using all collisions between 2011-2018
  
22-generate_testData(FinalDataSets).R\
  generate test data set for the classification modesl
  
23-HeatMap(SWITRS_wzOnlyRuralPop).py\
  generate a heatmap on Google Maps showing the density of work zone collisions by population code
  
24-MATCH(FinalDataSets).R\
  merge all the data sets, LEMO, WorkOrder, LCS, AADT, TRUCK AADT, CleanRouteFile based on matching keys
  
25-MATCH(LEMO_WorkOrder, CleanRouteFile).R\
  extract matchink keys for LEMO_WorkOrder and CleanRouteFile data set based on location

25-MATCH(LEMO_WorkOrder, ClosureMatches).R\
26-MATCH(LEMO_WorkOrder, collisionDensity).R\
27-MATCH(SWITRS, CleanRouteFile).R\
28-MATCH(SWITRS, WorkOrder).R\
29-MATCH(LCS, CHP).R

30-Pipeline(LEMO_WorkOrder_LCS_AADT_TRUCK).R\
  a pipeline to match LEMO_WorkOrder, LCS, AADT, and TRUCK AADT calling appropriate MATCH functions

31-pm_odom_geocode_query.py\
  script to define HTTP request for Caltrans API to convert postmiles to coordinates
  
32-pm_odom_query.py\
  script to define HTTP request for Caltrans API to convert postmiles to odometers
  
33-pm_submit_queries.py\
  a script to build xml request and translate xml responses

34-print_forOdometerConversion(CleanRouteFile).R\
  print the neccessary fields of CleanRouteFile for odometer conversion
  
35-print_forOdometerConversion(SWITRS).R\
36-Sankey(IMMS Crew classification).R\
  generate a sankey diagram for IMMS and Crew classification
  
37-STAT(LEMO_WorkOrder_LCS).R\
  generating some highlevel statistical observation of the joint LEMO_WorkOrder_LCS dataset
  
38-STAT(LEMO).R\
39-STAT(SWITRS_CleanRouteFeatures).R\
40-STAT(SWITRS_wzOnly_LEMO_LCS_AADT).R\
41-STAT(SWITRS_wzOnly)\
42-STAT(SWITRS).R

NOTE: The files are not numbered in order of operation