import os
os.chdir("//ahmct-065/teams/PMRF/Amir/Codes")
import pm_odom_query
import pm_odom_geocode_query
import pandas as pd
import numpy as np
import math
from time import sleep
from socket import gethostbyname, gaierror


# define function to call travis's script `pm_odom_geocode_query` looking for the function `locate_pm` which prepairs a xml request for caltrans servers and parses the response xml convering postmiles to geo coordinates 

def geocode_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, alignment):
    geocode=pm_odom_geocode_query.locate_pm(cty, rtnum, rtsfx, pmpfx, pmval, alignment)
    if (geocode is None):
        return None
    return geocode


# # Data Preparation
# 1. set of postmile prefix, route suffix, and postmile suffix
# 2. reset the path to read data files, and read the data
# 3. survey column names, and filter for the desired column
# 4. Look how many accidents do not have `(lat, long)` pair. For those, we may need to convert postmile information to geocoordinates
# 5. If neccessary, partition the data into separate dataframes based on existance of geocoordinates
# 6. If neccessary, convert postmile information to geocoordinates
# 7. We are looking for accidents in rural (unincorporated) areas in `POPULATION` column
# 8. `COLLISION_SEVERITY` is classified into two groups: `Fatality or symptomatic injury` and `PDO` (property damage only)
# 8. If neccessary, the partitioned data bases are joined together

# Initialize a dictionary for each `county_code`. County codes, PeMS code (odd number codes), and county abbreviations are stored in `\\AHMCT-065\teams\PMRF\Amir\County_Abbr_PeMS.code.csv`. Initialize a list of possible postmile prefixes, route suffixes, and alignments, and reset the path in order to read the collision data

FIPS_CTY = {
      1:'ALA',   3:'ALP',   5:'AMA',   7:'BUT',   9:'CAL',  11:'COL',  13:'CC',   15:'DN',
     17:'ED',   19:'FRE',  21:'GLE',  23:'HUM',  25:'IMP',  27:'INY',  29:'KER',  31:'KIN',
     33:'LAK',  35:'LAS',  37:'LA',   39:'MAD',  41:'MRN',  43:'MPA',  45:'MEN',  47:'MER',
     49:'MOD',  51:'MNO',  53:'MON',  55:'NAP',  57:'NEV',  59:'ORA',  61:'PLA',  63:'PLU',
     65:'RIV',  67:'SAC',  69:'SBT',  71:'SBD',  73:'SD',   75:'SF',   77:'SJ',   79:'SLO',
     81:'SM',   83:'SB',   85:'SCL',  87:'SCR',  89:'SHA',  91:'SIE',  93:'SIS',  95:'SOL',
     97:'SON',  99:'STA', 101:'SUT', 103:'TEH', 105:'TRI', 107:'TUL', 109:'TUO', 111:'VEN',
    113:'YOL', 115:'YUB'
}
pmpfx_lst=['R', 'M', 'C', 'D', 'G', 'H', 'L', 'N', 'S', 'T']
rtsfx_lst=['U', 'S']
align_lst=['R', 'L']
os.chdir("//ahmct-065/teams/PMRF/Amir/bin")
chp_df=pd.read_csv("./CHP.csv", low_memory=False)

# Survey `chp_df` columns to identify useless columns and drop them

chp_df.columns

# Partition the data into two dataframes: `chp_coord` filters the data for rows whose `LATITUDE` and `LONGITUDE` exist. `chp_NaNcoord` filters the data for rows whose `LATITUDE` or `LONGITUDE` does not exist.  

chp_coord=chp_df[chp_df['LATITUDE'].notnull() & chp_df['LONGITUDE'].notnull()]
chp_NaNcoord=chp_df[chp_df['LATITUDE'].isnull() | chp_df['LONGITUDE'].isnull()]
chp_NaNcoord.shape

# For `chp_coord` we only need `CASE_ID`, `COLLISION_SEVERITY`, `POPULATION`, `LATITUDE`, and `LONGITUDE` columns. For `chp_NaNcoord`, in addition to `CASE_ID`, `COLLISION_SEVERITY`, and `POPULATION` columns, we also need the postmile columns, i.e.,`CALTRANS_COUNTY`, `STATE_ROUTE`, `ROUTE_SUFFIX`, `POSTMILE_PREFIX`, `POSTMILE`, and `SIDE_OF_HWY`.

coord_uselessColumns=['ACCIDENT_YEAR', 'COLLISION_DATE', 'COLLISION_TIME',
       'DAY_OF_WEEK', 'CNTY_CITY_LOC', 'SPECIAL_COND',
       'PRIMARY_RD', 'SECONDARY_RD', 'DISTANCE', 'DIRECTION', 'INTERSECTION',
       'WEATHER_1', 'WEATHER_2', 'STATE_HWY_IND', 'CALTRANS_COUNTY',
       'CALTRANS_DISTRICT', 'STATE_ROUTE', 'ROUTE_SUFFIX', 'POSTMILE_PREFIX',
       'POSTMILE', 'LOCATION_TYPE', 'RAMP_INTERSECTION', 'SIDE_OF_HWY',
       'TOW_AWAY', 'NUMBER_KILLED', 'NUMBER_INJURED',
       'PARTY_COUNT', 'PRIMARY_COLL_FACTOR', 'PCF_CODE_OF_VIOL',
       'PCF_VIOL_CATEGORY', 'PCF_VIOLATION', 'PCF_VIOL_SUBSECTION',
       'HIT_AND_RUN', 'TYPE_OF_COLLISION', 'MVIW', 'PED_ACTION',
       'ROAD_SURFACE', 'ROAD_COND_1', 'ROAD_COND_2', 'LIGHTING',
       'CONTROL_DEVICE', 'CHP_ROAD_TYPE', 'PEDESTRIAN_ACCIDENT',
       'BICYCLE_ACCIDENT', 'MOTORCYCLE_ACCIDENT', 'TRUCK_ACCIDENT',
       'NOT_PRIVATE_PROPERTY', 'ALCOHOL_INVOLVED', 'STWD_VEHTYPE_AT_FAULT',
       'CHP_VEHTYPE_AT_FAULT', 'COUNT_SEVERE_INJ', 'COUNT_VISIBLE_INJ',
       'COUNT_COMPLAINT_PAIN', 'COUNT_PED_KILLED', 'COUNT_PED_INJURED',
       'COUNT_BICYCLIST_KILLED', 'COUNT_BICYCLIST_INJURED', 'COUNT_MC_KILLED',
       'COUNT_MC_INJURED', 'PRIMARY_RAMP', 'SECONDARY_RAMP']
chp_coord.drop(coord_uselessColumns, axis=1, inplace=True)
NaNcoord_uselessColumns=['ACCIDENT_YEAR', 'COLLISION_DATE', 'COLLISION_TIME',
       'DAY_OF_WEEK', 'CNTY_CITY_LOC', 'SPECIAL_COND',
       'PRIMARY_RD', 'SECONDARY_RD', 'DISTANCE', 'DIRECTION', 'INTERSECTION',
       'WEATHER_1', 'WEATHER_2', 'STATE_HWY_IND', 'CALTRANS_DISTRICT',
       'LOCATION_TYPE', 'RAMP_INTERSECTION', 'TOW_AWAY',
       'NUMBER_KILLED', 'NUMBER_INJURED',
       'PARTY_COUNT', 'PRIMARY_COLL_FACTOR', 'PCF_CODE_OF_VIOL',
       'PCF_VIOL_CATEGORY', 'PCF_VIOLATION', 'PCF_VIOL_SUBSECTION',
       'HIT_AND_RUN', 'TYPE_OF_COLLISION', 'MVIW', 'PED_ACTION',
       'ROAD_SURFACE', 'ROAD_COND_1', 'ROAD_COND_2', 'LIGHTING',
       'CONTROL_DEVICE', 'CHP_ROAD_TYPE', 'PEDESTRIAN_ACCIDENT',
       'BICYCLE_ACCIDENT', 'MOTORCYCLE_ACCIDENT', 'TRUCK_ACCIDENT',
       'NOT_PRIVATE_PROPERTY', 'ALCOHOL_INVOLVED', 'STWD_VEHTYPE_AT_FAULT',
       'CHP_VEHTYPE_AT_FAULT', 'COUNT_SEVERE_INJ', 'COUNT_VISIBLE_INJ',
       'COUNT_COMPLAINT_PAIN', 'COUNT_PED_KILLED', 'COUNT_PED_INJURED',
       'COUNT_BICYCLIST_KILLED', 'COUNT_BICYCLIST_INJURED', 'COUNT_MC_KILLED',
       'COUNT_MC_INJURED', 'PRIMARY_RAMP', 'SECONDARY_RAMP', 'LATITUDE', 'LONGITUDE']
chp_NaNcoord.drop(NaNcoord_uselessColumns, axis=1, inplace=True)

# Dropping those rows in `chp_NaNcoord` for which `STATE_ROUTE`, `POSTMILE`, and `CALTRANS_COUNTY` does not exist. Also, filter both data base for rural areas in `POPULATION` column

chp_NaNcoord=chp_NaNcoord[(chp_NaNcoord['STATE_ROUTE'].notnull()) & 
                          (chp_NaNcoord['POSTMILE'].notnull()) & 
                          (chp_NaNcoord['CALTRANS_COUNTY'].notnull()) &
                          (chp_NaNcoord['POPULATION']==9)]
chp_coord=chp_coord[chp_coord['POPULATION']==9]

# Classify `COLLISION_SEVERITY` into two groups: `PDO` and `Fatality or injury`

chp_coord.COLLISION_SEVERITY=np.where(chp_coord['COLLISION_SEVERITY']==0, 'PDO', chp_coord['COLLISION_SEVERITY'])
chp_coord.COLLISION_SEVERITY=np.where((chp_coord['COLLISION_SEVERITY']!=0) & 
                                      (chp_coord['COLLISION_SEVERITY']!='PDO'),
                                      'Fatality or injury', chp_coord['COLLISION_SEVERITY'])

chp_NaNcoord.COLLISION_SEVERITY=np.where(chp_NaNcoord['COLLISION_SEVERITY']==0, 'PDO', chp_NaNcoord['COLLISION_SEVERITY'])
chp_NaNcoord.COLLISION_SEVERITY=np.where((chp_NaNcoord['COLLISION_SEVERITY']!=0) & 
                                         (chp_NaNcoord['COLLISION_SEVERITY']!='PDO'),
                                         'Fatality or injury', chp_NaNcoord['COLLISION_SEVERITY'])

# Convert `chp_NaNcoord` postmiles to geo coordinates. In the for loop below, first type and format of the postmile information is checked according to `pm_odom_geocode_query` script. Then, a request is made (trying to catch possible errors) for the given information. If no result is returned, and postmile prefix or alignment information is missing, we try again with every possible combination. However, we first try for alignment, then for postmile prefix, and finally for combination of postmile prefix and alignment.

for row in chp_NaNcoord.itertuples():
    cty=row.CALTRANS_COUNTY
    try:
        cty=FIPS_CTY[2*cty-1]
    except (KeyError, TypeError):
        continue
    rtnum=row.STATE_ROUTE
    try:
        rtnum=int(rtnum)
    except (TypeError, ValueError):
        continue
    rtsfx=row.ROUTE_SUFFIX
    if rtsfx not in rtsfx_lst:
        rtsfx=None
    pmpfx=row.POSTMILE_PREFIX
    if pmpfx not in pmpfx_lst:
        pmpfx=None
    pmval=row.POSTMILE
    try:
        pmval=float(pmval)
    except (NameError, ValueError, TypeError):
        continue
    alignment=row.SIDE_OF_HWY
    if alignment not in align_lst:
        alignment=None
    
    res=None
    try:
        res=geocode_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, alignment)
        if res is not None:
            sleep(0.05)
            chp_NaNcoord.at[row.Index, 'LATITUDE']=res.latdeg
            chp_NaNcoord.at[row.Index, 'LONGITUDE']=res.londeg
    except (gaierror, TimeoutError, ConnectionResetError, OSError):
        sleep(30)
        res=geocode_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, alignment)
        if res is not None:
            chp_NaNcoord.at[row.Index, 'LATITUDE']=res.latdeg
            chp_NaNcoord.at[row.Index, 'LONGITUDE']=res.londeg
    if res is None and alignment is None:
        for try_alignment in align_lst:
            try:
                res=geocode_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, try_alignment)
                sleep(0.05)
                if res is not None:
                    chp_NaNcoord.at[row.Index, 'LATITUDE']=res.latdeg
                    chp_NaNcoord.at[row.Index, 'LONGITUDE']=res.londeg
                    break
            except (gaierror, TimeoutError, ConnectionResetError, OSError):
                sleep(30)
                res=geocode_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, try_alignment)
                if res is not None:
                    chp_NaNcoord.at[row.Index, 'LATITUDE']=res.latdeg
                    chp_NaNcoord.at[row.Index, 'LONGITUDE']=res.londeg
                    break
    if res is None and pmpfx is None:
        for try_pmpfx in pmpfx_lst:
            try:
                res=geocode_from_pm(cty, rtnum, rtsfx, try_pmpfx, pmval, alignment)
                sleep(0.05)
                if res is not None:
                    chp_NaNcoord.at[row.Index, 'LATITUDE']=res.latdeg
                    chp_NaNcoord.at[row.Index, 'LONGITUDE']=res.londeg
                    break
            except (gaierror, TimeoutError, ConnectionResetError, OSError):
                sleep(30)
                res=geocode_from_pm(cty, rtnum, rtsfx, try_pmpfx, pmval, alignment)
                if res is not None:
                    chp_NaNcoord.at[row.Index, 'LATITUDE']=res.latdeg
                    chp_NaNcoord.at[row.Index, 'LONGITUDE']=res.londeg
                    break
            if res is None and alignment is None:
                for try_alignment in align_lst:
                    try:
                        res=geocode_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, try_alignment)
                        sleep(0.05)
                        if res is not None:
                            chp_NaNcoord.at[row.Index, 'LATITUDE']=res.latdeg
                            chp_NaNcoord.at[row.Index, 'LONGITUDE']=res.londeg
                            break
                    except (gaierror, TimeoutError, ConnectionResetError, OSError):
                        sleep(30)
                        res=geocode_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, try_alignment)
                        if res is not None:
                            chp_NaNcoord.at[row.Index, 'LATITUDE']=res.latdeg
                            chp_NaNcoord.at[row.Index, 'LONGITUDE']=res.londeg
                            break


# Join `chp_coord` and `chp_NaNcoord` by rows dropping the postmile information from the `chp_NaNcoord` dataframe. Also, note that the `LONGITUDE` information in `chp_coord` is positive which is not possible for California. 

chp_NaNcoord.drop(['CALTRANS_COUNTY', 'STATE_ROUTE', 'ROUTE_SUFFIX',
                   'POSTMILE_PREFIX', 'POSTMILE', 'SIDE_OF_HWY'], axis=1, inplace=True)
chp_coord.LONGITUDE=-1*chp_coord.LONGITUDE

chp_summary=pd.concat([chp_coord, chp_NaNcoord], axis=0)
chp_summary.to_csv("chp_summary.csv", sep=",")

# # Plot a heatmap on using Google maps heatmap layer
# Use the `gmaps` library which has implemented some of the `google maps java script api` functionality including `heatmaps`. Read the summarised data for plotting heatmap without the weights. For a heatmap including weights, read the original `CHP.csv`file as well. 

import gmaps
gmaps.configure(api_key="AIzaSyA7YArDVXUXhgJjCybDBsVCpXeYm7epACg")

import os
import pandas as pd
import numpy as np
os.chdir("//ahmct-065/teams/PMRF/Amir/bin")
chp_summary=chp_df=pd.read_csv("./chp_summary.csv")
chp_summary=chp_summary.dropna(axis=0)

# The function below switches the severity so that an ordered score corresponds with severity

def severity_weights(severity):
    switcher={
        0: 0,
        1: 4,
        2: 3,
        3: 2,
        4: 1,
    }
    return switcher[severity]

coordinate_lst=list(zip(chp_summary.LATITUDE, chp_summary.LONGITUDE))
weights=chp_df[chp_df.CASE_ID.isin(chp_summary.CASE_ID)].COLLISION_SEVERITY
sorted_weight=[severity_weights(i) for i in weights]

# Declare a google map object, add a heatmap layer and tune the parameters. Note that `heatmap_layer.gradient` changes the color. The default is from green to red.

get_ipython().run_line_magic('matplotlib', 'inline')
m=gmaps.Map()
heatmap_layer=gmaps.heatmap_layer(locations=coordinate_lst, weights=sorted_weight)
heatmap_layer.max_intensity = 100
heatmap_layer.point_radius = 7
heatmap_layer.opacity=1
heatmap_layer.gradient = [
    (0, 255, 255, 0),
    (0, 255, 255, 1),
    (0, 191, 255, 1),
    (0, 127, 255, 1),
    (0, 63, 255, 1),
    (0, 0, 255, 1),
    (0, 0, 223, 1),
    (0, 0, 191, 1),
    (0, 0, 159, 1),
    (0, 0, 127, 1),
    (63, 0, 91, 1),
    (127, 0, 63, 1),
    (191, 0, 31, 1),
    (255, 0, 0, 1)
]
m.add_layer(heatmap_layer)
m